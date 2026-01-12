##########################
######## Pacotes #########
##########################


if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","GLMsData","gamlss")


##########################
######### Banco ##########
##########################


data("polyps")

polyps$Treatment=factor(polyps$Treatment,c("Placebo","Drug"),
                        c("Placebo","Medicamento"))

attach(polyps)


##########################
###### Descritiva ########
##########################


ggplot(polyps,aes(y=Number,x=Age))+
  geom_point()+
  labs(x="Idade",y="Número de Pólipos")+
  lims(x=c(0,50))+
  theme_bw()

ggplot(polyps,aes(y=Number,x=Treatment))+
  geom_boxplot(fill="steelblue4")+
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  )+
  labs(x="Tratamento",y="Número de Pólipos")+
  theme_bw()


##########################
######## Modelos #########
##########################


fitDist(Number, 
        k=2, # Criterio de informacao: k=2 (AIC), k=2.5 (GAIC) e k=log(n) (BIC)
        type="counts" # Tipo de dado
        )$fits 

newpar<-par(mfrow=c(1,1), # mfrow=c(2,2) para deixar os 4 graficos em conjunto
            mar=par("mar")+c(0,1,0,0),col.axis="black",
            col="black",col.lab="black", col.main="white", pch=16,
            cex=1, cex.lab=1.25, cex.axis=1)

# Modelo 1

fit1=gamlss(Number~Age+Treatment, # Definicao do submodelo de mu
            family=PO(mu.link = "log") # Distribuicao e funcao de ligacao
            );summary(fit1)

plot(fit1, par=newpar) # graficos da analise diagnostica individuais

plot(fit1)

wp(fit1) # worm plot

rqres.plot(fit1) # multiplos worm plots

# Modelo 2

fit2=gamlss(Number~Age+Treatment,
            family=GEOM(mu.link = "log")
            );summary(fit2)

plot(fit2, par=newpar)
plot(fit2)

rqres.plot(fit2)
wp(fit2)

# Modelo 3

fit3=gamlss(Number~Age+Treatment,        # submodelo de mu
            sigma.formula = ~ Treatment, # submodelo de sigma
            family=NBI(           # escolha da distribuicao
              mu.link = "log",    # funcao de ligacao do submodelo de mu
              sigma.link = "log"  # funcao de ligacao do submodelo de sigma
              )
            );summary(fit3)

plot(fit3, par=newpar)
plot(fit3)

rqres.plot(fit3)
wp(fit3)

# Calculando os p-valores da normal do teste de Wald

summary = summary(fit3)
z = summary[,"Estimate"]/summary[,"Std. Error"]
pvalor = 2 * pnorm(abs(z), lower.tail=F);pvalor
