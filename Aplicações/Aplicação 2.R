##########################
######## Pacotes #########
##########################


if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","GLMsData","gamlss")


##########################
######### Banco ##########
##########################


data(hcrabs)

hcrabs$Wt=hcrabs$Wt/1000

hcrabs$Col=factor(hcrabs$Col,rev(c("D","DM","M","LM")),
                  rev(c("Escura","Média Escura","Média","Média Clara")))

attach(hcrabs)


##########################
###### Descritiva ########
##########################


ggplot(hcrabs,aes(y=Sat,x=Wt))+
  geom_point()+
  labs(x="Peso (kg)",y="Número de satélites")+
  theme_bw()

ggplot(hcrabs,aes(y=Sat,x=Col))+
  geom_boxplot(fill="steelblue4")+
  labs(x="Cor da carapaça",y="Número de satélites")+
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  )+
  theme_bw()


##########################
######## Modelos #########
##########################


fitDist(Sat, 
        k=2, # Criterio de informacao: k=2 (AIC), k=2.5 (GAIC) e k=log(n) (BIC)
        type="counts" # Tipo de dado
)$fits 

newpar<-par(mfrow=c(1,1), # mfrow=c(2,2) para deixar os 4 graficos em conjunto
            mar=par("mar")+c(0,1,0,0),col.axis="black",
            col="black",col.lab="black", col.main="white", pch=16,
            cex=1, cex.lab=1.25, cex.axis=1)

# Modelo 1

fit1=gamlss(Sat~Wt+Col,family=PO(mu.link = "log"));summary(fit1)

plot(fit1,par=newpar)
plot(fit1)

wp(fit1, ylim.all=F)

# Modelo 2

fit2=gamlss(Sat~Wt+Col,sigma.formula = ~ Wt,
            family=ZIP(mu.link = "log", sigma.link = "logit"),
            data=hcrabs[-c(15,56,149),]);summary(fit2)

plot(fit2,par=newpar)
plot(fit2)

rqres.plot(fit2)
wp(fit2)


# Modelo 3

fit3=gamlss(Sat~Wt + Col,sigma.formula = ~ Wt + Col,
            family=NBI(mu.link = "log", sigma.link = "log"));summary(fit3)

plot(fit3,par=newpar)
plot(fit3)

rqres.plot(fit3)
wp(fit3)

# Modelo 4

fit4=gamlss(Sat~Wt+Col,sigma.formula = ~ 1, nu.formula = ~ Wt,
            family=ZINBI(sigma.link = "sqrt"));summary(fit4)

plot(fit4,par=newpar)
plot(fit4)

rqres.plot(fit4)
wp(fit4)

#hcrabs$Col <- relevel(hcrabs$Col, ref = "Escura")

# Modelo 5

fit5=gamlss(Sat~Wt + Col,sigma.formula = ~ Col,
            nu.formula = ~ 1,tau.formula = ~ Col,#data=hcrabs[-c(15,56,149),],
            family=ZISICHEL(tau.link = "logit"),data=hcrabs);summary(fit5)

plot(fit5,par=newpar)
plot(fit5)

wp(fit5)


# Calculando os p-valores da normal do teste de Wald

summary = summary(fit5)
z = summary[,"Estimate"]/summary[,"Std. Error"]
pvalor = 2 * pnorm(abs(z), lower.tail=F);pvalor