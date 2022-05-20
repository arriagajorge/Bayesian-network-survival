library(bayess)
library(MCMCpack)
data(bank)
bank$x1=scale(bank$x1)
bank$x2=scale(bank$x2)
bank$x3=scale(bank$x3)
bank$x4=scale(bank$x4)
colnames(bank)<-c("x1","x2","x3","x4","y")
# ajustamos el modelo probit
mod <- glm(y ~., data=bank,family=binomial(link="probit"))
# graficas sobre la bondad de ajuste del modelo
plot(mod)
# notamos que una regresion desde el origen es plausible, asi como no considerar
# la variable x2, pues no es estadisticamente significativa
summary(mod)

### Usando paquete de R: LearnBayes
library(LearnBayes)

y=bank$y
x1=bank$x1[,1]
x2=bank$x2[,1]
x3=bank$x3[,1]
x4=bank$x4[,1]
# ajustamos el modelos
modLearn <- bayes.probit(y,cbind(x1,x2,x3,x4,1),1000,
                         prior=list(beta=rep(0.000000001,5),P=diag(1/10000000,5)))
summary(modLearn)
# vemos la cadena de markov generada y las densidades de nuestras variables
plot(as.mcmc(modLearn$beta))
summary(modLearn$beta)
par(mfrow = c(3, 2))
# hisotramas de las densidades de las betas
for(i in 1:5){
    hist(modLearn$beta[, i])
    abline(v = mean(modLearn$beta[, i]))
}
##################################################
### Usando paquete de R: MCMCpack
library(MCMCpack)
modMCMC <- MCMCprobit(y~x1+x2+x3+x4, burnin=1000, mcmc=1000, thin=1) 
# del resumen, tenemos valores similares para las distribuciones posteriores
# de las betas, sin embargo, los cuantiles de las variables nos dicen que 
# justo la variable x2 se puede omitir de nuestro modelo, al igual que el
# intercepto
summary(modMCMC)
# grafica de las cadenas de markov y las respectivas densidades
plot(modMCMC)

############################################################################
# modelo reducido sin considerar el intercepto
modMCMC2 <- MCMCprobit(y~ 0 + x1 +x2 + x3 + x4, burnin = 1000, mcmc = 1000, thin = 1)
summary(modMCMC2)
# quitamos x3 por tener el valor mas grande del cuantil 2.5% al cuantil 97.5%
modMCMC3 <- MCMCprobit(y ~ 0 + x1 + x2 + x4, burnin = 1000, mcmc = 1000, thin = 1)
summary(modMCMC3)

modLearn2 <- bayes.probit(y,cbind(x1,x2,x4),1000,
                         prior=list(beta=rep(0.000000001,3),P=diag(1/10000000,3)))
# este modelo es el bueno, pues todas las variables son significativas

plot(as.mcmc(modLearn2$beta))
summary(modLearn2$beta)

##################################################
### Usando rjags
library(rjags)
library(coda)

n = 200

data <- list(
    y = y ,
    x1 = x1 ,
    x2 = x2 ,
    x3 = x3 ,
    x4 = x4 ,
    n = n 
)

param <- c("Beta")


### Probit

inits <- function(){	list(
  "Beta" = rnorm(5,0,0.1)  
)	}

modelo1 = "model{
    ### Likelihood	
    for(i in 1:n){	
        y[i] ~ dbern(p[i]) 
        ###		probit(p[i]) <- eta[i]  
        p[i] <- phi(eta[i])  
        eta[i] <- Beta[5]+Beta[1]*x1[i]+Beta[2]*x2[i] + Beta[3]*x3[i] + Beta[4]*x4[i]
    }
    ### Prior
    Beta[1] ~ dnorm(0.0, 1.0E-4)
    Beta[2] ~ dnorm(0.0, 1.0E-4)
    Beta[3] ~ dnorm(0.0, 1.0E-4)
    Beta[4] ~ dnorm(0.0, 1.0E-4)
    Beta[5] ~ dnorm(0.0, 1.0E-4)
}"

# ajustamos el modelo
fit <- jags.model(textConnection(modelo1), data, inits,  n.chains=5)
# actualizamos/quemamos 6000 valores
update(fit,6000)
# extraemos una muestra de 2000 
sample <- coda.samples(fit, param, n.iter=2000, thin=1)

dev.off()
# graficamos las cadenas de markov y las densidades, todo indica que si se logra 
# la convergencia del algoritmo
plot(sample)
# estos graficos de gelman lo confirman
gelman.plot(sample)
# resumen de los datos
summary(sample)

par(mfrow=c(2,2))
traceplot(sample)


dic.samples(fit, n.iter=2000,thin=1, type="pD")

dic.samples(fit, n.iter=2000,thin=1, type="popt")

#######################################
# ahora lo haremos con el modelo reducido
param <- c("Beta")

inits <- function(){	list(
    "Beta" = rnorm(3,0,0.1)  
)	}

modelo2 = "model{
    ### Likelihood	
    for(i in 1:n){	
        y[i] ~ dbern(p[i]) 
        ###		probit(p[i]) <- eta[i]  
        p[i] <- phi(eta[i])  
        eta[i] <- Beta[1]*x1[i] + Beta[2]*x2[i] + Beta[3]*x4[i]
    }
    ### Prior
    Beta[1] ~ dnorm(0.0, 1.0E-4)
    Beta[2] ~ dnorm(0.0, 1.0E-4)
    Beta[3] ~ dnorm(0.0, 1.0E-4)
}"
# ajustamos el modelo
fit2 <- jags.model(textConnection(modelo2), data, inits,  n.chains=5)
# actualizamos/quemamos 6000 valores
update(fit2,6000)
# extraemos una muestra de 2000 
sample <- coda.samples(fit2, param, n.iter=2000, thin=1)

dev.off()
# graficamos las cadenas de markov y las densidades, todo indica que si se logra 
# la convergencia del algoritmo
plot(sample)
# estos graficos de gelman lo confirman
gelman.plot(sample)

# calculamos los cuantiles
summary(sample)
#######################################
# con variables latentes
# se hace un procedimiento analogo al anterior, solo considerando la variable z
param2 <- c("Beta")

initsA <- function(){	list(
    "Beta" = rnorm(5,0,0.1) , 
    "z" = rep(0,n) 
)	}

modelo1aug = "model{		
### Likelihood
	for(i in 1:n){	
		y[i] ~ dbern(p[i]) 
		p[i] <- step(z[i])*0.999999999
		z[i] ~ dnorm(eta[i],5)  
		eta[i] <- Beta[5] + Beta[1]*x1[i] + Beta[2]*x2[i] + Beta[3]*x3[i] + Beta[4]*x4[i]
	}
### Prior
	Beta[1] ~ dnorm(0.0,1.0E-4)
	Beta[2] ~ dnorm(0.0,1.0E-4)
	Beta[3] ~ dnorm(0.0,1.0E-4)
	Beta[4] ~ dnorm(0.0,1.0E-4)
	Beta[5] ~ dnorm(0.0,1.0E-4)
}"

fitA <- jags.model(textConnection(modelo1aug), data, initsA,  n.chains=5)

update(fitA,6000)

sampleA <- coda.samples(fitA, param2, n.iter=2000, thin=1)

plot(sampleA)
summary(sampleA)

par(mfrow=c(2,2))
traceplot(sampleA)

gelman.plot(sampleA)

###############################################
# modelo reducido 
initsA <- function(){	list(
    "Beta" = rnorm(3,0,0.1) , 
    "z" = rep(0,n) 
)	}

modelo1aug = "model{		
### Likelihood
	for(i in 1:n){	
		y[i] ~ dbern(p[i]) 
		p[i] <- step(z[i])*0.999999999
		z[i] ~ dnorm(eta[i],3)  
		eta[i] <- Beta[1]*x1[i] + Beta[2]*x2[i] + Beta[3]*x4[i]
	}
### Prior
	Beta[1] ~ dnorm(0.0,1.0E-4)
	Beta[2] ~ dnorm(0.0,1.0E-4)
	Beta[3] ~ dnorm(0.0,1.0E-4)
}"

fitA <- jags.model(textConnection(modelo1aug), data, initsA,  n.chains=5)

update(fitA,6000)

sampleA <- coda.samples(fitA, param2, n.iter=2000, thin=1)

plot(sampleA)
