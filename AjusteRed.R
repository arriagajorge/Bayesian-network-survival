library(survival)
library(bnlearn)
library(arules)
# dado que tenemos variables categoricas, procederemos a discretizar las variables
# continuas.
data(nafld)
datos = nafld1

id = datos$id

# nos quedamos con las columnas deseadas, quitamos los id's y los case.id's
datos = datos[, -c(1, 7)]
# pasamos a tipo factor a las variables male y status
datos$male = as.factor(datos$male)
datos$status = as.factor(datos$status)

# imputamos valores faltantes por grupo de mujeres y de hombres
for(i in c(3,4,5)){
    # extraemos los grupos
    datosGenero0 = datos[datos$male == 0, i]
    datosGenero1 = datos[datos$male == 1, i]
    # imputamos los datos segun al grupo que pertenezcan
    #  primero lo hacemos para el grupo de mujeres 0
    datosGenero0[is.na(datosGenero0)] = median(datos[, i], na.rm = T)
    datos[datos$male == 0, i] = datosGenero0
    #  ahora para el grupo de hombres
    datosGenero1[is.na(datosGenero1)] = median(datos[, i], na.rm = T)
    datos[datos$male == 1, i] = datosGenero1
}

# ahora procederemos a discretizar las variables continuas: age, weight, height
# y bmi
discDatos = datos
attach(datos)
# cada variable la pasamos a tipo factor con 3 niveles, donde todos los niveles
# tienen el mismo numero de observaciones (o muy similares)
discDatos$age = arules::discretize(age, method = "frequency", breaks = 3)
discDatos$weight = arules::discretize(weight, method = "frequency", breaks = 3)
discDatos$height = arules::discretize(height, method = "frequency", breaks = 3)
discDatos$bmi = arules::discretize(bmi, method = "frequency", breaks = 3)

summary(discDatos)

# graficos de ayuda
plot(discDatos$age ~ discDatos$male)
# la edad en ambos grupos respecto a nuestra discretizacion, parece balanceada
# entre los grupos de hombre y mujeres

plot(discDatos$weight ~ discDatos$male)
# el peso parece cambiar respecto a ambos grupos, pues los hombres suelen tener
# mayor peso que las mujeres, este factor lo podemos considerar dentro de nuestra
# red

plot(discDatos$height ~ discDatos$male)
# aquí la diferencia es más notoria, pues los hombres tienden a ser mas altos que
# las mujeres de nuestro dataset

plot(discDatos$bmi ~ discDatos$male)
# los IMC son muy similares entre grupos.

plot(discDatos$bmi ~ discDatos$weight)


chisq.test(discDatos$age, discDatos$weight)
chisq.test(discDatos$bmi, discDatos$weight)
chisq.test(discDatos$age, discDatos$height)
# recordemos el las variables bmi y weight tienen una correlacion igual a 0.870
# y justo buscamos que las variables no estén correlacionadas (supuesto de 
# distribucion multinomial), podemos quitar una, dado que bmi aporta mas informacion
# de manera genera, podemos quitar weight de nuestro analisis. Posteriormente veremos
# si esta fue una buena eleccion

#############################################################################
#############################################################################
# primera red
#############################################################################
#############################################################################
# utilizaremos todas las variables menos bmi 
discDatos = discDatos[, -5]
summary(discDatos)
# utilizaremos el modelo simple, donde todas las aristas apunten hacia la variable
# status, es decir:
redes = list()
# no consideramos la variable futime en los datos
redes[[1]] = empty.graph(names(discDatos[, -5]))
arcs(redes[[1]]) = matrix(
    c("age", "status",
      "male", "status",
      "weight", "status",
      "height", "status"),
    ncol = 2, byrow = T
)
plot(redes[[1]])
#########################################################################
#########################################################################
#########################################################################
# ajuste de la primer red
#########################################################################
#########################################################################
#########################################################################


# haremos el ajuste por cada 365 dias
t = c(); for(x in 1:19) t[x] = x * 365
# guardaremos las 20 redes ajustadas
redesAjustadas = list()
# realizamos el ajuste de las redes

# primera red para el primer subconjunto respecto al tiempo
subcnjnto = subset(discDatos, futime <= t[1])
redesAjustadas[[1]] = bn.fit(redes[[1]], data = subcnjnto[, -5],
                             method = "bayes")
# para el ultimo subconjunto de datos
# ya casi no hay observaciones, predominan los ceros y unos
subcnjnto = subset(discDatos, futime > t[19])
redesAjustadas[[19]]  =bn.fit(redes[[1]], data = subcnjnto[, -5],
                              method = "bayes")
for(i in 2:(length(t)-1)){
    # creamos los subconjunto de datos
    subcnjnto = subset(discDatos, futime <= t[i] & futime > t[i-1])
    redesAjustadas[[i]] = bn.fit(redes[[1]], data = subcnjnto[, -5],
                                 method = "bayes")
}

# tenemos 108 probabilidades condicionales, de las cuales nos interesan las que
# tienen por valor a status = 0, pues es la probabilidad de sobrevivir al tiempo
# que se indica

# tenemos 54 modelos y 19 (redes) estimaciones para cada modelo
# por cada red hay 54 probas, de las que solo queremos las que están
# indexadas por un numero impar, es decir, las que tienen status = 0
probM = matrix(NA, nrow = length(redesAjustadas[[1]]$status$prob)/2, ncol = 19)
for(i in 1:19){
    probas = redesAjustadas[[i]]$status$prob
    pivP = c()
    for(j in 1:length(probas)){
        if(j%%2 != 0){
            pivP = c(pivP,probas[j])
        }
    }
    probM[, i] = pivP 
    
}

par(mfrow=c(2,2))
matplot(t(probM[1:5, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(1:5), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[6:10, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(6:10), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[11:15, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(11:15), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[16:20, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(16:20), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))

par(mfrow=c(2,2))
matplot(t(probM[21:25, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(21:25), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[26:30, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(26:30), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[31:35, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(31:35), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[36:40, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(36:40), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))

par(mfrow=c(2,2))
matplot(t(probM[41:45, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(41:45), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[46:50, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(46:50), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[51:54, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(50:54), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))

redesAjustadas[[1]]
# de nuestros modelos, veremos el tiempo donde la probabilidad de 
# supervivencia es menor a 0.5
# no todos los modelos presentan esta característica, por lo que será de gran
# importancia reportar los que si cumplen lo que queremos
toc = list()
for(i in 1:52){
    toc[[i]] = which(probM[i, ]<0.5)   
}

# los modelos que tienen alguna probabilidad menor a 0.5 son los siguientes
mod = c()
for(i in 1:52){
    if(length(toc[[i]]) != 0){
        mod = c(mod, i)
    }
}
mod


#########################################################################
#########################################################################
#########################################################################
# ajuste de la segunda red
#########################################################################
#########################################################################
#########################################################################

# segunda red, pero metiendo la variable del tiempo
redes[[2]] = empty.graph(names(discDatos))
arcs(redes[[2]]) = matrix(
    c("age", "status",
      "height", "status",
      "weight", "status",
      "male", "status",
      "futime","status"),
    ncol = 2, byrow = T
)
plot(redes[[2]])

# discretizamos la variable futime
t = c(); for(x in 1:19) t[x] = x * 365
# para el primer año
discDatos[which(discDatos[, 5] <= t[1]), 5] = 1
discDatos[which(discDatos[, 5] >t[19]), 5] = 19
for(i in 2:19){
    discDatos[which(discDatos[, 5]<=t[i] & discDatos[, 5]>t[i-1]), 5] = i
}
discDatos$futime = as.factor(discDatos$futime)

# para el entrenamiento de nuestra red, tenemos
red2Ajustada = bn.fit(redes[[2]], data = discDatos, method = "bayes")
class(red2Ajustada$status$prob)

probas = red2Ajustada$status$prob
length(probas)

# para un tiempo, hay 18 combinaciones posibles de nuestras probabilidades
# condicionales. Cada combinacion nos da 6 probabilidades, por lo que el objeto
# red2Ajustada$status$prob es un arreglo de 2052 probabilidades condicionales
# de las cuales solo nos interesan las que tienen por status = 0.

# para obtener las probas de solamente un tiempo, debemos ir tomando de 18*6=108
# en 108 y luego extraer las probas que tengan status = 0
ids = c(); for(i in 1:2052) ids[i] = i
porAnos = list()
obs = c(); for(i in 1:19) obs[i] = i * 108
porAnos[[1]] = probas[which(ids <= obs[1])]
for(i in 2:19){
    porAnos[[i]] = probas[which(ids <= obs[i] & ids > obs[i-1])]
}

# ya teniendo las probabilidades por años, procedemos a extraer las que tienen
# status = 0. Estas se encuentran en las posiciones impares y como ahora tenemos
# 108 probablidades por año, estas se nos van a reducir a la mitad, por lo que
# tendremos 54 probabilidades por cada año, estas van a corresponder
# a los 54 modelos que teniamos previstos
probM2 = matrix(NA, nrow = length(porAnos[[1]])/2, ncol = 19)
for(i in 1:19){
    probas = porAnos[[i]]
    pivP = c()
    for(j in 1:length(probas)){
        if(j%%2 != 0){
            pivP = c(pivP,probas[j])
        }
    }
    probM2[, i] = pivP 
    
}
par(mfrow=c(2,2))
matplot(t(probM2[1:5, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(1:5), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM2[6:10, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(6:10), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM2[11:15, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(11:15), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM2[16:20, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(16:20), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))

par(mfrow=c(2,2))
matplot(t(probM2[21:25, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(21:25), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM2[26:30, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(26:30), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM[31:35, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(31:35), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM2[36:40, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(36:40), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))

par(mfrow=c(2,2))
matplot(t(probM2[41:45, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(41:45), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM2[46:50, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(46:50), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))
matplot(t(probM2[51:54, ]), type = "l", lwd  = 2, ylab = "Proba de sobrevivir",
        xlab = "Tiempo en años", col = rainbow(5))
legend("topright", legend = as.character(50:54), col = rainbow(5),
       cex = 0.5, fill = rainbow(5))

# de nuestros modelos, veremos el tiempo donde la probabilidad de 
# supervivencia es menor a 0.5
# no todos los modelos presentan esta característica, por lo que será de gran
# importancia reportar los que si cumplen lo que queremos
toc2 = list()
for(i in 1:52){
    toc2[[i]] = which(probM2[i, ]<0.5)   
}
toc2
# los modelos que tienen alguna probabilidad menor a 0.5 son los siguientes
mod2 = c()
for(i in 1:52){
    if(length(toc2[[i]]) != 0){
        mod2 = c(mod2, i)
    }
}
mod2

########################################################################

# ajuste del modelo de riesgos proporcionales de cox
library(survminer)
summary(discDatos)
discDatos$futime = as.numeric(discDatos$futime)
discDatos$status = as.numeric(discDatos$status)
cox.fit = coxph(Surv(futime, status)~age+male+weight+height,
                data = discDatos)

summary(cox.fit)
summarycox = as.data.frame(cox.fit$coefficients)
summarycox = cbind(summarycox, exp(summarycox$`cox.fit$coefficients`))
str(cox.fit)
colnames(summarycox) = c("coef", "exp. coef")
summarycox
# para ver si el modelo de riesgos proporcionales se cumple, pero no funciona
#test.cox = cox.zph(cox.fit)
