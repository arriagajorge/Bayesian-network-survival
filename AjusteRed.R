library(survival)
library(bnlearn)
library(arules)
# dado que tenemos variables categoricas, procederemos a discretizar las variables
# continuas.
data(nafld)
datos = nafld1

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
# utilizaremos el modelo simple, donde todas las aristas apunten hacia la variable
# status, es decir:
redes = list()

redes[[1]] = empty.graph(names(discDatos))
arcs(redes[[1]]) = matrix(
    c("age", "status",
      "male", "status",
      "weight", "status",
      "height", "status",
      "futime", "status"),
    ncol = 2, byrow = T
)
plot(redes[[1]])

# segunda red, pero separando segun el genero
redes[[2]] = empty.graph(names(discDatos))
arcs(redes[[2]]) = matrix(
    c("age", "status",
      "height", "status",
      "weight", "status",
      "male", "weight",
      "male", "height",
      "futime", "status"),
    ncol = 2, byrow = T
)
plot(redes[[2]])
