options(digits = 7)
######## analisis descriptivo de datos
library(survival)
data(nafld)
datos = nafld1

head(datos)
dim(datos)
str(datos)
summary(datos)
# nos quedamos con las columnas deseadas, quitamos los id's y los case.id's
datos = datos[, -c(1, 7)]
# pasamos a tipo factor a las variables male y status
datos$male = as.factor(datos$male)
datos$status = as.factor(datos$status)
# tuplas que no tienen valores nulos 12588
dim(datos[complete.cases(datos), ])
# tenemos 17549 - 12588 =  4961 tuplas con valores nulos
# representan el 4961*100/17549 28.2% del total de tuplas

# dividimos las tuplas por la variable male e imputamos los valores faltantes
# utlizando la media muestral
library(ggplot2)
library(cowplot) # para poner graficas a lado de otras
# boxplots antes de imputar datos
box1 = ggplot(datos, aes(x=male, y=age, fill = male)) +
    geom_boxplot()
box2 = ggplot(datos, aes(x=male, y=weight, fill = male)) +
    geom_boxplot()
box3 = ggplot(datos, aes(x=male, y=height, fill = male)) +
    geom_boxplot()
box4 = ggplot(datos, aes(x=male, y=bmi, fill = male)) +
    geom_boxplot()
plot_grid(box1, box2, box3, box4)

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
# ahora ya no tenemos valores nulos
summary(datos)

# boxplots despues de imputar datos
box1I = ggplot(datos, aes(x=male, y=age, fill = male)) +
    geom_boxplot()
box2I = ggplot(datos, aes(x=male, y=weight, fill = male)) +
    geom_boxplot()
box3I = ggplot(datos, aes(x=male, y=height, fill = male)) +
    geom_boxplot()
box4I = ggplot(datos, aes(x=male, y=bmi, fill = male)) +
    geom_boxplot()
plot_grid(box1I, box2I, box3I, box4I)
# no podemos quitar outliers porque se puede dar el caso de tener
# personas muy obesas o muy altas y viceversa. Ademas para el bmi tampoco
# podemos omitir nada mas.

# graficos de dispersion 
library(GGally)
ggpairs(datos,
        columns = c(1,3,4,5,6), 
        aes(color = male, 
            alpha = 0.5))
