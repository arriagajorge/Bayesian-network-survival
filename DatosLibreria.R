library(survival)

# cargamos los datos
data(nafld)

# solo usaremos nafld1
write.csv(nafld1, file = "dataset.csv")

head(nafld1)
