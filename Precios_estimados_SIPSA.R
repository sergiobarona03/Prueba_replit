#############################
## Ajuste de precios SIPSA ##
#############################

install.packages("readxl")
library(readxl)
library(dplyr)
library(stringr)
library(dplyr)

#Precios SIPSA ajustados

Precios_sipsa = read_excel("/Users/sergiobarona03/Desktop/Precios_estimados_SIPSA.xlsx",
                           sheet = 1)
colnames(Precios_sipsa) = c("Fecha", "Grupo", "Producto", 
                            "Mercado", "Precio_promedio_kg")

Precios_sipsa = filter(Precios_sipsa, Mercado %in% c("Cali, Cavasa", "Cali, Galería Alameda",
                                     "Cali, La Floresta", "Cali, Santa Elena",
                                     "Cali, Siloé"))

Precios_sipsa$Grupo = as.factor(Precios_sipsa$Grupo)
levels(Precios_sipsa$Grupo)
levels(Precios_sipsa$Grupo)[1] = Factor_sipsa[1,2] #Carnes
levels(Precios_sipsa$Grupo)[2] = Factor_sipsa[2,2] #Frutas
levels(Precios_sipsa$Grupo)[3] = Factor_sipsa[3,2] #Granos y cereales
levels(Precios_sipsa$Grupo)[4] = Factor_sipsa[5,2] #Lácteos y huevos
levels(Precios_sipsa$Grupo)[5] = Factor_sipsa[6,2] #Pescado
levels(Precios_sipsa$Grupo)[6] = Factor_sipsa[7,2] #Procesados
levels(Precios_sipsa$Grupo)[7] = Factor_sipsa[4,2] #Verduras y hortalizas; Tubérculos y raíces
levels(Precios_sipsa$Grupo)[8] = Factor_sipsa[4,2] #Verduras y hortalizas; Tubérculos y raíces
Precios_sipsa$Grupo = as.character(Precios_sipsa$Grupo)
Precios_sipsa$Cod_SIPSA = NA

for (k in 1:length(Precios_sipsa$Grupo)) {
  print(k)
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[1,2]) {
    Precios_sipsa$Cod_SIPSA[k] = 1
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[2,2]) {
    Precios_sipsa$Cod_SIPSA[k] = 2
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[3,2]) {
    Precios_sipsa$Cod_SIPSA[k] = 3
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[4,2]) {
    Precios_sipsa$Cod_SIPSA[k] = 4
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[5,2]) {
    Precios_sipsa$Cod_SIPSA[k] = 5
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[6,2]) {
    Precios_sipsa$Cod_SIPSA[k] = 6
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[7,2]) {
    Precios_sipsa$Cod_SIPSA[k] = 7
  }
}

Precios_sipsa$Precio_estimado_kg = NA

for (k in 1:length(Precios_sipsa$Grupo)) {
  print(k)
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[1,2]) {
    Precios_sipsa$Precio_estimado_kg[k] = Precios_sipsa$Precio_promedio_kg[k]*Factor_sipsa$margen[1]
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[2,2]) {
    Precios_sipsa$Precio_estimado_kg[k] = Precios_sipsa$Precio_promedio_kg[k]*Factor_sipsa$margen[2]
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[3,2]) {
    Precios_sipsa$Precio_estimado_kg[k] = Precios_sipsa$Precio_promedio_kg[k]*Factor_sipsa$margen[3]
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[4,2]) {
    Precios_sipsa$Precio_estimado_kg[k] = Precios_sipsa$Precio_promedio_kg[k]*Factor_sipsa$margen[4]
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[5,2]) {
    Precios_sipsa$Precio_estimado_kg[k] = Precios_sipsa$Precio_promedio_kg[k]*Factor_sipsa$margen[5]
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[6,2]) {
    Precios_sipsa$Precio_estimado_kg[k] = Precios_sipsa$Precio_promedio_kg[k]*Factor_sipsa$margen[6]
  }
  if (Precios_sipsa$Grupo[k] == Factor_sipsa[7,2]) {
    Precios_sipsa$Precio_estimado_kg[k] = Precios_sipsa$Precio_promedio_kg[k]*Factor_sipsa$margen[7]
  }
}

Precios_sipsa$x = c(1:length(Precios_sipsa$Fecha))

plot(Precios_sipsa$x, Precios_sipsa$Precio_promedio_kg, 
     type = "l", col = "green")
lines(Precios_sipsa$x, Precios_sipsa$Precio_estimado_kg,
      type = "l", col = "red")

#Precios por grupos

#Carnes
Precios_sipsa_carnes = Precios_sipsa[Precios_sipsa$Cod_SIPSA==1,]
plot(Precios_sipsa_carnes$x, Precios_sipsa_carnes$Precio_promedio_kg, 
     type = "l", col = "green")
lines(Precios_sipsa_carnes$x, Precios_sipsa_carnes$Precio_estimado_kg,
      type = "l", col = "red")
  
#Frutas
Precios_sipsa_frutas = Precios_sipsa[Precios_sipsa$Cod_SIPSA==2,]
plot(Precios_sipsa_frutas$x, Precios_sipsa_frutas$Precio_promedio_kg, 
     type = "l", col = "green")
lines(Precios_sipsa_frutas$x, Precios_sipsa_frutas$Precio_estimado_kg,
      type = "l", col = "red")


#Granos y cereales
Precios_sipsa_cereales = Precios_sipsa[Precios_sipsa$Cod_SIPSA==3,]
plot(Precios_sipsa_cereales$x, Precios_sipsa_cereales$Precio_promedio_kg, 
     type = "l", col = "green", ylim = c(0, 10000))
lines(Precios_sipsa_cereales$x, Precios_sipsa_cereales$Precio_estimado_kg,
      type = "l", col = "red")

#Verduras y hortalizas
Precios_sipsa_verduras = Precios_sipsa[Precios_sipsa$Cod_SIPSA==4,]
plot(Precios_sipsa_verduras$x, Precios_sipsa_verduras$Precio_promedio_kg, 
     type = "l", col = "green", ylim = c(0, 10000))
lines(Precios_sipsa_verduras$x, Precios_sipsa_verduras$Precio_estimado_kg,
      type = "l", col = "red")

#Lácteos y huevos
Precios_sipsa_lacteos = Precios_sipsa[Precios_sipsa$Cod_SIPSA==5,]
plot(Precios_sipsa_lacteos$x, Precios_sipsa_lacteos$Precio_promedio_kg, 
     type = "l", col = "green", ylim = c(0, 100000))
lines(Precios_sipsa_lacteos$x, Precios_sipsa_lacteos$Precio_estimado_kg,
      type = "l", col = "red")

#Pescado
Precios_sipsa_pescado = Precios_sipsa[Precios_sipsa$Cod_SIPSA==6,]
plot(Precios_sipsa_pescado$x, Precios_sipsa_pescado$Precio_promedio_kg, 
     type = "l", col = "green", ylim = c(0, 100000))
lines(Precios_sipsa_pescado$x, Precios_sipsa_pescado$Precio_estimado_kg,
      type = "l", col = "red")

#Procesados
Precios_sipsa_procesados = Precios_sipsa[Precios_sipsa$Cod_SIPSA==7,]
plot(Precios_sipsa_procesados$x, Precios_sipsa_procesados$Precio_promedio_kg, 
     type = "l", col = "green", ylim = c(0, 150000))
lines(Precios_sipsa_procesados$x, Precios_sipsa_procesados$Precio_estimado_kg,
      type = "l", col = "red")
  
  
  




