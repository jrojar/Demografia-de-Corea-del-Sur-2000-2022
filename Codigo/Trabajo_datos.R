# Primera etapa de trabajo de los datos

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

datos_demografia <- read_csv("Datos/Korean_demographics_2000-2022.csv")
View(datos_demografia)

# Eliminar Sejong debid a que no tiene informacion (valores NA)

datos_demografia <- na.omit(datos_demografia)

# Formato variables

str(datos_demografia)

# Date esta e formato chr
names(datos_demografia) <- c("Fecha", "Region","Nacimientos","Tasa_de_nacimientos",
                             "Falleciemientos","Tasa_de_Falleciemientos","Divorcios",
                             "Tasa_de_divorcios","Casamientos","Tasa_de_casamientos",
                             "Crecimiento_natural", "Tasa_de_crecimiento_natural")

names(datos_demografia)
str(datos_demografia)

write_csv(datos_demografia, "Datos/Korean_demographics_2000-2022.csv")
