# Paquetes 
library(readr)
library(dplyr)
library(ggplot2)
library(forcats) 

# Datos


datos_demografia <- read_csv("Datos/Korean_demographics_2000-2022.csv")



# analisis 2019/2022 - Primer grafico o visualizacion de los datos

S <-datos_demografia %>% 
  filter(Region %in% "Seoul" ) %>% 
  aggregate(Tasa_de_nacimientos~Año, FUN = mean)

S$Region = rep("Seoul",23)

b <-datos_demografia %>% 
  filter(Region %in% "Busan" ) %>% 
  aggregate(Tasa_de_nacimientos~Año, FUN = mean)

b$Region = rep("Busan",23)

i <-datos_demografia %>% 
  filter(Region %in% "Incheon" ) %>% 
  aggregate(Tasa_de_nacimientos~Año , FUN = mean)

i$Region = rep("Incheon",23)

d <-datos_demografia %>% 
  filter(Region %in% "Daegu" ) %>% 
  aggregate(Tasa_de_nacimientos~Año, FUN = mean)
  
d$Region = rep("Daegu",23)

d2<-datos_demografia %>% 
  filter(Region %in% "Daejeon" ) %>% 
  aggregate(Tasa_de_nacimientos~Año, FUN = mean)
d2$Region = rep("Daejeon",23)

(datos2<-rbind(S,b,i,d,d2)) 
datos2<-data.frame(datos2)
View(datos2)


colores_paises <- c(rep("blue",23),rep("orange",23), rep("pink",23), 
                    rep("purple",23) ,rep("yellow",23))
paises <-c("Seoul" = "blue" ,  "Busan" = "orange"  , "Incheon" = "pink", "Daegu" = "green","Daejeon" = "black" )

datos2|>
  filter(Region %in% c("Seoul" ,  "Busan"  , "Incheon", 
                       "Daegu" ,  "Daejeon" ))|>
  ggplot(aes(x = Año, y= Tasa_de_nacimientos,color = Region))+
  labs(title="Titulo")+
  scale_x_continuous(breaks = seq("2000", "2022", by = 1)) +
  scale_y_continuous(breaks = seq(5, 14, by = 1)) + 
  geom_line( size = 1)+
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 270))+
  scale_color_manual(values = paises) 
  
