# Paquetes 
library(readr)
library(dplyr)
library(ggplot2)
library(forcats) 
library(tidyverse) 
library(stringr)   
library(rebus) 
library(gt)

# Datos

datos_demografia <- read_csv("Datos/Korean_demographics_2000-2022.csv")

# Graficos de evolucion de las tasas

# Primer grafico evolucion tasa de naciemientos----

# generando tabla
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

#grafico
paises <-c("Seoul" = "#052F61" ,  "Busan" = "#CE6633"  , "Incheon" = "#A50E82", "Daegu" = "#6A9E1F","Daejeon" = "#E87D37" )

datos2|>
  filter(Region %in% c("Seoul" ,  "Busan"  , "Incheon", 
                       "Daegu" ,  "Daejeon" ))|>
  ggplot(aes(x = Año, y= Tasa_de_nacimientos,color = Region))+
  labs(title="Evolución de la tasa de nacimientos en regiones de Corea del Sur durante 2000 y 2022",x = NULL,
       y = "Tasa de nacimiento")+
  scale_x_continuous(breaks = seq("2000", "2022", by = 2)) +
  ylim(c(0,14)) + 
  geom_line( size = 1)+
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 0),legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = paises) 
  


# Segundo grafico evolucion tasa de fallecimientos----


# generando tabla
S3 <-datos_demografia %>% 
  filter(Region %in% "Seoul" ) %>% 
  aggregate(Tasa_de_Falleciemientos~Año, FUN = mean)

S3$Region = rep("Seoul",23)

b3 <-datos_demografia %>% 
  filter(Region %in% "Busan" ) %>% 
  aggregate(Tasa_de_Falleciemientos~Año, FUN = mean)

b3$Region = rep("Busan",23)

i3 <-datos_demografia %>% 
  filter(Region %in% "Incheon" ) %>% 
  aggregate(Tasa_de_Falleciemientos~Año , FUN = mean)

i3$Region = rep("Incheon",23)

d3 <-datos_demografia %>% 
  filter(Region %in% "Daegu" ) %>% 
  aggregate(Tasa_de_Falleciemientos~Año, FUN = mean)

d3$Region = rep("Daegu",23)

d23<-datos_demografia %>% 
  filter(Region %in% "Daejeon" ) %>% 
  aggregate(Tasa_de_Falleciemientos~Año, FUN = mean)
d23$Region = rep("Daejeon",23)

(datos3<-rbind(S3,b3,i3,d3,d23)) 
datos3<-data.frame(datos3)
View(datos3)


#grafico
paises <-c("Seoul" = "#052F61" ,  "Busan" = "#CE6633"  , "Incheon" = "#A50E82", "Daegu" = "#6A9E1F","Daejeon" = "#E87D37" )

datos3|>
  filter(Region %in% c("Seoul" ,  "Busan"  , "Incheon", 
                       "Daegu" ,  "Daejeon" ))|>
  ggplot(aes(x = Año, y= Tasa_de_Falleciemientos,color = Region))+
  labs(title="Evolución de la tasa de fallecimientos en regiones de Corea del Sur durante 2000 y 2022",x = NULL,
       y = "Tasa de fallecimientos")+
  scale_x_continuous(breaks = seq("2000", "2022", by = 2)) +
  ylim(c(0,10)) + 
  geom_line( size = 1)+
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 0),legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = paises) 


# Tercer grafico evolucion tasa de divorcios----
S4 <-datos_demografia %>% 
  filter(Region %in% "Seoul" ) %>% 
  aggregate(Tasa_de_divorcios~Año, FUN = mean)

S4$Region = rep("Seoul",23)

b4 <-datos_demografia %>% 
  filter(Region %in% "Busan" ) %>% 
  aggregate(Tasa_de_divorcios~Año, FUN = mean)

b4$Region = rep("Busan",23)

i4 <-datos_demografia %>% 
  filter(Region %in% "Incheon" ) %>% 
  aggregate(Tasa_de_divorcios~Año , FUN = mean)

i4$Region = rep("Incheon",23)

d4 <-datos_demografia %>% 
  filter(Region %in% "Daegu" ) %>% 
  aggregate(Tasa_de_divorcios~Año, FUN = mean)

d4$Region = rep("Daegu",23)

d24<-datos_demografia %>% 
  filter(Region %in% "Daejeon" ) %>% 
  aggregate(Tasa_de_divorcios~Año, FUN = mean)
d24$Region = rep("Daejeon",23)

(datos4<-rbind(S4,b4,i4,d4,d24)) 
datos4<-data.frame(datos4)
View(datos4)


#grafico
paises <-c("Seoul" = "#052F61" ,  "Busan" = "#CE6633"  , "Incheon" = "#A50E82", "Daegu" = "#6A9E1F","Daejeon" = "#E87D37" )

datos4|>
  filter(Region %in% c("Seoul" ,  "Busan"  , "Incheon", 
                       "Daegu" ,  "Daejeon" ))|>
  ggplot(aes(x = Año, y= Tasa_de_divorcios,color = Region))+
  labs(title="Evolución de la tasa de divorcios en regiones de Corea del Sur durante 2000 y 2022",x = NULL,
       y = "Tasa de divorcios")+
  scale_x_continuous(breaks = seq("2000", "2022", by = 2)) +
  ylim(c(0,5)) + 
  geom_line( size = 1)+
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 0),legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = paises) 

# Cuarto grafico evolucion tasa de casamientos----
S5 <-datos_demografia %>% 
  filter(Region %in% "Seoul" ) %>% 
  aggregate(Tasa_de_casamientos~Año, FUN = mean)

S5$Region = rep("Seoul",23)

b5 <-datos_demografia %>% 
  filter(Region %in% "Busan" ) %>% 
  aggregate(Tasa_de_casamientos~Año, FUN = mean)

b5$Region = rep("Busan",23)

i5 <-datos_demografia %>% 
  filter(Region %in% "Incheon" ) %>% 
  aggregate(Tasa_de_casamientos~Año , FUN = mean)

i5$Region = rep("Incheon",23)

d5 <-datos_demografia %>% 
  filter(Region %in% "Daegu" ) %>% 
  aggregate(Tasa_de_casamientos~Año, FUN = mean)

d5$Region = rep("Daegu",23)


d25<-datos_demografia %>% 
  filter(Region %in% "Daejeon" ) %>% 
  aggregate(Tasa_de_casamientos~Año, FUN = mean)
d25$Region = rep("Daejeon",23)

(datos5<-rbind(S5,b5,i5,d5,d25)) 
datos5<-data.frame(datos5)
View(datos5)

#grafico
paises <-c("Seoul" = "#052F61" ,  "Busan" = "#CE6633"  , "Incheon" = "#A50E82", "Daegu" = "#6A9E1F","Daejeon" = "#E87D37" )

datos5|>
  filter(Region %in% c("Seoul" ,  "Busan"  , "Incheon", 
                       "Daegu" ,  "Daejeon" ))|>
  ggplot(aes(x = Año, y= Tasa_de_casamientos,color = Region))+
  labs(title="Evolución de la tasa de casamientos en regiones de Corea del Sur durante 2000 y 2022",x = NULL,y = "Tasa de casamientos")+
  scale_x_continuous(breaks = seq("2000", "2022", by = 2)) +
  ylim(c(0,8)) + 
  geom_line( size = 1)+
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 0),legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = paises) 


# Quinto grafico evolucion de tasa de crecimiento natural-----

S6 <-datos_demografia %>% 
  filter(Region %in% "Seoul" ) %>% 
  aggregate(Tasa_de_crecimiento_natural~Año, FUN = mean)

S6$Region = rep("Seoul",23)

b6 <-datos_demografia %>% 
  filter(Region %in% "Busan" ) %>% 
  aggregate(Tasa_de_crecimiento_natural~Año, FUN = mean)

b6$Region = rep("Busan",23)

i6 <-datos_demografia %>% 
  filter(Region %in% "Incheon" ) %>% 
  aggregate(Tasa_de_crecimiento_natural~Año , FUN = mean)

i6$Region = rep("Incheon",23)

d6 <-datos_demografia %>% 
  filter(Region %in% "Daegu" ) %>% 
  aggregate(Tasa_de_crecimiento_natural~Año, FUN = mean)

d6$Region = rep("Daegu",23)

d26<-datos_demografia %>% 
  filter(Region %in% "Daejeon" ) %>% 
  aggregate(Tasa_de_crecimiento_natural~Año, FUN = mean)
d26$Region = rep("Daejeon",23)

(datos6<-rbind(S6,b6,i6,d6,d26)) 
datos6<-data.frame(datos6)
View(datos6)


#grafico
paises <-c("Seoul" = "#052F61" ,  "Busan" = "#CE6633"  , "Incheon" = "#A50E82", "Daegu" = "#6A9E1F","Daejeon" = "#E87D37" )

datos6|>
  filter(Region %in% c("Seoul" ,  "Busan"  , "Incheon", 
                       "Daegu" ,  "Daejeon" ))|>
  ggplot(aes(x = Año, y= Tasa_de_crecimiento_natural,color = Region))+
  labs(title="Evolución de la tasa de crecimiento natural en regiones de Corea del Sur durante 2000 y 2022",x = NULL,y = "Tasa de crecimiento natural")+
  scale_x_continuous(breaks = seq("2000", "2022", by = 2)) +
  ylim(c(-10,12)) + 
  geom_line( size = 1)+
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 0),legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = paises) 




# Tablas especificamente entre 2019 y 2022 (PANDEMIA)----

inicio <- "2019"
final <- "2022"

# juntar datos entre los años

  
(datosf <- cbind(datos2,datos3[2],datos4[2],datos5[2],datos6[2]))
datosf <- data.frame(datosf)

datosf%>%
  filter(Año >= inicio & Año <= final)%>%
  gt(groupname_col = "Region",rowname_col = "Año") %>%
  tab_header(title = "Tasas durante la pandemia en regiones de Corea del Sur")%>%
  #tab_row_group(label = "Region")
  fmt_number(columns = Año,decimals = 0,use_seps = FALSE)%>%
  tab_options(row_group.background.color = "#F8BBD0") %>%
  tab_spanner(label = "Tasas",columns = c("Tasa_de_nacimientos",
                                          "Tasa_de_Falleciemientos",
                                          "Tasa_de_divorcios",
                                          "Tasa_de_casamientos",
                                          "Tasa_de_crecimiento_natural"))







