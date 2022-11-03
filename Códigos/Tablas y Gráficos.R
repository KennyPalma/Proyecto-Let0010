##################################
##### VISUALIZACIÓN DE DATOS #####
##################################

library(dplyr)
library(ggplot2)
library(ggtext)
library(gghighlight)

setwd("/Users/kennypalma/Documents/Universidad/2022-2/LET/PROYECTO/Códigos")
Casen = read.csv("Casen2020_filtrados.csv")
attach(Casen)

# Eliminamos NA en la variable de interés
datos = Casen[!is.na(Casen$Estado.nutricional.niñe),]

# Renombramos los valores según la codificación
datos$Estado.nutricional.niñe = as.factor(datos$Estado.nutricional.niñe)
levels(datos$Estado.nutricional.niñe) = c("Desnutrido","Normal", "Sobrepeso", "Obeso",
                                          "No sabe")


####### FIGURA 1
### CANTIDAD DE NIÑES POR ESTADO NUTRICIONAL
EN = datos %>% ggplot(aes(x = Estado.nutricional.niñe)) +
  labs(title = "Cantidad de niñas y niños según estado nutricional",
       x = "", y = "") + 
  geom_text(stat='count', aes(x = Estado.nutricional.niñe, 
                              label = ..count..), vjust = -0.15,
                               color = "#B22222", fontface = "bold") +
  geom_bar(fill = "lightskyblue4") +
  theme_bw()

# Dispositivo PNG
png("Estado_nutricional_conteo.png")
EN
dev.off()
############################3
####### FIGURA 2
datos$Falta.de.alimentos.saludables = as.factor(datos$Falta.de.alimentos.saludables)
levels(datos$Falta.de.alimentos.saludables) = c("Si", "No")
tab.alimentos = table(datos$Estado.nutricional.niñe,
      datos$Falta.de.alimentos.saludables)
knitr::kable(tab.alimentos)

####### FIGURA 3
### CANTIDAD DE NIÑES POR ESTADO NUTRICIONAL
datos$sexo = as.factor(datos$sexo)
levels(datos$sexo) = c("Hombre", "Mujer")
EN.sexo = datos %>% ggplot(aes(x = Estado.nutricional.niñe)) +
  labs(title = "Cantidad de niñas y niños según estado nutricional",
       subtitle = "División por sexo",
       x = "", y = "") + 
  geom_bar(aes(fill = sexo),position = "dodge") +
  scale_fill_manual(values=c("black","purple4"))+
  theme_bw()
EN.sexo

# Dispositivo PNG
png("Estado_nutricional_sexo.png")
EN.sexo
dev.off()
