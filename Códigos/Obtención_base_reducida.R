#############################################
### CREACIÓN DE BASE DE DATOS A UTILIZAR ####
#############################################

setwd("/Users/kennypalma/Documents/Universidad/2022-2/LET/PROYECTO/Códigos")
library(haven)

Casen = haven::read_dta("Casen_en_Pandemia_2020_STATA_revisada2022_09.dta")
attach(Casen)

Datos.casen = as.data.frame(cbind(Casen$id_persona,Casen$edad, Casen$sexo, Casen$region, Casen$r8a, Casen$r8b, 
                                  Casen$r8c,Casen$r8d,Casen$r8e, Casen$r8f, Casen$r8g, Casen$r8h, Casen$s2))


colnames(Datos.casen) = c("ID","Edad","Sexo","Región", "r8a","r8b","r8c","r8d"
                          ,"r8e","r8f","r8g","r8h","Estado_nutricional_niñe")

head(Datos.casen)
write.csv(Datos.casen, "Casen2020_filtrados.csv")

