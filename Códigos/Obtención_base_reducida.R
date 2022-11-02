#############################################
### CREACIÓN DE BASE DE DATOS A UTILIZAR ####
#############################################

setwd("/Users/kennypalma/Documents/Universidad/2022-2/LET/PROYECTO")

# Se importan los datos originales
Casen = haven::read_dta("Casen_en_Pandemia_2020_STATA_revisada2022_09.dta")

# Se seleccionan las variables a utilizar
attach(Casen)
Datos.casen = cbind(Casen$id_persona,Casen$edad, Casen$sexo, Casen$region, Casen$r8h, Casen$r8b,Casen$r8c, Casen$s2, Casen$s13, Casen$tot_per )

colnames(Datos.casen) = c("Id persona","Edad", "sexo","Region","Falta de comida por un día", "Falta de alimentos saludables", "Poca variedad en alimentos", "Estado nutricional niñe", "Sistema previsional", "total personas en la vivienda")

head(Datos.casen)

# Se guarda la nueva base en un .csv
write.csv(Datos.casen, "Casen2020_filtrados.csv")


