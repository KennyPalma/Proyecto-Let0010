
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

## Estado Nutricional
table(Datos.casen$Estado_nutricional_niñe)[2]/length(na.omit(Datos.casen$Estado_nutricional_niñe))


### TOTALES POR ÍNDICE

#######################
# Su hogar se preocupó por no tener suficientes alimentos para comer 
# por falta de dinero 

Datos.casen$r8a = as.factor(Datos.casen$r8a)
levels(Datos.casen$r8a) = c("Si", "No")


AS.a = Datos.casen %>% ggplot(aes(x = r8a)) +
  labs(title = "Preocupación por falta de alimentos saludables",
       x = "", y = "", caption = "Figura 1") + 
  geom_bar(aes(fill = r8a),position = "dodge", width = 0.6) +
  geom_text(stat='count', aes(x = r8a, 
                              label = ..count..), vjust = 1.4,
            color = "white", fontface = "bold",size=6) +
  scale_fill_manual(values=c("purple4","black"))+
  guides(fill = guide_legend(title = "Respuesta"))+
  theme_bw()
AS.a

# % 
table(Datos.casen$r8a)[1]/length(Datos.casen$r8a) # 37%

#######################
# no pudo comer alimentos saludables y nutritivos por falta de dinero 
# u otros recursos

Datos.casen$r8b = as.factor(Datos.casen$r8b)
levels(Datos.casen$r8b) = c("Si", "No")


AS.b = Datos.casen %>% ggplot(aes(x = r8b)) +
  labs(title = "No pudo comer alimentos saludables y nutritivos",
       x = "", y = "", caption = "Figura 1") + 
  geom_bar(aes(fill = r8b),position = "dodge", width = 0.6) +
  geom_text(stat='count', aes(x = r8b, 
                              label = ..count..), vjust = 1.4,
            color = "white", fontface = "bold",size=6) +
  scale_fill_manual(values=c("purple4","black"))+
  guides(fill = guide_legend(title = "Respuesta"))+
  theme_bw()
AS.b

# % 
table(Datos.casen$r8b)[1]/length(Datos.casen$r8b) # 26%

#######################
# comió poca variedad de alimentos por falta de dinero u otros recursos

Datos.casen$r8c = as.factor(Datos.casen$r8c)
levels(Datos.casen$r8c) = c("Si", "No")


AS.c = Datos.casen %>% ggplot(aes(x = r8c)) +
  labs(title = "Comió poca variedad de alimento",
       x = "", y = "", caption = "Figura 1") + 
  geom_bar(aes(fill = r8c),position = "dodge", width = 0.6) +
  geom_text(stat='count', aes(x = r8c, 
                              label = ..count..), vjust = 1.4,
            color = "white", fontface = "bold",size=6) +
  scale_fill_manual(values=c("purple4","black"))+
  guides(fill = guide_legend(title = "Respuesta"))+
  theme_bw()
AS.c

# % 
table(Datos.casen$r8c)[1]/length(Datos.casen$r8c) # 29%

#######################
# tuvo que dejar de desayunar, almorzar, tomar once o cenar
# porque no había suficiente dinero u otros recursos para obtener alimentos

Datos.casen$r8d = as.factor(Datos.casen$r8d)
levels(Datos.casen$r8d) = c("Si", "No")


AS.d = Datos.casen %>% ggplot(aes(x = r8d)) +
  labs(title = "Dejar una comida",
       x = "", y = "", caption = "Figura 1") + 
  geom_bar(aes(fill = r8d),position = "dodge", width = 0.6) +
  geom_text(stat='count', aes(x = r8d, 
                              label = ..count..), vjust = 1.4,
            color = "white", fontface = "bold",size=6) +
  scale_fill_manual(values=c("purple4","black"))+
  guides(fill = guide_legend(title = "Respuesta"))+
  theme_bw()
AS.d

# % 
table(Datos.casen$r8d)[1]/length(Datos.casen$r8d) # 12%


#######################
# Comió menos de lo que pensaba que debía comer

Datos.casen$r8e = as.factor(Datos.casen$r8e)
Datos.casen$r8e[is.na(Datos.casen$r8e)] = "Blanco"
levels(Datos.casen$r8e) = c("Si", "No","Blanco")


AS.e = Datos.casen %>% ggplot(aes(x =r8e)) +
  labs(title = "Comió menos de lo que pensaba que debía comer",
       x = "", y = "", caption = "Figura 1") + 
  geom_bar(aes(fill = r8e),position = "dodge", width = 0.6) +
  geom_text(stat='count', aes(x = r8e, 
                              label = ..count..), vjust = 1.5,
            color = "white", fontface = "bold",size=4) +
  scale_fill_manual(values=c("purple4","black", "cornsilk3"))+
  guides(fill = guide_legend(title = "Respuesta"))+
  theme_bw()
AS.e

# % 
table(Datos.casen$r8e)[1]/length(Datos.casen$r8e) # 22%

#######################
# Su hogar se quedó sin alimento

Datos.casen$r8f = as.factor(Datos.casen$r8f)
Datos.casen$r8f[is.na(Datos.casen$r8f)] = "Blanco"
levels(Datos.casen$r8f) = c("Si", "No","Blanco")


AS.f = Datos.casen %>% ggplot(aes(x =r8f)) +
  labs(title = "Su hogar se quedó sin alimentos",
       x = "", y = "", caption = "Figura 1") + 
  geom_bar(aes(fill = r8f),position = "dodge", width = 0.6) +
  geom_text(stat='count', aes(x = r8f, 
                              label = ..count..), vjust = 1.5,
            color = "white", fontface = "bold",size=4) +
  scale_fill_manual(values=c("purple4","black", "cornsilk3"))+
  guides(fill = guide_legend(title = "Respuesta"))+
  theme_bw()
AS.f

# % 
table(Datos.casen$r8f)[1]/length(Datos.casen$r8f) # 11%

#######################
# Sintió hambre y no comió

Datos.casen$r8g = as.factor(Datos.casen$r8g)
levels(Datos.casen$r8g) = c("Si", "No","Blanco")
Datos.casen$r8g[is.na(Datos.casen$r8g)] = "Blanco"



AS.g = Datos.casen %>% ggplot(aes(x =r8g)) +
  labs(title = "Sintió hambre y no comió",
       x = "", y = "", caption = "Figura 1") + 
  geom_bar(aes(fill = r8g),position = "dodge", width = 0.6) +
  geom_text(stat='count', aes(x = r8g, 
                              label = ..count..), vjust = 1.5,
            color = "white", fontface = "bold",size=4) +
  scale_fill_manual(values=c("purple4","black", "cornsilk3"))+
  guides(fill = guide_legend(title = "Respuesta"))+
  theme_bw()
AS.g

# % 
table(Datos.casen$r8g)[1]/length(Datos.casen$r8g) # 10%


#######################
# Dejó de comer por todo un día

Datos.casen$r8h = as.factor(Datos.casen$r8h)
levels(Datos.casen$r8h) = c("Si", "No","Blanco")
Datos.casen$r8h[is.na(Datos.casen$r8h)] = "Blanco"



AS.h = Datos.casen %>% ggplot(aes(x =r8h)) +
  labs(title = "Dejó de comer por todo un día",
       x = "", y = "", caption = "Figura 1") + 
  geom_bar(aes(fill = r8h),position = "dodge", width = 0.6) +
  geom_text(stat='count', aes(x = r8h, 
                              label = ..count..), vjust = 1.5,
            color = "white", fontface = "bold",size=4) +
  scale_fill_manual(values=c("purple4","black", "cornsilk3"))+
  guides(fill = guide_legend(title = "Respuesta"))+
  theme_bw()
AS.h

# % 
table(Datos.casen$r8h)[1]/length(Datos.casen$r8h) # 4%


