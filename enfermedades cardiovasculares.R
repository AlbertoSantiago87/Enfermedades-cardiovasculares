#Codigo para enfermedades cardiovasculares

library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)

#Paso 1
DB <- read_csv("~/R/Enfermedades-cardiovasculares/heart_rate_risk.csv")
View(DB)

#Paso 2
# cambiar en columna class los valores en 0 y 1

DB2 <- DB %>% mutate(
  hd = ifelse(class ==  0, 0, 1))
  
View(DB2)
  
# cambiar en columna sex los valores 0 y 1 a valores male y female
#paso 2.1

DB3 <- DB2 %>% mutate(
  sex = ifelse(sex ==  0, "Female", "Male"))

str(DB3)


# como yo realice la tabla en un archivo .csv mis datos son numericos (variables cuantitativas)
#Normalice los datos

DB4 <- DB3 %>% 
  mutate_if(is.numeric, scale)
View(DB4)


#Paso 3
#Identificando Variables RELEVANTES

#La prueba chi por su cuenta, examina si dos variables son independientes, 
#de una manera general si las distribuciones de probabilidad de dos variables difieren categóricamente.

#Chi cuadrada
chisq.test(DB3$sex, DB3$hd)
chisq.test(DB3$sex, DB3$hd, simulate.p.value = TRUE, B = 10)



# t de studen
#Recordemos que la prueba t se usa para determinar 
#si los promedios de dos grupos son significativos.

t.test(x=DB4$age, y=DB4$hd)

#Paso 4
# Exploración de asociaciones gráficamente

Db3 <- DB3%>%mutate(hd=ifelse(hd == 1, 'Disease', 'No Disease'))


#usando boxblot
ggplot(DB3, aes(age)) + 
  geom_boxplot()

# usando violin plot
ggplot(DB3)+
  aes(factor("age"), age) +
  geom_violin() +
  geom_jitter(color= 9, size = 4)


#Paso 5
# histograma d frecuencias
# opcion a
ggplot(DB3, aes(hd)) + 
  geom_bar(aes(fill(sex)))

#opcion b
ggplot(DB3, aes(hd, fill = sex)) + 
  geom_bar(fill = 'white', aes(color = sex))

ggplot(heart_data, aes(hd_text,fill = sex)) +
  geom_bar(position = 'fill')
