#
#Trabajo módulo n°3 Data Science 
#Nombre: Cristian Riquelme Fernández

#Cargando archivo y librería para análisis descriptivo

dat=read.csv(file.choose(),sep=",",dec=".",header=T)

library(agricolae)

#Revizando la cantidad de variables

names(dat)

#Revizando tipo de variables

str(dat)

#Cambiando tipo de variable "character" por "factor", para que los lea 
#de forma cualitativa nominal 

dat$marital = as.factor(dat$marital)
dat$job = as.factor(dat$job)
dat$marital = as.factor(dat$marital)
dat$education = as.factor(dat$education)
dat$default = as.factor(dat$default)
dat$housing = as.factor(dat$housing)
dat$loan = as.factor(dat$loan)
dat$contact = as.factor(dat$contact)
dat$month = as.factor(dat$month)
dat$day_of_week = as.factor (dat$day_of_week)
dat$poutcome = as.factor (dat$poutcome)
dat$y = as.factor(dat$y)

#Sumario a la data

summary(dat)


#Caracterización de los clientes

#Edad

summary(dat$age)
sd(dat$age, na.rm = TRUE)
boxplot(dat$age, col="brown1", main="Edad ")  
abline(h= mean(dat$age), col="blue")

HistAge= hist(dat$age, main="Histograma Edad", col="red", xlab="", ylab="")
tabla = table.freq(HistAge)
tabla

#Estado civil

summary(dat$marital)
table(dat$marital)/32950
labels = c("Divorciado", "Casado", "Soltero", "Sin Informar")
porcentajes = c(0.111*100, 0.605*100, 0.280*100, 0.001*100)
tags =paste(labels, porcentajes, "%", sep = " ")
pie(porcentajes, labels  =tags, main = "Porcentaje Estado Civil", radius = 1)


#Nivel educacional

educ_barr= table(dat$education)
educ_barr
porce =prop.table(educ_barr)*100
educ_bar =barplot(porce, las=2, main = "Nivel educacional", col= rainbow(9))
text(educ_bar, c(8), round(porce,2))

#Clientes con algún prestamo para vivienda

hous_bar= table(dat$housing)
hous_bar

porce =prop.table(hous_bar)*100
hous_barr =barplot(porce, las=2, main = "Préstamo para vivienda", col= rainbow(9))
text(hous_barr, c(8), round(porce,1))

#Clientes con algún préstamo personal

loan_bar= table(dat$loan)
loan_bar

porce =prop.table(loan_bar)*100
loan_barr =barplot(porce, las=2, main = "Préstamo personal", col= rainbow(9))
text(loan_barr, c(8), round(porce,1))


#Datos de la campaña

#Cantidad de contactos durante la campaña, 
#con histograma para ver tabla de frecuencia.

summary(dat$campaign)
Histcon= hist(dat$campaign, main="Histograma contactos", col="red", xlab="", ylab="")
tabla = table.freq(Histcon)
tabla


#Medio de contacto

contact_pie= table(dat$contact)
contact_pie
table(dat$contact)/32950
labels = c("Celular", "Teléfono")
porcentajes = c(0.634*100, 0.365*100)
tags =paste(labels, porcentajes, "%", sep = " ")
pie(porcentajes, labels  =tags, main = "Medio de contacto", radius = 1)

#Mes del último contacto 

summary(dat$month)

#Día del último contacto

summary(dat$day_of_week)

#Duración de la llamada en segundos,
#con histograma para ver tabla de frecuencia.

summary(dat$duration)
Histdurt= hist(dat$duration, main="Histograma duración", col="red", xlab="", ylab="")
tabla = table.freq(Histdurt)
tabla

#Resultado de la campaña de marketing anterior

summary(dat$poutcome)

#¿El cliente ha suscrito un depósito a plazo? ('sí No')
dep_pla= table(dat$y)
dep_pla
porce =prop.table(dep_pla)*100
dep_bar =barplot(porce, main = "Depósito a plazo", col=  "aquamarine2")
text(dep_bar, c(5,5), round(porce,1))


#Se generan bases de datos separadas para breve análisis estratificado 

dat_depyes= dat[dat$y=="yes",]
dat_depno= dat[dat$y=="no",]
summary(dat_depyes)
summary(dat_depno)

#Edad por estrato

summary(dat_depyes$age)
summary(dat_depno$age)

#Estado civil por estrato

summary(dat_depyes$marital)
summary(dat_depno$marital)

#Nivel educacional por estrato

summary(dat_depyes$education)
summary(dat_depno$education)

#Tipo de trabajo por estrato

summary(dat_depyes$job)
summary(dat_depno$job)

#Duración de llamadas en segundos por estrato

summary(dat_depyes$duration)
summary(dat_depno$duration)

#conclusiones
