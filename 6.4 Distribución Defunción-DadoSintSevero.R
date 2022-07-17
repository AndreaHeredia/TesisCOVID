# ----- INSTALAMOS PAQUETES Y LIBRERÍAS NECESARIOS -----
library(devtools)
install_github("thibautjombart/outbreaker")
install.packages("outbreaker2")
library(outbreaker2)

install.packages("EpiEstim")
library(EpiEstim)

library(readr)

install.packages(c("coarseDataTools", "stats19", "reshape2", 
                   "ggplot2", "gridExtra", "fitdistrplus", 
                   "coda", "incidence", "scales"))
#son necesarias las siguientes versiones de los paquetes: 
#coarseDataTools (>= 0.6-4), incidence (>= 1.7.0)
#lo anterior se verifica con la funciÃ³n: packageVersion("NombreDelPaquete")
library(coarseDataTools)
library(stats19)
library(stats)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(coda)
library(incidence)
library(scales)
library(grDevices)
library(graphics)

install.packages(c("MASS", "survival"))
library(fitdistrplus)



# ----- FIJAMOS EL DIRECTORIO EN EL QUE TRABAJAREMOS -----
setwd("~/Documents/1. UG/TESIS/COVID/30JUN2021/")


# ----- IMPORTAMOS EL .csv DE LA FECHA DESEADA -----
df.GTO <- read_csv("30jun2021-GTO.csv", 
                   col_types = cols(FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"),
                                    FECHA_INGRESO = col_date(format = "%Y-%m-%d"),
                                    FECHA_DEF = col_date(format = "%Y-%m-%d"),
                                    CLASIFICACION_FINAL = col_double(),
                                    ID_REGISTRO = col_skip(), MUNICIPIO_RES = col_double(),
                                    TIPO_PACIENTE = col_double(),
                                    UCI = col_logical(),
                                    INTUBADO = col_double()))
df.GTO <- subset.data.frame(df.GTO, CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3)

df.GTO <- df.GTO[order(as.Date(df.GTO$FECHA_SINTOMAS, format="%Y-%m-%d")),]
df.GTO <- subset.data.frame(df.GTO, FECHA_SINTOMAS <= "2021-06-30")

View(df.GTO)

# ----- SELECCIONAMOS LAS FECHAS DE INTERÉS PARA DEF | SÍNTOMAS SEVEROS-----
df.GTO4 <- subset.data.frame(df.GTO,
                             select = c(FECHA_INGRESO,
                                        FECHA_DEF,
                                        UCI, INTUBADO,
                                        TIPO_PACIENTE))

df.GTO4 <- df.GTO4[order(as.Date(df.GTO4$FECHA_INGRESO, format="%Y-%m-%d")),]

View(df.GTO4)


#índice de HOSP y AMBU (i.e. no-hospitalizado)
# TIPO_PACIENTE: 1 = AMBULATORIO      ----> 0 para HOSP 1 para AMBU
#                2 = HOSPITALIZADO    ----> 1 para HOSP 0 para AMBU
#               99 = NO-ESPECIFICADO  ----> 0 para HOSP 1 para AMBU
df.GTO4$IND_HOSP <- c(1:length(df.GTO4$FECHA_INGRESO))*0
df.GTO4$IND_AMBU <- c(1:length(df.GTO4$FECHA_INGRESO))*0
k=length(df.GTO4$FECHA_INGRESO)
for (i in 1:k) {
  if(df.GTO4$TIPO_PACIENTE[[i]]==2) #cuando TIPO_PACIENTE=2, ent. IND_HOSP=1
    df.GTO4$IND_HOSP[[i]] = 1
  if(df.GTO4$TIPO_PACIENTE[[i]]==1) #cuando TIPO_PACIENTE=1, ent. IND_AMBU=1
    df.GTO4$IND_AMBU[[i]] = 1
}


#creamos vector de 1's para el índice de DEF=DEFUNCIÓN
#i.e.   1 = DEF 
#       0 = NO-DEF
df.GTO4$IND_DEF <- c(1:length(df.GTO4$FECHA_INGRESO))*0 + 1
k=length(df.GTO4$FECHA_INGRESO)
for (i in 1:k) {
  if(is.na(df.GTO4$FECHA_DEF[[i]])) #cambiamos a 0's las casillas del vector anterior para aquellos donde FECHA_DEF = NA
    df.GTO4$IND_DEF[[i]] = 0
}





#Definimos SÍNTOMAS SEVEROS = (INTUB=1) + (UCI=TRUE ie. !is.na(UCI)) + (DEF=1 y AMBU=1)
#así, IND_SEVER = 1 = SÍ, 
#     IND_SEVER = 0 = NO
#iniciamos el vector de IND_SEVER  en ceros
df.GTO4$IND_SEVER <- c(1:length(df.GTO4$FECHA_INGRESO))*0
k=length(df.GTO4$FECHA_INGRESO)
for (i in 1:k) {
  if(!is.na(df.GTO4$UCI[[i]]) | df.GTO4$INTUBADO[[i]]==1)
    df.GTO4$IND_SEVER[[i]] = 1
  if(df.GTO4$IND_DEF[[i]]==1 && df.GTO4$IND_AMBU[[i]]==1) 
    df.GTO4$IND_SEVER[[i]] = 1
}



#Necesitamos separar a los SEVEROS en dos grupos HOSP.SEVER y AMBU.SEVER
#iniciamos los vectores de IND_HOSP.SEVER e IND_AMBU.SEVER en ceros
df.GTO4$IND_HOSP.SEVER <- c(1:length(df.GTO4$FECHA_INGRESO))*0
df.GTO4$IND_AMBU.SEVER <- c(1:length(df.GTO4$FECHA_INGRESO))*0
k=length(df.GTO4$FECHA_INGRESO)
for (i in 1:k) {
  if(df.GTO4$IND_HOSP[[i]]==1 && df.GTO4$IND_SEVER[[i]]==1)
    df.GTO4$IND_HOSP.SEVER[[i]] = 1
  if(df.GTO4$IND_AMBU[[i]]==1 && df.GTO4$IND_SEVER[[i]]==1) 
    df.GTO4$IND_AMBU.SEVER[[i]] = 1
}


#Ahora, queremos saber cuÃ¡ntos de los difuntos...
#pertenecen a cada categoría de HOSP.SEVER y AMBU.SEVER
#iniciamos el vector de IND_DEF.HOSP.SEVER e IND_DEF.AMBU.SEVER en ceros
df.GTO4$IND_DEF.HOSP.SEVER <- c(1:length(df.GTO4$FECHA_INGRESO))*0
df.GTO4$IND_DEF.AMBU.SEVER <- c(1:length(df.GTO4$FECHA_INGRESO))*0
k=length(df.GTO4$FECHA_INGRESO)
for (i in 1:k) {
  if(df.GTO4$IND_HOSP.SEVER[[i]]==1 && df.GTO4$IND_DEF[[i]]==1)
    df.GTO4$IND_DEF.HOSP.SEVER[[i]] = 1
  if(df.GTO4$IND_AMBU.SEVER[[i]]==1 && df.GTO4$IND_DEF[[i]]==1) 
    df.GTO4$IND_DEF.AMBU.SEVER[[i]] = 1
}



#Seleccionamos sÃ³lo los índices de interÃ©s que agruparemos por FECHA_INGRESO...
#para hacer los conteos y calcular lar probabilidades...
#IND_HOSP, IND_AMBU, IND_DEF, IND_SEVER, 
#IND_HOSP.SEVER, IND_AMBU.SEVER, IND_DEF.HOSP.SEVER, IND_DEF.AMBU.SEVER
df.GTO4aux <- subset.data.frame(df.GTO4, select = c(FECHA_INGRESO,
                                                    IND_HOSP, IND_AMBU,
                                                    IND_DEF, IND_SEVER,
                                                    IND_HOSP.SEVER, IND_AMBU.SEVER,
                                                    IND_DEF.HOSP.SEVER, IND_DEF.AMBU.SEVER))

df.GTO4aux <- aggregate(list(IND_HOSP=df.GTO4aux$IND_HOSP,
                             IND_AMBU=df.GTO4aux$IND_AMBU,
                             IND_DEF=df.GTO4aux$IND_DEF,
                             IND_SEVER=df.GTO4aux$IND_SEVER,
                             IND_HOSP.SEVER=df.GTO4aux$IND_HOSP.SEVER,
                             IND_AMBU.SEVER=df.GTO4aux$IND_AMBU.SEVER,
                             IND_DEF.HOSP.SEVER=df.GTO4aux$IND_DEF.HOSP.SEVER,
                             IND_DEF.AMBU.SEVER=df.GTO4aux$IND_DEF.AMBU.SEVER),
                        by = list(FECHA_INGRESO=df.GTO4aux$FECHA_INGRESO), sum,
                        na.rm = TRUE, na.action("na.pass"))

#ahora del anterior dataframe seleccionamos aquellos...
#...datos donde IND_HOSP!=0, IND_AMBU!=0, IND_HOSP.SEVER!=0, IND_AMBU.SEVER!=0 (i.e. eliminamos los ceros)...
#para poder calcular posteriormente las probabilidades 
#P(DEF|HOSP.SEVER)P(HOSP.SEVER), P(DEF|AMBU.SEVER)P(AMBU.SEVER)
df.GTO4aux2 <- subset.data.frame(df.GTO4aux, IND_HOSP!=0)
df.GTO4aux2 <- subset.data.frame(df.GTO4aux2, IND_AMBU!=0)

df.GTO4aux2 <- subset.data.frame(df.GTO4aux2, IND_HOSP.SEVER!=0)
df.GTO4aux2 <- subset.data.frame(df.GTO4aux2, IND_AMBU.SEVER!=0)



#Para cada día segÃºn FECHA_INGESO, calculamos POR DÍA

#P(DEF|HOSP.SEVER) = # defunciones que fueron HOSP.SEVER / # HOSP.SEVER, 
#P(HOSP.SEVER) = # HOSP.SEVER en ese día / # HOSP en ese día, 
df.GTO4aux2$Prob.DEF_HOSP.SEVER <- df.GTO4aux2$IND_DEF.HOSP.SEVER/df.GTO4aux2$IND_HOSP.SEVER
df.GTO4aux2$Prob.HOSP.SEVER <- df.GTO4aux2$IND_HOSP.SEVER/df.GTO4aux2$IND_SEVER

#y tambiÃ©n
#P(DEF|HOSP.AMBU) = # defunciones que fueron HOSP.AMBU / # HOSP.AMBU, 
#P(HOSP.AMBU) = # HOSP.AMBU en ese día / # HOSP en ese día, 
df.GTO4aux2$Prob.DEF_AMBU.SEVER <- df.GTO4aux2$IND_DEF.AMBU.SEVER/df.GTO4aux2$IND_AMBU.SEVER
df.GTO4aux2$Prob.AMBU.SEVER <- df.GTO4aux2$IND_AMBU.SEVER/df.GTO4aux2$IND_SEVER




#lo que nos interesa es calcular por día...
#P(DEF|HOSP.SEVER) P(HOSP.SEVER) + P(DEF|AMBU.SEVER) P(AMBU.SEVER) 
#por eso subdividimos el dataframe anterior
df.GTO4aux3 <- subset.data.frame(df.GTO4aux2,
                                 select = c(FECHA_INGRESO,
                                            Prob.DEF_HOSP.SEVER, Prob.HOSP.SEVER,
                                            Prob.DEF_AMBU.SEVER, Prob.AMBU.SEVER))
#calculamos por día según FECHA_INGRESO
#P(DEF|HOSP.SEVER)P(HOSP.SEVER) + P(DEF|AMBU.SEVER)P(AMBU.SEVER) 
df.GTO4aux3$Prob.DEF_SEVER <- df.GTO4aux3$Prob.DEF_HOSP.SEVER*df.GTO4aux3$Prob.HOSP.SEVER + df.GTO4aux3$Prob.DEF_AMBU.SEVER*df.GTO4aux3$Prob.AMBU.SEVER



#---- REALIZAREMOS LAS PREUBAS DE BONDAD DE AJUSTE: DEF | SÍNTOMAS SEVEROS ----
#las pruebas de bondad de ajuste se hacen con las observaciones, no con las frecuencias ...
#y para traslapar la distribución ajustada con la observada, se usan las frecuencias relativas

install.packages("EnvStats")
library(EnvStats)


install.packages('fitdistrplus')
library(fitdistrplus)



boxplot(df.GTO4aux3$Prob.DEF_SEVER)
quant4 <- quantile(df.GTO4aux3$Prob.DEF_SEVER, .98)


fit1 <- fitdist(as.double(df.GTO4aux3$Prob.DEF_SEVER), 
                distr = "gamma", method = "mme",  lower = c(0, 0))
plot(fit1)
summary(fit1)


fit2 <- fitdist(df.GTO4aux3$Prob.DEF_SEVER[df.GTO4aux3$Prob.DEF_SEVER!=0], 
                distr = "gamma", method = "mle",  
                lower = c(0, 0))
plot(fit2)
summary(fit2)


fit3 <- fitdist(as.double(df.GTO4aux3$Prob.DEF_SEVER), 
                distr = "beta", method = "mme",  lower = c(0, 0))
plot(fit3)
summary(fit3)

fit4 <- fitdist(df.GTO4aux3$Prob.DEF_SEVER[df.GTO4aux3$Prob.DEF_SEVER!=0], 
                distr = "beta", method = "mle",  
                lower = c(0, 0)) #igual eliminar los ceros
plot(fit4)
summary(fit4)



#realizamos el ajuste con un percentil del 98% con fitdist
fit5 <- fitdist(df.GTO4aux3$Prob.DEF_SEVER[df.GTO4aux3$Prob.DEF_SEVER < quant4], 
                "beta")
plot(fit5)
summary(fit5)





#---- SELECCIONAMOS EL MEJOR AJUSTE ----
fit <- fit4

data.fit <- data.frame(x = seq(from = 0, 
                               to = 1, length = 2000))
data.fit$y <- dbeta(data.fit$x,
                    shape1 = fit$estimate[[1]],
                    shape2 = fit$estimate[[2]])


fit.gamma <- fit2

data.fit.gamma <- data.frame(x = seq(from = 0, 
                                     to = 1, length = 2000))
data.fit.gamma$y <- dgamma(data.fit.gamma$x,
                           shape = fit.gamma$estimate[[1]],
                           rate = fit.gamma$estimate[[2]])




# ---- DISTRIBUCIÓN EMPÍRICA VS AJUSTADA: DEF | SÍNTOMAS SEVEROS ----

#ajuste de beta
bar = "#f27eb0" #"#69b3a2"
shape1 = fit$estimate[[1]]
shape2 = fit$estimate[[2]]


qqadj <- ggplot(df.GTO4aux3, aes(x = Prob.DEF_SEVER)) +
  geom_histogram(aes(y = (..density..)),
                 colour = bar, fill = bar, alpha = 0.5,
                 breaks = seq(0, 1, by = 1/10),
                 binwidth = 1/10) + #bins = cantidad de barras 
  geom_line(data = data.fit,
            aes(x = x, y  = y), size=0.8, color="black") +
  
  theme_ipsum() +
  
  labs(x = "proporción de fallecidos dado que son sintomáticos severos", 
       y = "densidad de frecuencias",
       title = paste0("Probabilidad de fallecer dado que es sintomático severo",
                      " \n    ajuste vs. observaciones"),
       subtitle = paste0("distribución ajustada: Beta(forma1 = ", round(shape1,4), ", forma2 = ", round(shape2,4), ")")) +
  
  scale_x_continuous(breaks = seq(0, 1, 1/10)) +
  xlim(0.3, 1.0)

qqadj


#------------------ajuste de gamma
shape = fit.gamma$estimate[[1]]
rate = fit.gamma$estimate[[2]]


qqadj.gamma <- ggplot(df.GTO4aux3, aes(x = Prob.DEF_SEVER)) +
  geom_histogram(aes(y = (..density..)),
                 colour = bar, fill = bar, alpha = 0.5,
                 breaks = seq(0, 1, by = 1/20),
                 binwidth = 1/10) + #bins = cantidad de barras 
  geom_line(data = data.fit.gamma,
            aes(x = x, y  = y), size=0.8, color="black") +
  
  theme_ipsum() +
  
  labs(x = "proporción de fallecidos dado que son sintomáticos severos", 
       y = "densidad de frecuencias",
       title = paste0("Probabilidad de fallecer dado que es sintomático severo",
                      " \n    ajuste vs. observaciones"),
       subtitle = paste0("distribución ajustada: Gamma(forma = ", round(shape,4), ", escala = ", round(1/rate,4), ")")) +
  
  scale_x_continuous(breaks = seq(0, 0.2, 1/10)) +
  xlim(0.3, 1.0)

qqadj.gamma

















#hacemos la tabla de frecuencias para despuÃ©s graficar el histograma
df.GTO4.tab <- as.data.frame(table(df.GTO4aux3$Prob.DEF_SEVER))

#transformamos a histograma lo anterior
probas <- data.frame(df.GTO4aux3$Prob.DEF_SEVER)
hist(probas$df.GTO4aux3.Prob.DEF_SEVER, breaks=10)

days4 <- seq(from = 0, to = 1, length = 2000)
y4.GTO <- dbeta(days4, 
                shape1 = fit4$estimate[[1]],
                shape2 = fit4$estimate[[2]])

data.adj4_GTO <- data.frame(days4, y4.GTO)
