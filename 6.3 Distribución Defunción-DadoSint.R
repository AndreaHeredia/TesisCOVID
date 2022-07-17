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
setwd("~/Documents/1. UG/TESIS/COVID/30JUN2021")


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


# ---- SELECCIONAMOS DATOS PARA PROBABILIDAD DE MUERTE DADO SÍNTOMAS Y GRAFICAMOS ----
install.packages("data.table")
library(data.table)


df.GTO3 <- subset.data.frame(df.GTO,
                             select = c(FECHA_SINTOMAS,
                                        FECHA_DEF,
                                        MUNICIPIO_RES))

#iniciamos el índice de defunción (IND_DEF) en 1
df.GTO3$IND_DEF <- c(1:length(df.GTO3$FECHA_SINTOMAS))*0 + 1 

# cambiamos a 0 el IND_DEF si la fecha de def es NA
k=length(df.GTO3$FECHA_SINTOMAS)
for (i in 1:k) {
  if(is.na(df.GTO3$FECHA_DEF[[i]]))
    df.GTO3$IND_DEF[[i]] = 0
}

#creamos un df auxiliar para contar la proporción de defunciones entre síntomas
df.GTO3aux <- subset.data.frame(df.GTO3,
                                select = c(FECHA_SINTOMAS,
                                           IND_DEF))

#contamos las defunciones por fecha de síntomas 
df.GTO3aux <- aggregate(df.GTO3aux$"IND_DEF",
                        by = list(df.GTO3aux$"FECHA_SINTOMAS"), sum,
                        na.rm = TRUE, na.action("na.pass"))

#contamos los sintomáticos por fecha de síntomas
df.GTO3aux$IND_SINT <- as.data.frame(table(df.GTO3$FECHA_SINTOMAS))$Freq

names(df.GTO3aux)[names(df.GTO3aux) == "Group.1"] <- "FECHA_SINTOMAS"
names(df.GTO3aux)[names(df.GTO3aux) == "x"] <- "IND_DEF"

# calculamos la proba de DEF | SÍNTOMAS dividiendo IND_DEF entre IND_SINT
df.GTO3aux$Prob <- df.GTO3aux$IND_DEF/df.GTO3aux$IND_SINT





#---- REALIZAREMOS LAS PREUBAS DE BONDAD DE AJUSTE: DEF | SÍNTOMAS ----
  #las pruebas de bondad de ajuste se hacen con las observaciones, no con las frecuencias ...
  #y para traslapar la distribución ajustada con la observada, se usan las frecuencias relativas

install.packages("EnvStats")
library(EnvStats)


install.packages('fitdistrplus')
library(fitdistrplus)


gofaux <- gofTest(df.GTO3aux$Prob, distribution="gamma")
plot(gofaux)





fit1 <- fitdist(as.double(df.GTO3aux$Prob), 
                distr = "gamma", method = "mme",  lower = c(0, 0))
plot(fit1)
summary(fit1)

fit2 <- fitdist(df.GTO3aux$Prob[df.GTO3aux$Prob!=0], 
                distr = "gamma", method = "mle",  
                lower = c(0, 0)) 
# el ajuste por máxima verosimilitud (mle) arroja error ...
# cuando existen observaciones igual a 0, por lo que debemos quitarlas

plot(fit2)
summary(fit2)


fit3 <- fitdist(as.double(df.GTO3aux$Prob), 
                distr = "beta", method = "mme",  lower = c(0, 0))
plot(fit3)
summary(fit3)

fit4 <- fitdist(df.GTO3aux$Prob[df.GTO3aux$Prob!=0], 
                distr = "beta", method = "mle",  
                lower = c(0, 0)) #igual eliminar los ceros
plot(fit4)
summary(fit4)



#---- SELECCIONAMOS EL MEJOR AJUSTE ----
fit <- fit4

data.fit <- data.frame(x = seq(from = 0, 
                               to = 1, length=2000))
data.fit$y <- dbeta(data.fit$x,
                     shape1 = fit$estimate[[1]],
                     shape2 = fit$estimate[[2]])


fit.gamma <- fit2

data.fit.gamma <- data.frame(x = seq(from = 0, 
                               to = 1, length=2000))
data.fit.gamma$y <- dgamma(data.fit.gamma$x,
                    shape = fit.gamma$estimate[[1]],
                    rate = fit.gamma$estimate[[2]])



# ---- DISTRIBUCIÓN EMPÍRICA VS AJUSTADA: DEF | SÍNTOMAS ----

#ajuste de beta
bar = "#f7b94d" #"#69b3a2"
shape1 = fit$estimate[[1]]
shape2 = fit$estimate[[2]]


ppadj <- ggplot(df.GTO3aux, aes(x = Prob)) +
  geom_histogram(aes(y = (..density..)),
                 colour = bar, fill = bar, alpha = 0.5,
                 breaks = seq(0, 1, by = 1/30),
                 binwidth = 1/30) + #bins = cantidad de barras 
  geom_line(data = data.fit,
            aes(x = x, y  = y), size=0.8, color="black") +
  
  theme_ipsum() +
  
  labs(x = "proporción de fallecidos dado que son sintomáticos", 
       y = "densidad de frecuencias",
       title = paste0("Probabilidad de fallecer dado que es sintomático",
                      " \n    ajuste vs. observaciones"),
       subtitle = paste0("distribución ajustada: Beta(forma1 = ", round(shape1,4), ", forma2 = ", round(shape2,4), ")")) +
  
  scale_x_continuous(breaks = seq(0, 0.42, 1/30)) +
  xlim(0, 0.4)

ppadj


#ajuste de gamma
bar = "#f7b94d" #"#69b3a2"
shape = fit.gamma$estimate[[1]]
rate = fit.gamma$estimate[[2]]


ppadj.gamma <- ggplot(df.GTO3aux, aes(x = Prob)) +
  geom_histogram(aes(y = (..density..)),
                 colour = bar, fill = bar, alpha = 0.5,
                 breaks = seq(0, 1, by = 1/30),
                 binwidth = 1/30) + #bins = cantidad de barras 
  geom_line(data = data.fit.gamma,
            aes(x = x, y  = y), size=0.8, color="black") +
  
  theme_ipsum() +
  
  labs(x = "proporción de fallecidos dado que son sintomáticos", 
       y = "densidad de frecuencias",
       title = paste0("Probabilidad de fallecer dado que es sintomático",
                      " \n    ajuste vs. observaciones"),
       subtitle = paste0("distribución ajustada: Gamma(forma = ", round(shape,4), ", escala = ", round(1/rate,4), ")")) +
  
  scale_x_continuous(breaks = seq(0, 0.42, 1/30)) +
  xlim(0,0.4)

ppadj.gamma




