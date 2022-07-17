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

# ----- SELECCIONAMOS LAS FECHAS DE INTERÉS PARA FECHA ENTRE SINTOMAS Y DEFUNCIÓN-----
# En el estado de Guanajuato 

#la columna 2 corresponde a FECHA_INGRESO
df.GTO2 <- subset.data.frame(df.GTO, complete.cases(df.GTO[ , 3]),
                             select = c(FECHA_SINTOMAS, 
                                        FECHA_DEF, 
                                        MUNICIPIO_RES))

#`complete.cases(dataframe[fila,col])` es una función que elimina los NA...
#del data frame ingresado en las filas y/o columnas indicadas entre corchetes
#y regresa el data frame sin los NA en lo establecido



# ---- FRECUENCIAS DE TIEMPO ENTRE SÍNTOMAS Y DEFUNCIÓN ----
df.GTO2$TIME_DIFF_DAYS <- df.GTO2$FECHA_DEF - df.GTO2$FECHA_SINTOMAS

data.sint_def <- as.data.frame(table(df.GTO2$TIME_DIFF_DAYS))

df.GTO2 <- subset.data.frame(df.GTO2, TIME_DIFF_DAYS >= 0)


# ---- GRÁFICAS DE TIEMPO ENTRE SÍNTOMAS E INGRESO ----
# Esta es la gráfica de frecuencias absolutas
q <- ggplot() +
  geom_point(data = data.sint_def, 
             aes(x=Var1, y=Freq, group=1, color="#69b3a2")) +
  geom_line(data = data.sint_def, 
            aes(x=Var1, y=Freq, group=1, color="#69b3a2")) + 
  labs(x = "días", y = "frecuencia",
       title="Distribución del tiempo entre fechas de 
       síntomas y deceso",
       subtitle="10 mar 2020 -  30 jun 2021 ",
       color = " ") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylim(0,max(data.sint_def$Freq))

q

# Esta es la gráfica de frecuencias relativas
qrel <- ggplot() +
  geom_segment(data = data.sint_def, 
               aes(x = Var1, xend = Var1, y = 0, yend = Freq/sum(Freq), color=bar),
               color=bar) + 
  geom_point(data = data.sint_def, 
             aes(x=Var1, y=Freq/sum(Freq)),
             color=bar, fill=alpha(bar, 0.3)) +
  
  labs(x="días", y="frecuencia relativa",
       title="Distribución relativa del tiempo entre fechas 
       de síntomas y deceso",
       subtitle="01 marzo 2020 -  30 junio 2021 ",
       color = " ") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

qrel






# ---- REALIZAREMOS LAS PREUBAS DE BONDAD DE AJUSTE: SÍNTOMAS-INGRESO ----
#las pruebas de bondad de ajuste se hacen con las observaciones, no con las frecuencias ...
#y para traslapar la distribución ajustada con la observada, se usan las frecuencias relativas

install.packages("EnvStats")
library(EnvStats)


install.packages('fitdistrplus')
library(fitdistrplus)


set.seed(182)
samp2 <- sample(df.GTO2$TIME_DIFF_DAYS, 500)
samp2 <- as.double(samp2)





#realizamos distintos ajustes
fit1 <- fitdist(samp2, distr = "gamma", method = "mme",  lower = c(0, 0))
plot(fit1)
summary(fit1)
gofstat(fit1)

fit2 <- fitdist(as.double(df.GTO2$TIME_DIFF_DAYS), 
                distr = "gamma", method = "mme",  lower = c(0, 0))
plot(fit2)
summary(fit2)

fit3 <- fitdist(as.double(df.GTO2$TIME_DIFF_DAYS)[as.double(df.GTO2$TIME_DIFF_DAYS)!=0], 
                distr = "gamma", method = "mle",  
                lower = c(0, 0)) 
# el ajuste por máxima verosimilitud (mle) arroja error ...
# cuando existen observaciones igual a 0, por lo que debemos quitarlas

plot(fit3)
summary(fit3)



#---- SELECCIONAMOS EL MEJOR AJUSTE ----
fit <- fit3

data.fit <- data.frame(x = seq(from = 0, 
                               to = 300, length=2000))
data.fit$y <- dgamma(data.fit$x,
                     shape = fit$estimate[[1]],
                     rate = fit$estimate[[2]])


# ---- DISTRIBUCIÓN EMPÍRICA VS AJUSTADA: SÍNTOMAS-INGRESO ----

bar = "#a7db18" #"#69b3a2"
totalconteos = data.sint_def$Freq / sum(data.sint_def$Freq)
shape = fit$estimate[[1]]
rate = fit$estimate[[2]]


qadj <- ggplot(df.GTO2, aes(x = TIME_DIFF_DAYS)) +
  geom_histogram(aes(y = (..count..) / sum(..count..)),
                 colour = bar, fill = bar, alpha = 0.5,
                 binwidth = 1) + #bins = cantidad de barras 
  geom_line(data = data.fit,
            aes(x = x, y  = y), size=0.8, color="black") +
  
  theme_ipsum() +
  
  labs(x = "cantidad de días",
       y = "frecuencia relativa",
       title = paste0("Número de días entre inicio de síntomas y deceso",
                      " \n    ajuste vs. observaciones"),
       subtitle = paste0("distribución ajustada: Gamma(forma = ", round(shape,4), ", escala = ", round(1/rate,4), ")")) +
  
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  xlim(-1,50)

qadj

