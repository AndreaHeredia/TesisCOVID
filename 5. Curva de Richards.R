#-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-
#        AJUSTE DE LA CURVA DE RICHARDS POR MÍNIMOS CUADRADOS
#_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._
#- Queremos ajustar, a los datos que tenemos de COVID, los parámetros del modelo
#  I = K / ( 1 + e^(-r(t - tm)) )^(1/a),
# que es la curva o modelo de Richards
#- Importamos los datos del último día de las bases de datos COVID Gto.
#- Contamos no. de infectados y no. acumulado de infectados.
#- Usamos la función nls para ajustar por mínimos cuadrados el modelo Richards.
#- Graficamos los datos reales vs. la predicción.
#_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._


# ----- LIBRERÍAS Y PAQUETES -----
#lubridate, chron, datetime, data.table, readr
library(readr)
library(lubridate)
library(chron)
library(datetime)
library(data.table)

# para entender/observar la incidencia
install.packages("incidence")
library(incidence)

# para hacer gráficas bonitas
install.packages("ggplot2")
install.packages("dplyr")
install.packages("hrbrthemes")

library(ggplot2)
library(dplyr)
library(hrbrthemes)





# ----- FIJAMOS EL DIRECTORIO E IMPORTAMOS LOS DATOS -----
# Directorio en donde queremos guardar las gráficas
setwd("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021")

# Importamos la base de datos en un data.frame
X18_GTO <- read_csv("0. GTO/18-GTO.csv", 
                    col_types = cols(FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"), 
                                     FECHA_INGRESO = col_date(format = "%Y-%m-%d"), 
                                     FECHA_DEF = col_date(format = "%Y-%m-%d"), 
                                     ID_REGISTRO = col_skip(), RESULTADO_LAB = col_skip(), 
                                     RESULTADO_ANTIGENO = col_skip()))


# Seleccionamos los datos hasta la fecha de interés
# los casos positivos corresponden a CLASIFICACION_FINAL=1,2,3
df.24ago <- subset.data.frame(X18_GTO, FECHA_SINTOMAS<="2020-08-24")
df.24ago <- subset.data.frame(df.24ago, CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3 )
# para el 31 de mayo debemos obtener 2682 obs. de 11 variables ...
# y para el 24 de agosto 32814 obs. de 11 variables

#----- GRAFICAMOS INCIDENCIA EN FORMA DE LOLIPOPOP GRAPH -----

incid <- subset.data.frame(X18_GTO, CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3 )
incid <- incidence(incid$FECHA_SINTOMAS)

df.total <- data.frame(day = incid$dates,
                       counts = incid$counts,
                       cumulative = cumulate(incid$counts))

tot <- ggplot(df.total, aes(x = day, y = counts)) +
  geom_segment( aes(x = day, xend = day, y = 0, yend = counts),
                size = 0.45, color = "#69b3a2", alpha = 0.7) +
  geom_point(size = 2, color = "#69b3a2", pch = 18) +
  labs(x = "fechas",
       y = "incidencia",
       title = "Incidencia",
       subtitle = "en el estado de Guanajuato",
       caption = paste0("observaciones hasta el 19 de junio de 2021",
                        "\nTotal casos: ", incid$n,
                        "\nIncidencia máxima: ", max(incid$counts))) +
  theme_ipsum() +
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank()) +
  
  scale_x_date(limit = c(as.Date(min(incid$dates)), "2021-07-24"),
               date_breaks = "14 days",
               date_labels = "%b%d") +
  ylim(0, max(incid$counts))
  

tot


acum <- ggplot(df.total, aes(x = day, y = cumulative)) +
  geom_segment( aes(x = day, xend = day, y = 0, yend = cumulative),
                size = 0.45, color = "#69b3a2", alpha = 0.7) +
  geom_point(size = 2, color = "#69b3a2", pch = 18) +
  labs(x = "fechas",
       y = "incidencia acumulada",
       title = "Incidencia acumulada",
       subtitle = "en el estado de Guanajuato",
       caption = paste0("observaciones hasta el 19 de junio de 2021",
                        "\nTotal casos: ", incid$n,
                        "\nIncidencia máxima: ", max(incid$counts))) +
  theme_ipsum() +
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank()) +
  
  scale_x_date(limit = c(as.Date(min(incid$dates)), "2021-07-24"),
               date_breaks = "14 days",
               date_labels = "%b%d") +
  ylim(0, max(df.total$cumulative))

acum


# ----- SOBREPONEMOS LA PRIMERA OLA (HASTA 24ago2020) A LA GRÁFICA DE INCIDENCIA -----
# Calculamos la incidencia
x <- incidence(df.24ago$FECHA_SINTOMAS)
y <- cumulate(x)

plot(x) #gráfica de incidencia
plot(y) #gráfica de incidencia acumulada
hist(x$counts, breaks = 35) #histograma de frecuencia de los casos por día

tot <- tot +
  annotate("rect", xmin = as.Date(min(x$dates)), xmax = as.Date(max(x$dates)), 
           ymin = -Inf, ymax = Inf, 
           alpha = .3)

acum <- acum +
  annotate("rect", xmin = as.Date(min(x$dates)), xmax = as.Date(max(x$dates)), 
           ymin = -Inf, ymax = Inf, 
           alpha = .3)

tot
acum

# ----- OBSERVACIONES PARA EL AJUSTE -----
fechas <- x$dates
incid <- x$counts
acum <- y$counts
total.casos <- y$n



# ----- DEFINIMOS LA EC. DE LA CURVA DE RICHARDS -----
# Definimos la función que determina el no. acumulado de infectados (Curva de Richards)

# I: no. acumulado de infectados al tiempo t
# K: capacidad de carga (no. total de casos en el brote)
# r: tasa de crecimiento de la población infectada
# tm: punto de inflexión de la curva epidémica
# a: exponente de la desviación de la curva logística
I <- function(t, K, r, tm, a){
  I = K / ( (1 + exp((-r)*(t - tm))))^(1/a) 
  return(I)
}





# ----- AJUSTE DEL MODELO -----
# Creamos los vectores de valores xval y yval para ajustar el modelo I
#xval es el vector de fechas transformado a double 
#yval es el vector de incidencia acumulada

xval <- as.double(fechas-min(fechas))
yval <- acum

# Fijamos las condiciones iniciales 
K_0 = 3700
r_0 = 0.05
tm_0 = 135
a_0 = 0.9 #también podemos usar 0.85


model <- nls(yval ~ I(xval, K, r, tm, a), 
             start = c(K = K_0, r = r_0, tm = tm_0, a = a_0),
             trace  = TRUE)

ci.95 <- nlstools::confint2(model, level = 0.95) 
#intervalos de confianza de los parámetros al 95%


# por medio de la función nls(nonlinear least squares) ajustamos la cruva de Richards
# nls predice la variable "yval" por medio de la variable "xval"
# otra forma de verlo es que "xval" es el tiempo (eje x) y "yval" son las observaciones


# OJO: nls es SUMAMENTE sensible a los parámetros iniciales,
# así que debemos tomar valores iniciales adecuados que se acerquen a la curva
# las siguientes gráficas son las que usé para estimar los valores iniciales
plot(0:200, I(0:200, 37000, 0.05, 130, 1), col="red") #inicial
points(xval, yval)

points(0:200, I(0:200, 37000, 0.055, 130, 0.8), col="blue")
points(0:200, I(0:200, 37000, 0.05, 135, 0.85), col="green") #mejor USAR ESTE!!!
points(0:200, I(0:200, 37000, 0.055, 135, 0.9), col="orange")


# ----- GRÁFICA: DATOS REALES VS. AJUSTE DEL MODELO -----
# "Damos continuidad" al intervalo de fechas para usar en el ajuste del modelo
# Tenemos que pasar como argumento del df "new.data" el nombre de la variable "xval"...
# pues es la que usamos en "model" (o en su defecto, el nombre con el que identificamos "xval")
# Luego, "predict" es una func. para predicciones a partir del resultado del modelo ingresado 
# en "predict" ingresamos newdata como el nuevo df, pero también podemos usar xval original

new.data <- data.frame(xval = seq(min(xval), max(xval), len = 500))
fit <- predict(model, newdata = new.data)

lwr.ci_data <- I(new.data, ci.95[1,1], ci.95[2,1], ci.95[3,1], ci.95[4,1])
upr.ci_data <- I(new.data, ci.95[1,2], ci.95[2,2], ci.95[3,2], ci.95[4,2])

# Para conocer los resultados del ajuste hay dos opciones y...
# ambas regresan una lista con los parámetros estimados
model$m$getAllPars()
coef(model)

data.table <- data.frame(dates = fechas,
                         val = acum)
fit.table <- data.frame(dates = seq(as.Date(min(fechas)), as.Date(max(fechas)), len = 500),
                        val = fit,
                        lw.fit = c(lwr.ci_data), up.fit = c(upr.ci_data))
names(fit.table)[3] <- "lw.fit"
names(fit.table)[4] <- "up.fit"


#col1 = "#69b3a2"
#col1 = "#ff8181"
col1 = "#C90076"
p <- ggplot(data = fit.table, aes(x = dates, y = val)) +
  geom_line(aes(linetype = "Ajuste"), color = col1, size = 1.3) + 
  geom_ribbon(aes(ymin = lw.fit, ymax = up.fit, 
                  fill = "Bandas de confiabilidad al 95%"),
              alpha = 0.25) +
  geom_point(data = data.table, 
             aes(x = dates, y = val, shape = "Observaciones"),
             size = 1) +
  
  theme(legend.title = element_blank()) +     # hide legend titles
  scale_fill_manual(values = col1) +        # set ribbon colour as grey
  guides(shape = guide_legend(order = 1),     # specify order of legends
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) + 
  
  labs(x = "fechas",
       y = "incidencia acumulada",
       title = "Ajuste de la curva de Richards",
       subtitle = paste0("parámetros estimados: ",
                         "\nK = 40119.58,  r = 0.0372,  ", 
                         expression(t[m]),
                         " = 116.81,  a = 0.477"),
       caption = paste0("observaciones hasta el 24 de agosto de 2020",
                        "\nTotal casos: ", total.casos,
                        "\nIncidencia máxima: ", max(incid))) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.title = element_blank()) +
  
  scale_x_date(limit=c(as.Date(min(fechas)), "2020-08-24"),
               date_breaks = "14 days",
               date_labels = "%b%d") +
  ylim(min(acum), max(acum))

p



# ----- SEGUNDO MÉTODO PARA ESTIMAR PARÁMETROS -----


# 
# rm(df.gto29may_sort, df.total29may, df.total29may_acum)
# rm(total.casos)
# rm(I, xval, yval,xval2)
# rm(model, pred, new.data)
