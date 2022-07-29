# ----- INSTALAMOS PAQUETES Y LIBRERÍAS NECESARIOS -----
install.packages("deSolve")
library(deSolve)
install.packages("optimx")
library(optimx)
install.packages("ggplot2")
library(ggplot2)
install.packages("readr")
library(readr)
install.packages("lubridate") #no usar chron
library(lubridate)
install.packages("dplyr")
library(dplyr)




# ----- FIJAMOS EL DIRECTORIO EN EL QUE TRABAJAREMOS -----
setwd("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/0. GTO")

# ----- IMPORTAMOS EL .csv CON LOS DATOS DE INTERÉS -----
df.GTO <- read_csv("18-GTO.csv", col_types = cols(FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"),
                                                  FECHA_DEF = col_date(format = "%Y-%m-%d"),
                                                  FECHA_INGRESO = col_skip(), 
                                                  ID_REGISTRO = col_skip(), MUNICIPIO_RES = col_skip(),
                                                  ENTIDAD_RES = col_skip(), RESULTADO_LAB = col_skip(),
                                                  RESULTADO_ANTIGENO = col_skip(), EDAD = col_skip(),
                                                  SEXO = col_skip()))
df.GTO <- subset.data.frame(df.GTO,
                            (CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3),
                            select=c(FECHA_SINTOMAS, FECHA_DEF, CLASIFICACION_FINAL))

df.GTO2 <- subset.data.frame(df.GTO,
                             (CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3 | CLASIFICACION_FINAL==7),
                             select=c(FECHA_SINTOMAS, FECHA_DEF, CLASIFICACION_FINAL))
df.GTO2$CLASIFICACION_FINAL[df.GTO2$CLASIFICACION_FINAL==2] <- 1
df.GTO2$CLASIFICACION_FINAL[df.GTO2$CLASIFICACION_FINAL==3] <- 1
#Y cambiamos el identificador de CLASIFICACION_FINAL negativo  7 a 0 en el df de casos positivos (por simplicidad)
df.GTO2$CLASIFICACION_FINAL[df.GTO2$CLASIFICACION_FINAL==7] <- 0





# ----- DEFINIMOS LAS ECUACIONES DEL MODELO SIR -----

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    #tasas de cambio
    dS <- -beta * I * S / N
    dI <- beta * I * S / N - gamma * I
    dR <- gamma * I
    
    #regresa las tasas de cambio
    list(c(dS, dI, dR))
  })
}




# ----- SELECCIONAMOS LAS OBSERVACIONES CON LAS QUE QUEREMOS TRABAJAR -----
# Guardamos en un df incidencia e incidencia acumulada
df.GTO_incid <- df.GTO2
df.GTO_incid <- aggregate.data.frame(df.GTO_incid["CLASIFICACION_FINAL"],
                                     by = df.GTO_incid["FECHA_SINTOMAS"], sum,
                                     na.rm = TRUE, na.action("na.pass"))

df.GTO_incid$acum <- cumsum(df.GTO_incid$CLASIFICACION_FINAL)


# El ajuste del modelo SIR debe ser con PREVALENCIA, NO con la incidencia
# Así que calculamos la prevalencia y la añadimos al df
df.GTO_incid["preval14"] <- NA
df.GTO_incid["preval10"] <- NA

n=length(df.GTO_incid$CLASIFICACION_FINAL)
for (i in 1:n) {
  if(i <= 14){
    df.GTO_incid$preval14[i] = df.GTO_incid$acum[i]
  }
  
  if(i >= 15){
    sum = 0
    for (j in 1:14) {
      sum = sum + df.GTO_incid$CLASIFICACION_FINAL[i-j+1]
    }
    df.GTO_incid$preval14[i] = sum
  }
}

n=length(df.GTO_incid$CLASIFICACION_FINAL)
for (i in 1:n) {
  if(i <= 10){
    df.GTO_incid$preval10[i] = df.GTO_incid$acum[i]
  }
  
  if(i >= 11){
    sum = 0
    for (j in 1:10) {
      sum = sum + df.GTO_incid$CLASIFICACION_FINAL[i-j+1]
    }
    df.GTO_incid$preval10[i] = sum
  }
}


rm(i,j,n,sum)


# ---- CONDICIONES INICIALES PARA EL MODELO (N - S - I - R) PREVAL14 ----

# Primero fijamos las fechas de inicio y fin del SIR
sir_start_date <- "2020-10-01"
sir_end_date <- "2021-03-31"

# Ahora creamos un vector de observaciones (¡¡prevalencia!!) y de conteo de días del SIR
Infected <- subset(df.GTO_incid, FECHA_SINTOMAS >= ymd(sir_start_date) & FECHA_SINTOMAS <= ymd(sir_end_date))$preval14
Day <- 1:(length(Infected)) # de la misma longitud que el vector de infectados

# especificamos las condiciones iniciales para N, S, I, R
N = 6166934 # introducimos el no. total de la población (según INEGI 2020)
init <- c(S = N - Infected[1], I = Infected[1], R = 0)




# ---- DEFINIMOS LA FUNCIÓN DE ERROR A OPTIMIZAR (Cuadrados Residuales) ----
# Definimos la función de error que calcula el RSS (residual sum of squares) 
# i.e. la suma de cuadrados residuales, pasando los parámetros beta y gamma
# que se desean ajustar mejor a los datos de incidencia 

RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[, 3] #columna 3 = infectados
  sum((Infected - fit)^2)
  print(sum((Infected - fit)^2))
}




# ---- AJUSTE DEL MODELO ----
# Recordemos que la epidemia va en ascenso, por lo que beta / gamma > 1
# Así que imponemos las restricción: beta > gamma
# Realizamos distintos ajustes para elegir al final el que más convenga
# Recordemos que además como la tasa de recuperación oscila entre 10 y 20 días
# entonces (1/gamma) \in [0.05, 0.1]

Opt1 <- optim(c(0.5, 0.057),
              RSS, method = "L-BFGS-B",
              lower = c(0, 0.05),
              upper = c(1, 0.1))

Opt2 <- optim(c(0.9, 0.5),
             RSS, method = "L-BFGS-B",
             lower = c(0, 0.05),
             upper = c(1,0.1))

Opt3 <- optim(c(0.9, 0.057),
              RSS, method = "L-BFGS-B",
              lower = c(0, 0.05),
              upper = c(1, 0.1))

Opt4 <- optim(c(0.1, 0.075), 
             RSS, method = "L-BFGS-B",
             lower = c(0, 0.05),
             upper = c(1.0, 0.1))


  # tenemos que obtener el mensaje: "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
Opt1$message
Opt2$message
Opt3$message
Opt4$message

Opt1$par
Opt2$par
Opt3$par
Opt4$par

# Elegimos la mejor optimización y guardamos los parámetros estimados
Opt <- Opt3

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

rm(Opt1, Opt2, Opt3, Opt4)




#---- GRAFICANDO AJUSTE VS. OBSERVACIONES ----
# tiempo (en días) para las estimaciones
t <- 1:as.integer(ymd(sir_end_date) - ymd(sir_start_date) + 1) #aquí agregar un +1 si es necesario

# obteniendo los valores ajustados del modelo SIR
fitted_cumulative_incidence <- data.frame(ode(
  y = init, times = t,
  func = SIR, parms = Opt_par ))

# add a Date column and the observed incidence data
fitted_cumulative_incidence <- fitted_cumulative_incidence %>%
  mutate(
    Date = ymd(sir_start_date) + days(t - 1),
    State = "Guanajuato",
    cumulative_incident_cases = Infected, #prevalencia
    cumulative_incidence = subset(df.GTO_incid, FECHA_SINTOMAS >= ymd(sir_start_date) & FECHA_SINTOMAS <= ymd(sir_end_date))$CLASIFICACION_FINAL #incidencia real
  )





#----------- Gráfica en el intervalo sir_start_date - sir_end_date
Sys.setenv("LANGUAGE"="Sp")

p <- ggplot() + 
  geom_point(data = fitted_cumulative_incidence, 
             aes(x = Date, y = cumulative_incident_cases, color = "prevalencia")) +
  geom_point(data = fitted_cumulative_incidence,
            aes(x = Date, y = cumulative_incidence, color = "incidencia")) +
  geom_line(data = fitted_cumulative_incidence,
            aes(x = Date, y = I, color = "ajuste"), size = 0.8) +
  
  labs(x="días", y="no. de casos",
       title = paste0("Modelo SIR en el estado de Guanajuato:",
                      "\n     ajuste vs. prevalencia a 14 días"),
       subtitle = paste0("parámetros estimados:     beta = ", 
                         round(Opt_par[[1]],4) ,
                         ",    gamma =  ", round(Opt_par[[2]],4),
                         ",    beta / gamma =  ", round(Opt_par[[1]]/Opt_par[[2]],4),
                         ",    1 / gamma =  ", round(1/Opt_par[[2]],4)),
       color = " ",
       caption = paste0("Fechas para el ajuste:\n", 
                        format(as.Date(sir_start_date), "%d·%b·%Y"), " - ", 
                        format(as.Date(sir_end_date), "%d·%b·%Y"))) +
  
  theme_ipsum() +
  
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.justification = c("right", "center")) +
  scale_x_date(limit=c(as.Date(sir_start_date), as.Date(sir_end_date)),
               date_breaks = "14 days", 
               date_labels = "%b%d") +
  
  scale_color_brewer(palette="Accent")
  #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

p




#----------- Gráficas en perspectiva de la pandemia completa
#Para visualizar de forma completa el ajuste, graficamos todo
# tiempo (en días) para las estimaciones
t2 <- 1:as.integer(ymd("2021-06-19") - ymd("2020-03-10"))

# obteniendo los valores ajustados del modelo SIR
fitted_cumulative_incidence2 <- data.frame(ode(
  y = init, times = t2,
  func = SIR, parms = Opt_par))

# add a Date column and the observed incidence data
fitted_cumulative_incidence2 <- fitted_cumulative_incidence2 %>%
  mutate(
    Date = ymd("2020-03-10") + days(t2 - 1),
    State = "Guanajuato",
    cumulative_incident_cases2 = subset(df.GTO_incid, FECHA_SINTOMAS >= ymd("2020-03-10") & FECHA_SINTOMAS <= ymd("2021-06-19"))$preval14, #prevalencia
    cumulative_incidence2 = subset(df.GTO_incid, FECHA_SINTOMAS >= ymd("2020-03-10") & FECHA_SINTOMAS <= ymd("2021-06-19"))$CLASIFICACION_FINAL #incidencia real
  )


Sys.setenv("LANGUAGE"="Sp")

p_ext <- ggplot() + 
  geom_point(data = fitted_cumulative_incidence2, 
             aes(x = Date, y = cumulative_incident_cases2, color = "prevalencia")) +
  geom_point(data = fitted_cumulative_incidence2,
             aes(x = Date, y = cumulative_incidence2, color = "incidencia")) +
  geom_line(data = fitted_cumulative_incidence,
            aes(x = Date, y = I, color = "ajuste"), size = 0.8) +
  
  labs(x="días", y="no. de casos",
       title = paste0("Modelo SIR en el estado de Guanajuato:",
                      "\n     ajuste vs. prevalencia a 14 días"),
       subtitle = paste0("parámetros estimados:     beta = ", 
                         round(Opt_par[[1]],4) ,
                         ",    gamma =  ", round(Opt_par[[2]],4),
                         ",    beta / gamma =  ", round(Opt_par[[1]]/Opt_par[[2]],4),
                         ",    1 / gamma =  ", round(1/Opt_par[[2]],4)),
       color = " ",
       caption = paste0("Fechas para el ajuste:\n", 
                        format(as.Date(sir_start_date), "%d·%b·%Y"), " - ", 
                        format(as.Date(sir_end_date), "%d·%b·%Y"))) +
  
  theme_ipsum() +
  
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.justification = c("right", "center")) +
  scale_x_date(limit=c(as.Date("2020-03-10"), as.Date("2021-06-19")),
               date_breaks = "14 days", 
               date_labels = "%b%d") +
  
  scale_color_brewer(palette="Accent")
#scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))


p_ext


# sombreamos el intervalo de fechas que usamos para la estimación SIR
p_ext <- p_ext +
  annotate("rect", xmin = as.Date(sir_start_date), xmax = as.Date(sir_end_date), 
           ymin = -Inf, ymax = Inf, 
           alpha = .2)

p_ext



#......................................

t3 <- 1:as.integer(ymd("2021-06-19") - ymd(sir_start_date) + 1)

# obteniendo los valores ajustados del modelo SIR
fitted_cumulative_incidence3 <- data.frame(ode(
  y = init, times = t3,
  func = SIR, parms = Opt_par ))

# add a Date column and the observed incidence data
fitted_cumulative_incidence3 <- fitted_cumulative_incidence3 %>%
  mutate(
    Date = ymd(sir_start_date) + days(t3 - 1),
    State = "Guanajuato",
    cumulative_incident_cases3 = subset(df.GTO_incid, FECHA_SINTOMAS >= ymd(sir_start_date) & FECHA_SINTOMAS <= ymd("2021-06-19"))$preval14, #prevalencia
    cumulative_incidence3 = subset(df.GTO_incid, FECHA_SINTOMAS >= ymd(sir_start_date) & FECHA_SINTOMAS <= ymd("2021-06-19"))$CLASIFICACION_FINAL #incidencia real
  )


Sys.setenv("LANGUAGE"="Sp")

p_persp <- ggplot() + 
  geom_point(data = fitted_cumulative_incidence3, 
             aes(x = Date, y = cumulative_incident_cases3, color = "prevalencia")) +
  geom_point(data = fitted_cumulative_incidence3,
             aes(x = Date, y = cumulative_incidence3, color = "incidencia")) +
  geom_line(data = fitted_cumulative_incidence3,
            aes(x = Date, y = I, color = "ajuste"), size = 0.8) +
  
  labs(x="días", y="no. de casos",
       title = paste0("Modelo SIR en el estado de Guanajuato:",
                      "\n     ajuste vs. prevalencia a 14 días"),
       subtitle = paste0("parámetros estimados:     beta = ", 
                         round(Opt_par[[1]],4) ,
                         ",    gamma =  ", round(Opt_par[[2]],4),
                         ",    beta / gamma =  ", round(Opt_par[[1]]/Opt_par[[2]],4),
                         ",    1 / gamma =  ", round(1/Opt_par[[2]],4)),
       color = " ",
       caption = paste0("Fechas para el ajuste:\n", 
                        format(as.Date(sir_start_date), "%d·%b·%Y"), " - ", 
                        format(as.Date(sir_end_date), "%d·%b·%Y"))) +
  
  theme_ipsum() +
  
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.justification = c("right", "center")) +
  scale_x_date(limit=c(as.Date(sir_start_date), as.Date("2021-06-19")),
               date_breaks = "14 days", 
               date_labels = "%b%d") +
  
  scale_color_brewer(palette="Accent")
#scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))


p_persp


# sombreamos el intervalo de fechas que usamos para la estimación SIR
p_persp <- p_persp +
  annotate("rect", xmin = as.Date(sir_start_date), xmax = as.Date(sir_end_date), 
           ymin = -Inf, ymax = Inf, 
           alpha = .2)

p_persp







