

#.------------------CORRER PRIMERO EL CÓDIGO SIR-------------------.



# ----- OBTENEMOS LA INCIDENCIA, INCIDENCIA ACUMULADA, DEFUNCIONES Y DEFUNCIONES ACUMULADAS ----
df.GTO_incid3 <- df.GTO2
df.GTO_incid3 <- aggregate.data.frame(df.GTO_incid3["CLASIFICACION_FINAL"],
                                     by = df.GTO_incid3["FECHA_SINTOMAS"], sum,
                                     na.rm = TRUE, na.action("na.pass")) #agrupa todos los datos por "FECHA_SINTOMAS" y cuenta cuántos casos hubieron cada día

df.GTO_incid3$acum <- cumsum(df.GTO_incid3$CLASIFICACION_FINAL)


df.GTO_incid3["preval14"] <- NA

df.GTO_incid3["incub5"] <- NA
df.GTO_incid3["desarr9"] <- NA

n=length(df.GTO_incid3$CLASIFICACION_FINAL)
for (i in 1:n) {
  if(i <= 14){
    df.GTO_incid3$preval14[i] = df.GTO_incid3$acum[i]
  }
  
  if(i >= 15){
    sum = 0
    for (j in 1:14) {
      sum = sum + df.GTO_incid3$CLASIFICACION_FINAL[i-j+1]
    }
    df.GTO_incid3$preval14[i] = sum
  }
}

n=length(df.GTO_incid3$CLASIFICACION_FINAL)
for (i in 1:n) {
  if(i <= 5){
    df.GTO_incid3$incub5[i] = df.GTO_incid3$acum[i]
  }
  
  if(i >= 6){
    sum = 0
    for (j in 1:5) {
      sum = sum + df.GTO_incid3$CLASIFICACION_FINAL[i-j+1]
    }
    df.GTO_incid3$incub5[i] = sum
  }
}


n=length(df.GTO_incid3$CLASIFICACION_FINAL)
for (i in 1:n) {
  if(i <= 14){
    df.GTO_incid3$desarr9[i] = df.GTO_incid3$acum[i] - df.GTO_incid3$incub5[i]
  }
  
  if(i >= 15){
    sum = 0
    for (j in 1:14) {
      sum = sum + df.GTO_incid3$CLASIFICACION_FINAL[i-j+1]
    }
    df.GTO_incid3$desarr9[i] = sum - df.GTO_incid3$incub5[i]
  }
}



rm(i,j,n,sum)

#----- DEFINIMOS EL MODELO  SEIR  ----
eta = 0.2 #porque sabemos que 1/eta es el tiempo de incubación, que son 5días
#tasas de cambio
#definimos el modelo SEIR a utilizar
SEIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    eta = 0.2
    
    dS <- -beta * I * S / N
    dE <- beta * I * S / N - eta * E
    dI <- eta * E - gamma * I
    dR <- gamma * I
    
    #regresa las tasas de cambio
    list(c(dS, dE, dI, dR))
  })
}




#---- DATOS PARA EL AJUSTE (PREVALENCIA 14 DÍAS)----
# en el vector "Infected" se encuentra la incidencia acumulada en Guanajuato estado 

seir_start_date <- "2020-12-30"
seir_end_date <- "2021-03-30"

Exposed <- subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd(seir_start_date) & FECHA_SINTOMAS <= ymd(seir_end_date))$incub5
Infected <- subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd(seir_start_date) & FECHA_SINTOMAS <= ymd(seir_end_date))$desarr9
Prevalence <- subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd(seir_start_date) & FECHA_SINTOMAS <= ymd(seir_end_date))$preval14


# Create an incrementing Day vector the same length as our
# cases vector
Day <- 1:(length(Infected))

# especificamos las condiciones iniciales para N, S, E, I, R
N = 6166934 #según INEGI 2020
init <- c(S = N - Infected[1] - Exposed[1], 
          E = Exposed[1],
          I = Infected[1], 
          R = 0)


# Definimos la función de error que calcula el RSS (residual sum of squares) 
# i.e. la suma de cuadrados residuales, pasando los parámetros beta y gamma
# que se desean ajustar mejor a los datos de incidencia 
RSS2 <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SEIR, parms = parameters)
  fit <- out[, 4]
  sum((Infected - fit)^2)
  print(sum((Infected - fit)^2))
}



# ---- AJUSTE DEL MODELO ----

Opt1 <- optim(c(0.1, 0.075),
             RSS2,
             method = "L-BFGS-B",
             lower = c(0, 0),
             upper = c(1, 1))

#Opt2 <- optimx::optimx(c(0.5, 0.05), RSS2)

Opt2 <- optim(c(0.4, 0.057),
              RSS2, method = "L-BFGS-B",
              lower = c(0, 0.05),
              upper = c(1, 0.1))


Opt3 <- optim(c(0.9, 0.057),
              RSS2, method = "L-BFGS-B",
              lower = c(0, 0),
              upper = c(1,1))

Opt4 <- optim(c(0.9, 0.057),
              RSS2, method = "L-BFGS-B",
              lower = c(0, 0.05),
              upper = c(1, 0.1))


# check for convergence
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
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
Opt <- Opt2

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

#usar esta forma de Opt_par cuando se use la función "optimx"
# Opt_par <- c(Opt2$p1[1], Opt2$p2[1])
# Opt_par <- setNames(Opt_par, c("beta", "gamma"))
# Opt_par

rm(Opt1, Opt2, Opt3, Opt4)


# ---- GRAFICANDO OBSERVACIONES VS. AJUSTE ----
# time in days for predictions
t <- 1:as.integer(ymd(seir_end_date) - ymd(seir_start_date) + 1) #a veces es necesario agregar +1

# get the fitted values from our SIR model
SEIRfitted_cumulative_incidence <- data.frame(ode(
  y = init, times = t,
  func = SEIR, parms = Opt_par
))

# add a Date column and the observed incidence data
SEIRfitted_cumulative_incidence <- SEIRfitted_cumulative_incidence %>%
  mutate(
    Date = ymd(seir_start_date) + days(t - 1),
    State = "Guanajuato",
    SEIRcumulative_incident_cases = Infected, #I
    SEIRcumulative_exposed_cases = Exposed, #E
    SEIRcumulative_prevalence_cases = Prevalence,
    SEIRcumulative_incidence = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd(seir_start_date) & FECHA_SINTOMAS <= ymd(seir_end_date))$CLASIFICACION_FINAL #incidencia (casos)
  )

# plot the data





#----------- Gráfica en el intervalo seir_start_date - seir_end_date
Sys.setenv("LANGUAGE"="Sp")

q <- ggplot() + 
  geom_point(data = SEIRfitted_cumulative_incidence, 
             aes(x = Date, y = SEIRcumulative_incident_cases, color = "infecciosos")) +
  geom_point(data = SEIRfitted_cumulative_incidence,
             aes(x = Date, y = SEIRcumulative_incidence, color = "incidencia")) +
  geom_line(data = SEIRfitted_cumulative_incidence,
            aes(x = Date, y = I, color = "ajuste"), size = 0.8) +
  
  labs(x="días", y="no. de casos",
       title = paste0("Modelo SEIR en el estado de Guanajuato:",
                      "\n     ajuste vs. prevalencia a 14 días (incubación 5 días)"),
       subtitle = paste0("parámetros estimados:     beta = ", 
                         round(Opt_par[[1]],4) ,
                         ",    gamma =  ", round(Opt_par[[2]],4),
                         ",    beta / gamma =  ", round(Opt_par[[1]]/Opt_par[[2]],4),
                         ",    1 / gamma =  ", round(1/Opt_par[[2]],4)),
       color = " ",
       caption = paste0("Fechas para el ajuste:\n", 
                        format(as.Date(seir_start_date), "%d·%b·%Y"), " - ", 
                        format(as.Date(seir_end_date), "%d·%b·%Y"))) +
  
  theme_ipsum() +
  
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.justification = c("right", "center")) +
  scale_x_date(limit=c(as.Date(seir_start_date), as.Date(seir_end_date)),
               date_breaks = "14 days", 
               date_labels = "%b%d") +
  
  scale_color_brewer(palette="Set2")
#scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

q




#----------- Gráficas en perspectiva de la pandemia completa
#Para visualizar de forma completa el ajuste, graficamos todo
# tiempo (en días) para las estimaciones
t2 <- 1:as.integer(ymd("2021-06-19") - ymd("2020-03-10"))

# obteniendo los valores ajustados del modelo SEIR
SEIRfitted_cumulative_incidence2 <- data.frame(ode(
  y = init, times = t2,
  func = SEIR, parms = Opt_par ))

# add a Date column and the observed incidence data
SEIRfitted_cumulative_incidence2 <- SEIRfitted_cumulative_incidence2 %>%
  mutate(
    Date = ymd("2020-03-10") + days(t2 - 1),
    State = "Guanajuato",
    SEIRcumulative_incident_cases2 = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd("2020-03-10") & FECHA_SINTOMAS <= ymd("2021-06-19"))$desarr9,
    SEIRcumulative_exposed_cases2 = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd("2020-03-10") & FECHA_SINTOMAS <= ymd("2021-06-19"))$incub5,
    SEIRcumulative_prevalence_cases2 = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd("2020-03-10") & FECHA_SINTOMAS <= ymd("2021-06-19"))$preval14,
    SEIRcumulative_incidence2 = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd("2020-03-10") & FECHA_SINTOMAS <= ymd("2021-06-19"))$CLASIFICACION_FINAL
  )

Sys.setenv("LANGUAGE"="Sp")

q_ext <- ggplot() + 
  geom_point(data = SEIRfitted_cumulative_incidence2, 
             aes(x = Date, y = SEIRcumulative_incident_cases2, color = "prevalencia")) +
  geom_point(data = SEIRfitted_cumulative_incidence2,
             aes(x = Date, y = SEIRcumulative_incidence2, color = "incidencia")) +
  geom_line(data = SEIRfitted_cumulative_incidence,
            aes(x = Date, y = I, color = "ajuste"), size = 0.8) +
  
  labs(x="días", y="no. de casos",
       title = paste0("Modelo SEIR en el estado de Guanajuato:",
                      "\n     ajuste vs. prevalencia a 14 días (incubación 5 días)"),
       subtitle = paste0("parámetros estimados:     beta = ", 
                         round(Opt_par[[1]],4) ,
                         ",    gamma =  ", round(Opt_par[[2]],4),
                         ",    beta / gamma =  ", round(Opt_par[[1]]/Opt_par[[2]],4),
                         ",    1 / gamma =  ", round(1/Opt_par[[2]],4)),
       color = " ",
       caption = paste0("Fechas para el ajuste:\n", 
                        format(as.Date(seir_start_date), "%d·%b·%Y"), " - ", 
                        format(as.Date(seir_end_date), "%d·%b·%Y"))) +
  
  theme_ipsum() +
  
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.justification = c("right", "center")) +
  scale_x_date(limit=c(as.Date("2020-03-10"), as.Date("2021-06-19")),
               date_breaks = "14 days", 
               date_labels = "%b%d") +
  
  scale_color_brewer(palette="Set2")
#scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))


q_ext


# sombreamos el intervalo de fechas que usamos para la estimación SEIR
q_ext <- q_ext +
  annotate("rect", xmin = as.Date(seir_start_date), xmax = as.Date(seir_end_date), 
           ymin = -Inf, ymax = Inf, 
           alpha = .2)

q_ext



#......................................

t3 <- 1:as.integer(ymd("2021-06-19") - ymd(seir_start_date) + 1)

# obteniendo los valores ajustados del modelo SEIR
SEIRfitted_cumulative_incidence3 <- data.frame(ode(
  y = init, times = t3,
  func = SEIR, parms = Opt_par ))

# add a Date column and the observed incidence data
SEIRfitted_cumulative_incidence3 <- SEIRfitted_cumulative_incidence3 %>%
  mutate(
    Date = ymd(seir_start_date) + days(t3 - 1),
    State = "Guanajuato",
    SEIRcumulative_incident_cases3 = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd(seir_start_date) & FECHA_SINTOMAS <= ymd("2021-06-19"))$desarr9,
    SEIRcumulative_exposed_cases3 = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd(seir_start_date) & FECHA_SINTOMAS <= ymd("2021-06-19"))$incub5,
    SEIRcumulative_prevalence_cases3 = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd(seir_start_date) & FECHA_SINTOMAS <= ymd("2021-06-19"))$preval14,
    SEIRcumulative_incidence3 = subset(df.GTO_incid3, FECHA_SINTOMAS >= ymd(seir_start_date) & FECHA_SINTOMAS <= ymd("2021-06-19"))$CLASIFICACION_FINAL
  )


Sys.setenv("LANGUAGE"="Sp")

q_persp <- ggplot() + 
  geom_point(data = SEIRfitted_cumulative_incidence3, 
             aes(x = Date, y = SEIRcumulative_incident_cases3, color = "prevalencia")) +
  geom_point(data = SEIRfitted_cumulative_incidence3,
             aes(x = Date, y = SEIRcumulative_incidence3, color = "incidencia")) +
  geom_line(data = SEIRfitted_cumulative_incidence3,
            aes(x = Date, y = I, color = "ajuste"), size = 0.8) +
  
  labs(x="días", y="no. de casos",
       title = paste0("Modelo SEIR en el estado de Guanajuato:",
                      "\n     ajuste vs. prevalencia a 14 días (incubación 5 días)"),
       subtitle = paste0("parámetros estimados:     beta = ", 
                         round(Opt_par[[1]],4) ,
                         ",    gamma =  ", round(Opt_par[[2]],4),
                         ",    beta / gamma =  ", round(Opt_par[[1]]/Opt_par[[2]],4),
                         ",    1 / gamma =  ", round(1/Opt_par[[2]],4)),
       color = " ",
       caption = paste0("Fechas para el ajuste:\n", 
                        format(as.Date(seir_start_date), "%d·%b·%Y"), " - ", 
                        format(as.Date(seir_end_date), "%d·%b·%Y"))) +
  
  theme_ipsum() +
  
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.justification = c("right", "center")) +
  scale_x_date(limit=c(as.Date(seir_start_date), as.Date("2021-06-19")),
               date_breaks = "14 days", 
               date_labels = "%b%d") +
  
  scale_color_brewer(palette="Set2")
#scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))


q_persp


# sombreamos el intervalo de fechas que usamos para la estimación SEIR
q_persp <- q_persp +
  annotate("rect", xmin = as.Date(seir_start_date), xmax = as.Date(seir_end_date), 
           ymin = -Inf, ymax = Inf, 
           alpha = .2)

q_persp






