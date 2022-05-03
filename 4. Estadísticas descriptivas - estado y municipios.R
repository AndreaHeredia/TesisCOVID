#-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-·-
# ESTADÍSTICAS DESCRIPTIVAS DEL ESTADO Y DE CADA MUNICIPIO
#_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._
# Correr este código después de "2. Plot State - incidence and cumulative"
# Usaremos el data frame "df.GTO" del código anterior

# Calcularemos indicadores epidemiológicos y descriptores estadísticos:
    # - Tasa de incidencia por 10mil habitantes
    # - Total de defunciones
    # - Casos activos
    # - Porcentaje de defunciones
    # 
    # - No. mínimo de infectados
    # - No. máximo de infectados
    # - Total de casos
    # 
    # - Moda
    # - Media
    # - Mediana
    # - Cuantil 1
    # 
    # - Rango (intervalo del mín al máx)
    # - Desviación estándar
    # - Varianza
    # - Coeficiente variacional (def std / media)

# Lo anterior lo ponemos en una tabla y lo guardaremos en un .csv
#_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._


library(readr)
library(lubridate)
library(chron)
library(datetime)
library(data.table)



municipios <- c("GTO", 
                "Guanajuato", "León", "Celaya",
                "Silao", "Irapuato", "Salamanca")
habitantes <- c(6166934, 
                194500, 1721215, 97928 + 65791 + 521169  + 94126,
                203556, 592953, 273417)

# Create the function mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#---- FIJAMOS EL DIRECTORIO DE TRABAJO ----
setwd("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021") 


#---- SEPARAMOS LOS DATOS DE CADA MUNICIPIO ----
#Para esto, usamos "df.GTO" mencionado al inicio del código

# Guanajuato = 015
# León = 020
# Cortazar + Villagrán + Celaya + Salvatierra = 011 + 044 + 007 + 028
# Silao = 037
# Irapuato = 017
# Salamanca = 027

df.Guanajuato <- subset.data.frame(df.GTO, MUNICIPIO_RES == 15,
                                   select=c(FECHA_SINTOMAS, FECHA_DEF, 
                                            CLASIFICACION_FINAL, 
                                            MUNICIPIO_RES,
                                            SEXO, EDAD, RANGO), drop=FALSE)
df.León <- subset.data.frame(df.GTO, MUNICIPIO_RES == 20,
                             select=c(FECHA_SINTOMAS, FECHA_DEF, 
                                      CLASIFICACION_FINAL, 
                                      MUNICIPIO_RES,
                                      SEXO, EDAD, RANGO), drop=FALSE)
df.Celaya <- subset.data.frame(df.GTO, 
                               (MUNICIPIO_RES == 11 | MUNICIPIO_RES == 44 | MUNICIPIO_RES == 7 | MUNICIPIO_RES == 28),
                               select=c(FECHA_SINTOMAS, FECHA_DEF, 
                                        CLASIFICACION_FINAL, 
                                        MUNICIPIO_RES,
                                        SEXO, EDAD, RANGO), drop=FALSE)
df.Silao <- subset.data.frame(df.GTO, MUNICIPIO_RES == 37,
                              select=c(FECHA_SINTOMAS, FECHA_DEF, 
                                       CLASIFICACION_FINAL, 
                                       MUNICIPIO_RES,
                                       SEXO, EDAD, RANGO), drop=FALSE)
df.Irapuato <- subset.data.frame(df.GTO, MUNICIPIO_RES == 17,
                                 select=c(FECHA_SINTOMAS, FECHA_DEF, 
                                          CLASIFICACION_FINAL, 
                                          MUNICIPIO_RES,
                                          SEXO, EDAD, RANGO), drop=FALSE)
df.Salamanca <- subset.data.frame(df.GTO, MUNICIPIO_RES == 27,
                                  select=c(FECHA_SINTOMAS, FECHA_DEF, 
                                           CLASIFICACION_FINAL, 
                                           MUNICIPIO_RES,
                                           SEXO, EDAD, RANGO), drop=FALSE)
  

#---- USAMOS EL PAQUETE INCIDENCE PARA CALCULAR INCIDENCIA Y DEFUNCIONES ----
library(incidence)
library(outbreaks)


# Aquí obtenemos la incidencia para el estado y municipios
n = length(municipios)
for (i in 1:n) {
  assign(paste0("incid.", municipios[i]),
         incidence(as.Date(get(paste0("df.", municipios[i]))$FECHA_SINTOMAS, 
                           format = "%Y-%m-%d"), 
                   groups = NULL, interval = 1))
}


# Aquí obtenemos la incidencia acumulada para el estado y municipios
n = length(municipios)
for (i in 1:n) {
  assign(paste0("acum.", municipios[i]),
         cumulate(get(paste0("incid.", municipios[i]))))
}

# Finalmente calculamos las defunciones y defunciones acumuladas para el estado y municipios
n = length(municipios)
for (i in 1:n) {
  assign(paste0("defs.", municipios[i]),
         incidence(as.Date(get(paste0("df.", municipios[i]))$FECHA_DEF, format = "%Y-%m-%d"), 
                   groups = NULL, interval = 1))
  assign(paste0("defs_acum.", municipios[i]),
         cumulate(get(paste0("defs.", municipios[i]))))
}

#Aquí agregamos a incid.municipios[i] un vector con los casos activos a 14 días
n = length(municipios)
for (l in 1:n) {
  k = length(get(paste0("incid.", municipios[l]))$counts)
  for (i in 1:k) {
    if(i <= 14){
      .GlobalEnv[[paste0("incid.", municipios[l])]]$ ACTIV_14[i] <-  get(paste0("acum.", municipios[l]))$counts[i]
    }
    if(i >= 15){
      sum = 0
      for(j in 1:14){
        sum = sum + get(paste0("incid.", municipios[l]))$counts[i-j+1]
      }
      .GlobalEnv[[paste0("incid.", municipios[l])]]$ ACTIV_14[i] <-  sum
    }
  }
}


#Realizamos algunas gráficas que permitan ilustrar...
#...incidencias, defunciones y casos acumulados

i7 <- grid.arrange(plot(incid.GTO), plot(incid.Guanajuato),
                   plot(incid.León), plot(incid.Celaya),
                   plot(incid.Silao), plot(incid.Irapuato),
                   plot(incid.Salamanca), ncol = 2, top="Incidencias")
a7 <- grid.arrange(plot(acum.GTO), plot(acum.Guanajuato),
                   plot(acum.León), plot(acum.Celaya),
                   plot(acum.Silao), plot(acum.Irapuato),
                   plot(acum.Salamanca), ncol = 2, top="Incidencias Acumuladas")
d7 <- grid.arrange(plot(defs.GTO), plot(defs.Guanajuato),
                   plot(defs.León), plot(defs.Celaya),
                   plot(defs.Silao), plot(defs.Irapuato),
                   plot(defs.Salamanca), ncol = 2, top="Defuncones")
da7 <- grid.arrange(plot(defs_acum.GTO), plot(defs_acum.Guanajuato),
                   plot(defs_acum.León), plot(defs_acum.Celaya),
                   plot(defs_acum.Silao), plot(defs_acum.Irapuato),
                   plot(defs_acum.Salamanca), ncol = 2, top="Defunciones Acumuladas")

# ----- USANDO INCIDENCE PACKAGE PARA GRÁFICAS PASTEL----
library(outbreaks)
library(ggplot2)
library(incidence)

library(grid)
library(gridExtra)

install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)

library(dplyr)
library(ggrepel)
library(forcats)
library(scales)

library(extrafont)


df.GTO <- subset.data.frame(df.MESES_GTO[[18]],
                            (CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3),
                            select=c(FECHA_SINTOMAS, FECHA_DEF, 
                                     CLASIFICACION_FINAL, 
                                     MUNICIPIO_RES,
                                     SEXO, EDAD))

#son las fechas de los datos a agrupar
data <- as.Date(df.GTO$FECHA_SINTOMAS, format = "%Y-%m-%d")
datadef <- as.Date(df.GTO$FECHA_DEF, format = "%Y-%m-%d")

#agruparemos los datos positivos de acuerdo a su clase
clasif <- df.GTO$CLASIFICACION_FINAL

df.GTO$SEXO[df.GTO$SEXO==1] <- "Mujer"
df.GTO$SEXO[df.GTO$SEXO==2] <- "Hombre"
df.GTO$SEXO[df.GTO$SEXO==99] <- "No identificado"

# Clasificación de edades
# A:= <18, B:= 18-25, C:= 26-39, D:= 40-59, E:= >=60
df.GTO$RANGO[df.GTO$EDAD<18] <- "A" 
df.GTO$RANGO[df.GTO$EDAD>=18 & df.GTO$EDAD<=25] <- "B"
df.GTO$RANGO[df.GTO$EDAD>=26 & df.GTO$EDAD<=39] <- "C"
df.GTO$RANGO[df.GTO$EDAD>=40 & df.GTO$EDAD<=59] <- "D"
df.GTO$RANGO[df.GTO$EDAD>=60] <- "E"


#obtenemos la incidencia por grupo(tipo de positivo, i.e. CLASIFICACION_FINAL=1,2,3)
#el intervalo de la incidencia es por día y luego lo graficamos
incid <- incidence(data, groups = df.GTO$SEXO, interval = 1)#cambiamos el gpo por SEXO (h ó m) ahí puede ir el vector "clasif"
incid2 <- incidence(data, groups = df.GTO$RANGO, interval = 1)

incid3 <- incidence(data, groups=NULL, interval = 1)
plot(incid3)


def <- incidence(datadef, groups = df.GTO$SEXO, interval = 1)
def2 <- incidence(datadef, groups = df.GTO$RANGO, interval = 1)

acum <- cumulate(incid)
acumdef <- cumulate(def)

i1 <- plot(incid)
i2 <- plot(incid2)
a <- plot(acum)
p <- grid.arrange(i1, i2, a, ncol = 1, top="Incidencia")

d <- plot(def)
d2 <- plot(def2)
da <- plot(acumdef)
q <- grid.arrange(d, d2, da, ncol = 1, top="Defunciones")
#ggsave("Incidencia_GTO.png", width = 16, height = 9)

#.-.-.-.Creamos gráficas de pastel para visualizar mejor la proporción de 
# contagios y defunciones por sexo y por grupos de edad

sum_of_obs <- sum(incid$counts) 
#esto equivale a "sum(df.GTO$SEXO=="Hombre") + sum(df.GTO$SEXO=="Mujer")"
# 
# data_hm <- data.frame(
#   sexo = c("Hombres", "Mujeres"),
#   value = c(sum(df.GTO$SEXO=="Hombre"), sum(df.GTO$SEXO=="Mujer")),
#   per = c(sum(df.GTO$SEXO=="Hombre")/sum_of_obs,
#           sum(df.GTO$SEXO=="Mujer")/sum_of_obs)
# )

data_hm <- data.frame(
  sexo = df.GTO$SEXO,
  n = 1:sum_of_obs
)
data_hm <- data_hm %>% 
  group_by(sexo) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(sexo))

data_hm$label <- scales::percent(data_hm$per)

pie_hm <- ggplot(data=data_hm) +
  geom_bar(aes(x="", y=per, fill=sexo),
           stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(title = "Incidencia en el estado de Guanajuato:",
       subtitle = "clasificación de acueerdo al sexo",
       caption = paste0("Total casos: ", sum(incid$counts))) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2)) +
  
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label),
            size=4, color="white") +
  theme_void() # remove background, grid, numeric labels

pie_hm



data_edades <- data.frame(
  edades = df.GTO$RANGO,
  n = 1:sum_of_obs
)
data_edades <- data_edades %>% 
  group_by(edades) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(edades))

data_edades$label <- scales::percent(data_edades$per)


pie_edades <- ggplot(data=data_edades) +
  geom_bar(aes(x="", y=per, fill=edades),
           stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(title = "Incidencia en el estado de Guanajuato:",
       subtitle = "clasificación por rangos de edad",
       caption = paste0("Total casos: ", sum(incid$counts))) +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 5),
                    labels = sprintf("%s (%s)", 
                                     c("  < 18", "18-25", "26-39", "40-59", ">= 60"),
                                     rev(percent(round(data_edades$per,5))))) +
  
  guides(fill = guide_legend(title = "Rangos de edad")) +
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label),
            size=3, color="white") +
  theme_void() # remove background, grid, numeric labels

pie_edades


sum_of_obs <- sum(def$counts)

# en defs_hm ponemos en un vector todos los conteosde 
# "Hombre" y "Mujer" encontrados en def$counts 
# donde la primera columna es de "Hombre" y la segunda de "Mujer"
defs_hm <- data.frame(
  sexo = c(rep("Hombre", sum(def$counts[,1])), 
           rep("Mujer", sum(def$counts[,2]))),
  value = 1:sum_of_obs
) 

defs_hm <- defs_hm %>% 
  group_by(sexo) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(sexo))

defs_hm$label <- scales::percent(defs_hm$per)

pie_defs_hm <- ggplot(data=defs_hm) +
  geom_bar(aes(x="", y=per, fill=sexo),
           stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(title = "Defunciones en el estado de Guanajuato:",
       subtitle = "clasificación de acuerdo al sexo",
       caption = paste0("Total casos: ", sum(incid$counts),
                        "\nTotal muertes: ", sum(def$counts))) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2)) +
  
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label),
            size=4, color="white") +
  theme_void() # remove background, grid, numeric labels

pie_defs_hm


defs_edades<- data.frame(
  edades = c(rep("A", sum(def2$counts[,1])),
             rep("B", sum(def2$counts[,2])),
             rep("C", sum(def2$counts[,3])),
             rep("D", sum(def2$counts[,4])),
             rep("E", sum(def2$counts[,5]))),
  value = 1:sum_of_obs
)
defs_edades <- defs_edades %>% 
  group_by(edades) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(edades))

defs_edades$label <- scales::percent(defs_edades$per)

pie_defs_edades <- ggplot(data=defs_edades) +
  geom_bar(aes(x="", y=per, fill=edades),
           stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  
  labs(title = "Defunciones en el estado de Guanajuato:",
       subtitle = "clasificación de acuerdo a grupos de edades",
       caption = paste0("Total casos: ", sum(incid$counts),
                        "\nTotal muertes: ", sum(def$counts))) +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 5),
                    labels = sprintf("%s (%s)", 
                                     c("  < 18", "18-25", "26-39", "40-59", ">= 60"),
                                     rev(percent(round(defs_edades$per,5))))) +
  guides(fill = guide_legend(title = "Rangos de edad")) +
  
  # geom_text(aes(x=1, y = cumsum(per) - per/2, label=label),
  #           size=4, color="white") +
  theme(plot.title    = element_text(family = "mono"),
        plot.subtitle = element_text(family = "sans"),
        text = element_text(family = "Times")) +
  theme_void()# remove background, grid, numeric labels

pie_defs_edades


# vec <- c(defs_edades$n/34)
# vec_names <- sprintf("%s (%s)", c(">= 60", "40-59", "26-39", "18-25", "  < 18"), 
#                      scales::percent(round(vec/sum(vec), 5)))
# names(vec) <- vec_names
# 
# waffle::waffle(vec)

# aa <- sum(df.GTO$RANGO=="A")/length(df.GTO$RANGO)
# bb <- sum(df.GTO$RANGO=="B")/length(df.GTO$RANGO)
# cc <- sum(df.GTO$RANGO=="C")/length(df.GTO$RANGO)
# dd <- sum(df.GTO$RANGO=="D")/length(df.GTO$RANGO)
# ee <- sum(df.GTO$RANGO=="E")/length(df.GTO$RANGO)




#---- CALCULAMOS LAS ESTADÍSTICAS ----

df.stats <- data.frame(
  MUNICIPIOS = municipios
)

df.epidem <- data.frame(
  MUNICIPIOS = municipios
)



#primero los indicadores epidemiológicos
n = length(municipios)
for (i in 1:n) {
  df.epidem$CASOS_ACTIV[i] <- get(paste0("incid.", municipios[i]))$ACTIV_14[i]
  df.epidem$TOTAL_CASOS[i] <- get(paste0("incid.", municipios[i]))$n
  df.epidem$TOTAL_DEF[i] <- get(paste0("defs_acum.", municipios[i]))$n
  df.epidem$PORC_DEF[i] <- percent(get(paste0("defs_acum.", municipios[i]))$n / get(paste0("acum.", municipios[i]))$n)
  df.epidem$TASA_INCID10MIL[i] <- last(get(paste0("incid.", municipios[i]))$counts) / habitantes[i] *10000 # al último día registrado
  df.epidem$TASA_MORT10MIL[i] <- last(get(paste0("defs.", municipios[i]))$counts) / habitantes[i] *10000 # al último día registrado
}

# primero incidencia mínima (mayor que 0), máxima y total de casos
n = length(municipios)
for (i in 1:n) {
  df.stats$MIN[i] <- min(get(paste0("incid.", municipios[i]))$counts[get(paste0("incid.", municipios[i]))$counts>0])
  df.stats$MAX[i] <- max(get(paste0("incid.", municipios[i]))$counts)
  df.stats$TOTAL_CASOS[i] <- get(paste0("incid.", municipios[i]))$n
}


# ahora moda(>0), media, mediana y cuantiles (0.25, .5, 0.75)
n = length(municipios)
for (i in 1:n) {
  df.stats$MODA[i] <- getmode(get(paste0("incid.", municipios[i]))$counts[get(paste0("incid.", municipios[i]))$counts>0])
  df.stats$MEDIA[i] <- mean(get(paste0("incid.", municipios[i]))$counts)
  df.stats$MEDIANA[i] <- median(get(paste0("incid.", municipios[i]))$counts)
  df.stats$CUANTIL_0.25[i] <- quantile(get(paste0("incid.", municipios[i]))$counts, 0.25)[[1]]
  df.stats$CUANTIL_0.50[i] <- quantile(get(paste0("incid.", municipios[i]))$counts, 0.50)[[1]]
  df.stats$CUANTIL_0.75[i] <- quantile(get(paste0("incid.", municipios[i]))$counts, 0.75)[[1]]
}

# > municipios
# [1] "GTO"   "Guanajuato" "León"       "Celaya"     "Silao"      "Irapuato"  
# [7] "Salamanca"

# finalmente rango, desviación estándar, varianza y coeficiente variacional
n = length(municipios)
for (i in 1:n) {
  df.stats$RANGO[i] <- paste0(range(get(paste0("incid.", municipios[i]))$counts)[1], " - ", range(get(paste0("incid.", municipios[i]))$counts)[2])
  df.stats$DESV_STD[i] <- sd(get(paste0("incid.", municipios[i]))$counts)
  df.stats$VAR[i] <- var(get(paste0("incid.", municipios[i]))$counts)
  df.stats$COEF_VAR[i] <- df.stats$DESV_STD[i] / df.stats$MEDIA[i]
}


#---- GUARDAMOS ESTADÍSTICAS E INDICADORES ----

#DescriptiveStats
write.csv(df.stats, "/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/DescriptiveStats.csv",
          row.names = FALSE)
#EpidemInd
write.csv(df.epidem, "/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/EpidemDescrip.csv",
          row.names = FALSE)




