# ----- INSTALAMOS PAQUETES Y LIBRERÍAS NECESARIOS -----
library(readr)
library(lubridate)
library(chron)
library(datetime)
library(data.table)

install.packages("ggplot2")
install.packages("dplyr")
install.packages("hrbrthemes")
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(gcookbook)
library(tidyverse)
library(devtools)

devtools::install_github("hrbrmstr/hrbrthemes")
remotes::install_github("const-ae/ggupset")

install.packages("dygraphs")
install.packages("lubridate")
install.packages("xts")
install.packages("tidyverse")

library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format

# ----- PRIMERO FIJAMOS EL DIRECTORIO EN EL QUE TRABAJAREMOS -----
setwd("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/0. GTO")

# ---- LEEMOS LOS .csv DE LOS DATOS CON LOS QUE TRABAJAREMOS ----
#primero leemos los nombres de los archivos
file_names<- list.files(pattern = ".csv")
#OJOOO revisar que la lista esté acomodada por orden ascendente de meses

#guardamos los archivos en un df gigante (lista de df)
df.MESES_GTO <- list()

n = length(file_names)
for (i in 1:n) {
  df.MESES_GTO[[i]] <- read.csv(file_names[i])
}



# ----- USANDO INCIDENCE PACKAGE ----
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

# ----- AHORA LO HAREMOS MANUALMENTE ----
# .....SEPAREMOS LOS DATOS DEL ESTADO POR CASOS SEGÚN CLASIFICACION_LAB ......
# CLASIFICACION_FINAL:  1 =	CASO DE COVID-19 CONFIRMADO POR ASOCIACIÓN CLÍNICA EPIDEMIOLÓGICA
#                       2 =	CASO DE COVID-19 CONFIRMADO POR COMITÉ DE  DICTAMINACIÓN
#                       3 =	CASO DE SARS-COV-2  CONFIRMADO
#                       4 = INVÁLIDO POR LABORATORIO	
#                       5 =	NO REALIZADO POR LABORATORIO
#                       6 = CASO SOSPECHOSO 	
#                       7 =	NEGATIVO A SARS-COV-2

#guardamos los archivos en un df gigante (lista de df) por meses
#por ejemplo df.POS_GTO[[3]] contiene los datos de los casos positivos hasta el mes 3 (marzo)
df.POS_GTO <- list() 
df.NEG_GTO <- list()
df.PEND_GTO <- list()
df.MORT_GTO <- list()

n = length(df.MESES_GTO)
for (i in 1:n) {
  #primero separamos los casos positivos y negativos (para poder graficar los negativos como ceros)
  df.POS_GTO[[i]] <- subset.data.frame(df.MESES_GTO[[i]], 
                                       (CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3 | CLASIFICACION_FINAL==7),
                                       select=c(FECHA_SINTOMAS,
                                                CLASIFICACION_FINAL,
                                                RESULTADO_LAB,
                                                RESULTADO_ANTIGENO), 
                                       drop=FALSE)
  #Cambiamos el identificador de CLASIFICACION_FINAL positivos 1,2,3 a 1 en el df de casos positivo
  df.POS_GTO[[i]]$CLASIFICACION_FINAL[df.POS_GTO[[i]]$CLASIFICACION_FINAL==2] <- 1
  df.POS_GTO[[i]]$CLASIFICACION_FINAL[df.POS_GTO[[i]]$CLASIFICACION_FINAL==3] <- 1
  #Y cambiamos el identificador de CLASIFICACION_FINAL negativo  7 a 0 en el df de casos positivos (por simplicidad)
  df.POS_GTO[[i]]$CLASIFICACION_FINAL[df.POS_GTO[[i]]$CLASIFICACION_FINAL==7] <- 0
  #ahora contamos los casos positivos por día
  #aggregate.data.frame guarda en un df ...
  #la suma (el total) de los casos positivos por FECHA_SINTOMAS
  df.POS_GTO[[i]] <- aggregate.data.frame(df.POS_GTO[[i]]["CLASIFICACION_FINAL"],
                                          by = df.POS_GTO[[i]]["FECHA_SINTOMAS"], sum,
                                          na.rm = TRUE, na.action("na.pass"))
  #finalmente añadimos una columna con los casos positivos acumulados por día
  df.POS_GTO[[i]]$POSITIVOS_ACUM <- cumsum(df.POS_GTO[[i]]$CLASIFICACION_FINAL)
  
  
  #lo mismo que lo anterior, pero para casos NEGATIVOS, PENDIENTES y MUERTOS
  df.NEG_GTO[[i]] <- subset.data.frame(df.MESES_GTO[[i]], 
                                       CLASIFICACION_FINAL==7,
                                       select=c(FECHA_SINTOMAS,
                                                CLASIFICACION_FINAL,
                                                RESULTADO_LAB,
                                                RESULTADO_ANTIGENO), 
                                       drop=FALSE)
  df.NEG_GTO[[i]] <- aggregate.data.frame(df.NEG_GTO[[i]]["CLASIFICACION_FINAL"],
                                          by = df.NEG_GTO[[i]]["FECHA_SINTOMAS"], sum,
                                          na.rm = TRUE, na.action("na.pass"))
  df.NEG_GTO[[i]]$NEGATIVOS_ACUM <- cumsum(df.NEG_GTO[[i]]$CLASIFICACION_FINAL)/7
  
  
  df.PEND_GTO[[i]] <- subset.data.frame(df.MESES_GTO[[i]],
                                        CLASIFICACION_FINAL==6,
                                        select=c(FECHA_SINTOMAS,
                                                 CLASIFICACION_FINAL,
                                                 RESULTADO_LAB,
                                                 RESULTADO_ANTIGENO),
                                        drop=FALSE)
  df.PEND_GTO[[i]] <- aggregate.data.frame(df.PEND_GTO[[i]]["CLASIFICACION_FINAL"],
                                           by = df.PEND_GTO[[i]]["FECHA_SINTOMAS"], sum,
                                           na.rm = TRUE, na.action("na.pass"))
  df.PEND_GTO[[i]]$PENDIENTES_ACUM <- cumsum(df.PEND_GTO[[i]]$CLASIFICACION_FINAL)/6
  
  df.MORT_GTO[[i]] <- subset.data.frame(df.MESES_GTO[[i]], 
                                        (CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3 | CLASIFICACION_FINAL==7),
                                        select=c(FECHA_SINTOMAS,
                                                 FECHA_INGRESO,
                                                 FECHA_DEF,
                                                 RESULTADO_LAB,
                                                 RESULTADO_ANTIGENO,
                                                 CLASIFICACION_FINAL,
                                                 SEXO, EDAD),
                                        drop=FALSE)
  df.MORT_GTO[[i]]$CLASIFICACION_FINAL[df.MORT_GTO[[i]]$CLASIFICACION_FINAL==2] <- 1
  df.MORT_GTO[[i]]$CLASIFICACION_FINAL[df.MORT_GTO[[i]]$CLASIFICACION_FINAL==3] <- 1
  df.MORT_GTO[[i]]$CLASIFICACION_FINAL[df.MORT_GTO[[i]]$CLASIFICACION_FINAL==7] <- 0
  
  
  df.MORT_GTO[[i]] <- aggregate.data.frame(df.MORT_GTO[[i]]["CLASIFICACION_FINAL"],
                                           by = df.MORT_GTO[[i]]["FECHA_DEF"], sum,
                                           na.rm = TRUE, na.action("na.pass"))
  
  df.MORT_GTO[[i]]$MUERTES_ACUM <- cumsum(df.MORT_GTO[[i]]$CLASIFICACION_FINAL)
}




# ----- GRAFICAMOS LA INCIDENCIA DEL ESTADO HASTA CADA MES DE INTERÉS -----
#Creamos vector de los meses que tenemos los datos en df.MESES_GTO
meses <- c("Enero 2020","Febrero 2020", "Marzo 2020",
           "Abril 2020", "Mayo 2020", "Junio 2020",
           "Julio 2020", "Agosto 2020", "Septiembre 2020",
           "Octubre 2020", "Noviembre 2020", "Diciembre 2020", 
           "Enero 2021", "Febrero 2021", "Marzo 2021",
           "Abril 2021", "Mayo 2021", "Junio 2021",
           "Julio 2021")

n=length(meses)
#iniciamos en 4 porque queremos graficar desde ABRIL
for (i in 4:n) {
  #creamos dos vectores; uno de fechas y otro de incidencia
  assign(paste0("fechas_",meses[i]), as.Date(df.POS_GTO[[i]]$FECHA_SINTOMAS,
                                             format = "%Y-%m-%d"))
  assign(paste0("incid_",meses[i]), df.POS_GTO[[i]]$CLASIFICACION_FINAL)
  
  #guardamos los datos anteriores en un df para usarlos en ggplot
  assign(paste0("datos_",meses[i]), data.frame(day = get(paste0("fechas_",meses[i])),
                                               value = get(paste0("incid_",meses[i]))))
  
  assign(paste0("fechas2_",meses[i]), as.Date(df.MORT_GTO[[i]]$FECHA_DEF,
                                              format = "%Y-%m-%d"))
  assign(paste0("incid2_",meses[i]), df.MORT_GTO[[i]]$CLASIFICACION_FINAL)
  
  #guardamos los datos anteriores en un df para usarlos en ggplot
  assign(paste0("datos2_",meses[i]), data.frame(day = get(paste0("fechas2_",meses[i])),
                                                value = get(paste0("incid2_",meses[i]))))
  
  assign(paste0("p_", meses[i]), 
         ggplot() +
           geom_line(data = get(paste0("datos_",meses[i])),
                     aes(x = day, y = value),
                     color="#69b3a2") +
           geom_point(data = get(paste0("datos_",meses[i])),
                      aes(x = day, y = value),
                      color="#69b3a2") +
           geom_line(data = get(paste0("datos2_",meses[i])),
                     aes(x = day, y = value),
                     color="#FD625E") +
           geom_point(data = get(paste0("datos2_",meses[i])),
                      aes(x = day, y = value),
                      color="#FD625E") +
           labs(x = "semanas", y = "incidencia",
                title = "Incidencia en el estado de Guanajuato",
                subtitle = paste0(meses[i]),
                caption = paste0("Total casos: ", max(df.POS_GTO[[i]]$POSITIVOS_ACUM),
                                 "\nIncidencia máxima: ", max(df.POS_GTO[[i]]$CLASIFICACION_FINAL),
                                 "\nDefunciones: ", max(df.MORT_GTO[[i]]$MUERTES_ACUM),
                                 "\nMáximo número de defunciones: ", max(df.MORT_GTO[[i]]$CLASIFICACION_FINAL))) +
           theme_ipsum() +
           theme(axis.text.x=element_text(angle=60, hjust=1),
                 legend.justification = c("right", "top")) +
           scale_x_date(limit=c(as.Date("2020-01-05"), max(get(paste0("fechas_",meses[i])))),
                        date_breaks = "7 days", 
                        date_labels = "%b%d") +
           ylim(0, max(get(paste0("incid_",meses[i])))))
  
  print(get(paste0("p_", meses[i])))
  
  #Guardamos las gráficas en formato .png
  ggsave(paste0("p-", i, ". ", meses[i], ".png"), width = 16, height = 9)
}






# ----- GRAFICAMOS LOS CASOS ACUMULADOS DEL ESTADO HASTA CADA MES DE INTERÉS -----
#Usamos el vector de los meses que habíamos creado anteriormente:
# meses <- c("Enero 2020","Febrero 2020", "Marzo 2020",
#            "Abril 2020", "Mayo 2020", "Junio 2020",
#            "Julio 2020", "Agosto 2020", "Septiembre 2020",
#            "Octubre 2020", "Noviembre 2020", "Diciembre 2020", 
#            "Enero 2021")

n=length(meses)
#iniciamos en 4 porque queremos graficar desde ABRIL
for (i in 4:n) {
  #creamos dos vectores; uno de fechas y otro de incidencia
  assign(paste0("fechas_",meses[i]), as.Date(df.POS_GTO[[i]]$FECHA_SINTOMAS,
                                             format = "%Y-%m-%d"))
  assign(paste0("acum_",meses[i]), df.POS_GTO[[i]]$POSITIVOS_ACUM)
  
  #guardamos los datos anteriores en un df para usarlos en ggplot
  assign(paste0("datos_",meses[i]), data.frame(day = get(paste0("fechas_",meses[i])),
                                               value = get(paste0("acum_",meses[i]))))
  
  assign(paste0("q_", meses[i]), 
         ggplot(get(paste0("datos_",meses[i])),
                aes(x = day, y = value)) +
           geom_line(color="#aedb9f") + 
           geom_point(color="#aedb9f") +
           labs(x = "semanas", y = "incidencia acumulada",
                title = "Casos positivos acumulados en el estado de Guanajuato",
                subtitle = paste0(meses[i]),
                caption = paste0("Total casos: ", max(df.POS_GTO[[i]]$POSITIVOS_ACUM),
                                 "\nIncidencia máxima: ", max(df.POS_GTO[[i]]$CLASIFICACION_FINAL),
                                 "\nDefunciones: ", max(df.MORT_GTO[[i]]$MUERTES_ACUM),
                                 "\nMáximo número de defunciones: ", max(df.MORT_GTO[[i]]$CLASIFICACION_FINAL))) +
           theme_ipsum() +
           theme(axis.text.x=element_text(angle=60, hjust=1)) +
           scale_x_date(limit=c(as.Date("2020-01-05"), max(get(paste0("fechas_",meses[i])))),
                        date_breaks = "7 days", 
                        date_labels = "%b%d") +
           ylim(0, max(get(paste0("acum_",meses[i])))))
  
  print(get(paste0("q_", meses[i])))
  
  #Guardamos las gráficas en formato .png
  ggsave(paste0("q-", i, ". ", meses[i], ".png"), width = 16, height = 9)
}







# ----- HAREMOS UNA GRÁFICA EN SERIE DE TIEMPO INTERACTIVA -----
Sys.setenv(LANG = "sp")

fechas <- as.Date(df.POS_GTO[[18]]$FECHA_SINTOMAS, format = "%Y-%m-%d", languageEl("sp"))
valores <- df.POS_GTO[[18]]$CLASIFICACION_FINAL
datos <- data.frame(day = fechas, value = valores)

Sys.setenv(LANG = "sp") #fija español como el idioma de R

don <- xts(x=datos$value, order.by = datos$day) #xts = extensible time series object
q <- dygraph(don, 
             main = "Incidencia en el estado de Guanajuato",
             ylab = "incidencia")%>%
  dySeries("V1", label = "Infectados GTO") %>%
  dyLegend(show = "follow", hideOnMouseOut = FALSE) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, 
            drawGrid = TRUE, colors="#69b3a2",
            drawPoints = TRUE, pointSize = 2) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "horizontal") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2, 
              hideOnMouseOut = TRUE)  %>%
  dyRoller(rollPeriod = 1)

rojo <- "#FFCCCC"
naranja <- "#FFE5CC"
amarillo <- "#FFFFCC"
amarilloA <- "#FFFF66"

q <- q %>%
  dyShading(from = "2020-06-01", to = "2020-08-03", color = rojo) %>%
  dyShading(from = "2020-08-04", to = "2020-10-03", color = naranja) %>%
  dyShading(from = "2020-10-04", to = "2020-11-09", color = amarillo) %>%
  dyShading(from = "2020-11-10", to = "2020-12-24", color = naranja) %>%
  dyShading(from = "2020-12-25", to = "2021-02-21", color = rojo) %>%
  dyShading(from = "2021-02-22", to = "2021-03-07", color = naranja) %>%
  dyShading(from = "2021-03-08", to = "2021-05-03", color = amarilloA) %>%
  dyShading(from = "2021-05-04", to = "2021-06-19", color = amarillo) 

q

# %>%
#   dyShading(from = "2020-08-20", to = "2021-10-14", color = amarilloA) %>%
#   dyShading(from = "2020-10-15", to = "2021-11-16", color = amarillo) %>%
#   dyShading(from = "2020-11-17", to = "2021-12-19", color = verde)    




#---- GRÁFICA INTERACTIVA CONJUNTA ----


Gua <- read.csv("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/1. Guanajuato/18-Guanajuato.csv")
Leo <- read.csv("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/2. León/18-León.csv")
Cya <- read.csv("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/3. Celaya/18-Celaya.csv")
Sil <- read.csv("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/4. Silao/18-Silao.csv")
Irap <- read.csv("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/5. Irapuato/18-Irapuato.csv")
Sal <- read.csv("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/6. Salamanca/18-Salamanca.csv")

datos1 <- data.frame(day = as.Date(Gua$FECHA_SINTOMAS, format = "%Y-%m-%d", languageEl("sp")),
                     value = Gua$CLASIFICACION_FINAL)
datos2 <- data.frame(day = as.Date(Leo$FECHA_SINTOMAS, format = "%Y-%m-%d", languageEl("sp")),
                     value = Leo$CLASIFICACION_FINAL)
datos3 <- data.frame(day = as.Date(Cya$FECHA_SINTOMAS, format = "%Y-%m-%d", languageEl("sp")),
                     value = Cya$CLASIFICACION_FINAL)
datos4 <- data.frame(day = as.Date(Sil$FECHA_SINTOMAS, format = "%Y-%m-%d", languageEl("sp")),
                     value = Sil$CLASIFICACION_FINAL)
datos5 <- data.frame(day = as.Date(Irap$FECHA_SINTOMAS, format = "%Y-%m-%d", languageEl("sp")),
                     value = Irap$CLASIFICACION_FINAL)
datos6 <- data.frame(day = as.Date(Sal$FECHA_SINTOMAS, format = "%Y-%m-%d", languageEl("sp")),
                     value = Sal$CLASIFICACION_FINAL)

for (i in 1:6) {
  assign(paste0("datos",i),
         subset.data.frame(get(paste0("datos",i)),
                           (value==1 | value==2 | value==3),
                           select=c(day, value)))
  aux <- get(paste0("datos",i))
  assign(paste0("datos",i),
         data.frame(day = incidence(aux$day)$dates,
                    value = incidence(aux$day)$counts))
}

rm(aux)


bind <- datos
bind$value1 <- 0
n = length(bind$day)
k = length(datos1$day)
for (i in 1:min(k,n)) {
  if(bind$day[n-i+1] == datos1$day[k-i+1])
    bind$value1[n-i+1] = datos1$value[k-i+1]
  else bind$value1[k-i+1] = 0
}

bind$value2 <- 0
n = length(bind$day)
k = length(datos2$day)
for (i in 1:min(k,n)) {
  if(bind$day[n-i+1] == datos2$day[k-i+1])
    bind$value2[n-i+1] = datos2$value[k-i+1]
  else bind$value2[k-i+1] = 0
}

bind$value3 <- 0
n = length(bind$day)
k = length(datos3$day)
for (i in 1:min(k,n)) {
  if(bind$day[n-i+1] == datos3$day[k-i+1])
    bind$value3[n-i+1] = datos3$value[k-i+1]
  else bind$value3[k-i+1] = 0
}

bind$value4 <- 0
n = length(bind$day)
k = length(datos4$day)
for (i in 1:min(k,n)) {
  if(bind$day[n-i-6] == datos4$day[k-i+1])
    bind$value4[n-i-6] = datos4$value[k-i+1]
  else bind$value4[k-6] = 0
}

bind$value5 <- 0
n = length(bind$day)
k = length(datos5$day)
for (i in 1:min(k,n)) {
  if(bind$day[n-i+1] == datos5$day[k-i+1])
    bind$value5[n-i+1] = datos5$value[k-i+1]
  else bind$value5[k-i+1] = 0
}

bind$value6 <- 0
n = length(bind$day)
k = length(datos6$day)
for (i in 1:min(k,n)) {
  if(bind$day[n-i+1] == datos6$day[k-i+1])
    bind$value6[n-i+1] = datos6$value[k-i+1]
  else bind$value6[k-i+1] = 0
}

# 
# aux <- aggregate.data.frame(as.list(datos1)["value1"],
#                      by = as.list(datos1)["day"], sum,
#                      na.rm = TRUE, na.action("na.pass"))



colores <-c("#808080",
            "#A66999", "#FE6DB6", "#FF944E",
            "#00B2D9", "#73B761", "#0050EB")

#colores <- RColorBrewer::brewer.pal(7, "Set2") #esta paleta también está bonita

don2 <- xts(x=bind[2:8], order.by = bind$day)

q2 <- dygraph(don2, 
             main = "Incidencia en el estado de Guanajuato",
             ylab = "incidencia") %>%
  dySeries("value", label = "Estado") %>%
  dySeries("value1", label = "Guanajuato") %>%
  dySeries("value2", label = "León") %>%
  dySeries("value3", label = "Celaya*") %>%
  dySeries("value4", label = "Silao") %>%
  dySeries("value5", label = "Irapuato") %>%
  dySeries("value6", label = "Salamanca")

q2 <- q2 %>%
  dyOptions(colors = colores) %>%
  dyLegend(width = 300)

q2 <- q2 %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "horizontal") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.6, 
              hideOnMouseOut = TRUE)  %>%
  dyRoller(rollPeriod = 1) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, 
            drawGrid = TRUE, colors="#69b3a2",
            drawPoints = TRUE, pointSize = 2)

q2

q2 <- q2 %>%
  dyShading(from = "2020-06-01", to = "2020-08-03", color = rojo) %>%
  dyShading(from = "2020-08-04", to = "2020-10-03", color = naranja) %>%
  dyShading(from = "2020-10-04", to = "2020-11-09", color = amarillo) %>%
  dyShading(from = "2020-11-10", to = "2020-12-24", color = naranja) %>%
  dyShading(from = "2020-12-25", to = "2021-02-21", color = rojo) %>%
  dyShading(from = "2021-02-22", to = "2021-03-07", color = naranja) %>%
  dyShading(from = "2021-03-08", to = "2021-05-03", color = amarilloA) %>%
  dyShading(from = "2021-05-04", to = "2021-06-19", color = amarillo) 

q2



# ----- NO CORRER HASTA ESTAR SEGUROS DE NO NECESITAR MÁS LOS DATOS -----
# rm(list=ls(pattern = "df"))

# rm(list=ls(pattern = "fechas"))
# rm(list=ls(pattern = "incid"))
# rm(list=ls(pattern = "datos"))

# rm(list=ls(pattern = "p"))
# rm(list=ls(pattern = "q"))

# rm(file_names, meses)
# rm(i, n)





# ----- EJEMPLO DE GRAFICAR INCIDENCIA DEL ESTADO HASTA ABRIL -----
# fechas_abr <- df.POS_GTO[[4]]$FECHA_SINTOMAS
# fechas_abr <- as.Date(fechas_abr, format = "%Y-%m-%d")
# incid_abr <- df.POS_GTO[[4]]$CLASIFICACION_FINAL
# 
# datos_abr <- data.frame(day=fechas_abr, value=incid_abr)
# max_abr <- max(df.POS_GTO[[4]]$POSITIVOS_ACUM)
# 
# abr <- ggplot(datos_abr, aes(x=day, y=value)) +
#   geom_line(color="#69b3a2") + 
#   geom_point(color="#69b3a2") +
#   labs(x="semanas", y="incidencia",
#        title="Incidencia en el estado de Guanajuato",
#        subtitle="Hasta el final de Abril",
#        caption=paste0("Total casos: ", max(df.POS_GTO[[4]]$POSITIVOS_ACUM),
#                       "\nIncidencia máxima", max(df.POS_GTO[[4]]$CLASIFICACION_FINAL))) +
#   theme_ipsum() +
#   theme(axis.text.x=element_text(angle=60, hjust=1)) +
#   scale_x_date(limit=c(as.Date("2020-01-05"), max(fechas_abr)),
#                date_breaks = "7 days",
#                date_labels = "%b%d") +
#   ylim(0,max(incid_abr))
# 
# abr
