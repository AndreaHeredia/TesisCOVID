# ----- INSTALAMOS PAQUETES Y LIBRERÍAS NECESARIOS -----
install.packages("ggplot2")
library(ggplot2)


library(readr)
library(lubridate)
library(chron)
library(datetime)
library(data.table)


install.packages("dplyr")
install.packages("hrbrthemes")

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


municipios <- c("GTO", 
                "Guanajuato", "León", "Celaya",
                "Silao", "Irapuato", "Salamanca")

# ----- PRIMERO FIJAMOS EL DIRECTORIO EN EL QUE TRABAJAREMOS -----
m = length(municipios)
for (j in 1:m) {
  dir <- paste0("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/", j-1,
                ". ", municipios[j])
  setwd(dir)
  
  # ---- LEEMOS LOS .csv DE LOS DATOS CON LOS QUE TRABAJAREMOS ----
  #primero leemos los nombres de los archivos
  file_names <- list.files(pattern = ".csv")
  #OJOOO revisar que la lista esté acomodada por orden ascendente de meses
  
  #guardamos los archivos en un df gigante (lista de df)
  assign(paste0("df.MESES_", municipios[j]), list())
  
  n = length(file_names)
  for (i in 1:n) {
    .GlobalEnv[[ paste0("df.MESES_", municipios[j]) ]][[i]] <- read.csv(file_names[i])
  }
  
  print(j)
}

#este ejemplo nos ayuda a manipular listas de df usando ".GlobalEnv"
# x_1 <- list()
# for (i in 1:m) {
#   #eval(parse(text=sprintf('x_1[[%s]] <- %s', i, 123)))
#   .GlobalEnv[[paste0("x_",1)]][[i]] <- c(6, i )
# }
# 
# eval(parse(text=paste0("x_1[[1]] <-", 123)))




# ----- SEPAREMOS LOS DATOS DEL ESTADO POR CASOS SEGÚN CLASIFICACION_LAB -----
# CLASIFICACION_FINAL:  1 =	CASO DE COVID-19 CONFIRMADO POR ASOCIACIÓN CLÍNICA EPIDEMIOLÓGICA
#                       2 =	CASO DE COVID-19 CONFIRMADO POR COMITÉ DE  DICTAMINACIÓN
#                       3 =	CASO DE SARS-COV-2  CONFIRMADO
#                       4 = INVÁLIDO POR LABORATORIO	
#                       5 =	NO REALIZADO POR LABORATORIO
#                       6 = CASO SOSPECHOSO 	
#                       7 =	NEGATIVO A SARS-COV-2

#guardamos los archivos en un df gigante (lista de df) por meses
#por ejemplo df.POS_Salamanca[[3]] contiene los datos de los casos positivos hasta el mes 3 (marzo)
m = length(municipios)
for (j in 1:m) {
  assign(paste0("df.POS_", municipios[j]), list())
  assign(paste0("df.NEG_", municipios[j]), list())
  #assign(paste0("df.PEND_", municipios[j]), list())
  assign(paste0("df.MORT_", municipios[j]), list())
}


for (j in 1:m) {
  n = length(get(paste0("df.MESES_", municipios[j])))
  for (i in 1:n) {
    .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]] <- subset.data.frame(get(paste0("df.MESES_", municipios[j]))[[i]], 
                                                                               (CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3 | CLASIFICACION_FINAL==7),
                                                                               select=c(FECHA_SINTOMAS, FECHA_DEF,
                                                                                        CLASIFICACION_FINAL,
                                                                                        RESULTADO_LAB,
                                                                                        RESULTADO_ANTIGENO), 
                                                                               drop=FALSE)
    
    #Cambiamos el identificador de CLASIFICACION_FINAL positivos 1,2,3 a 1 en el df de casos positivo
    #Y cambiamos el identificador de CLASIFICACION_FINAL negativo  7 a 0 en el df de casos positivos (por simplicidad)
    .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL <- replace(.GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL, 
                                                                                         .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL==2, 1)
    
    .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL <- replace(.GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL, 
                                                                                         .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL==3, 1)
    
    .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL <- replace(.GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL, 
                                                                                         .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL==7, 0)
    
    #Aquí incluímos la lista de negativos y muertos, para más adelante intercambiar ceros y unos.
    .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]] <- .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]
    .GlobalEnv[[ paste0("df.MORT_", municipios[j]) ]][[i]] <- .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]
    
    
    #ahora contamos los casos positivos por día
    #aggregate.data.frame guarda en un df la suma (el total) de los casos positivos por FECHA_SINTOMAS
    .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]] <- aggregate.data.frame(get(paste0("df.POS_", municipios[j]))[[i]]["CLASIFICACION_FINAL"],
                                                                                  by = get(paste0("df.POS_", municipios[j]))[[i]]["FECHA_SINTOMAS"],
                                                                                  sum,
                                                                                  na.rm = TRUE, na.action("na.pass"))
    
    #finalmente añadimos una columna con los casos positivos acumulados por día
    .GlobalEnv[[ paste0("df.POS_", municipios[j]) ]][[i]]$POSITIVOS_ACUM <- cumsum(get(paste0("df.POS_", municipios[j]))[[i]]$"CLASIFICACION_FINAL")
    
    
    #lo mismo que lo anterior, pero para casos NEGATIVOS, PENDIENTES y MUERTOS
    #Aquí cambiamos los índices 1 a 0 y 0 a 1, de la lista de positivos
    .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL <- replace(.GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL, 
                                                                                         .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL==0, 2)
    .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL <- replace(.GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL, 
                                                                                         .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL==1, 0)
    .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL <- replace(.GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL, 
                                                                                         .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$CLASIFICACION_FINAL==2, 1)
    
    .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]] <- aggregate.data.frame(get(paste0("df.NEG_", municipios[j]))[[i]]["CLASIFICACION_FINAL"],
                                                                                  by = get(paste0("df.NEG_", municipios[j]))[[i]]["FECHA_SINTOMAS"],
                                                                                  sum,
                                                                                  na.rm = TRUE, na.action("na.pass"))
    
    #finalmente añadimos una columna con los casos negativos acumulados por día
    .GlobalEnv[[ paste0("df.NEG_", municipios[j]) ]][[i]]$NEGATIVOS_ACUM <- cumsum(get(paste0("df.NEG_", municipios[j]))[[i]]$"CLASIFICACION_FINAL")
    
    
    .GlobalEnv[[ paste0("df.MORT_", municipios[j]) ]][[i]] <- aggregate.data.frame(get(paste0("df.MORT_", municipios[j]))[[i]]["CLASIFICACION_FINAL"],
                                                                                   by = get(paste0("df.MORT_", municipios[j]))[[i]]["FECHA_DEF"],
                                                                                   sum,
                                                                                   na.rm = TRUE, na.action("na.pass"))
    
    #finalmente añadimos una columna con los casos positivos acumulados por día
    .GlobalEnv[[ paste0("df.MORT_", municipios[j]) ]][[i]]$MUERTES_ACUM <- cumsum(get(paste0("df.MORT_", municipios[j]))[[i]]$"CLASIFICACION_FINAL")
    
  }
}



# ----- GRAFICAMOS LA INCIDENCIA DEL ESTADO HASTA CADA MES DE INTERÉS -----
#Creamos vector de los meses que tenemos los datos en df.MESES_León
meses <- c("Enero 2020","Febrero 2020", "Marzo 2020",
           "Abril 2020", "Mayo 2020", "Junio 2020",
           "Julio 2020", "Agosto 2020", "Septiembre 2020",
           "Octubre 2020", "Noviembre 2020", "Diciembre 2020", 
           "Enero 2021", "Febrero 2021", "Marzo 2021",
           "Abril 2021", "Mayo 2021", "Junio 2021")

# gris, morado, rosa, naranja, azul claro, verde claro, azul obscuro
colores <- c("GTO"="#787878",
             "Guanajuato"="#A66999", "León"="#FE6DB6", "Celaya"="#FF944E", 
             "Silao"="#00B2D9", "Irapuato"="#73B761", "Salamanca"="#0050EB")

m=length(municipios)
for (j in 1:m) {
  dir <- paste0("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/", j-1,
                ". ", municipios[j])
  setwd(dir)
  
  
  n=length(meses)
  #iniciamos en 4 porque queremos graficar desde ABRIL
  for (i in 4:n) {
    #creamos dos vectores; uno de fechas y otro de incidencia
    assign(paste0("fechas.", municipios[j], "_", meses[i]), 
           as.Date(get(paste0("df.POS_", municipios[j]))[[i]]$FECHA_SINTOMAS, 
                   format = "%Y-%m-%d"))
    assign(paste0("incid.", municipios[j], "_", meses[i]), 
           get(paste0("df.POS_", municipios[j]))[[i]]$CLASIFICACION_FINAL)
    
    #guardamos los datos anteriores en un df para usarlos en ggplot
    assign(paste0("datos.", municipios[j], "_", meses[i]), 
           data.frame(day = get(paste0("fechas.", municipios[j], "_", meses[i])),
                      value = get(paste0("incid.", municipios[j], "_", meses[i]))))
    
    
    #comenzamos el ggplot
    assign(paste0("p.", municipios[j], "_", meses[i]), 
           ggplot() +
             geom_line(data = get(paste0("datos.", municipios[j], "_", meses[i])),
                       aes(x = day, y = value),
                       color = colores[[j]]) +
             geom_point(data = get(paste0("datos.", municipios[j], "_", meses[i])),
                        aes(x = day, y = value), 
                        color = colores[[j]]) +
             labs(x = "semanas", y = "incidencia",
                  color = " ",
                  title = paste0("Incidencia en el municipio de ", municipios[j]),
                  subtitle = paste0(meses[i]),
                  caption = paste0("Total casos: ", max(get(paste0("df.POS_", municipios[j]))[[i]]$POSITIVOS_ACUM),
                                   "\nIncidencia máxima: ", max(get(paste0("df.POS_", municipios[j]))[[i]]$CLASIFICACION_FINAL),
                                   "\nDefunciones: ", max(get(paste0("df.MORT_", municipios[j]))[[i]]$MUERTES_ACUM),
                                   "\nMáximo número de defunciones: ", max(get(paste0("df.MORT_", municipios[j]))[[i]]$MUERTES_ACUM))) +
             theme_ipsum() +
             theme(axis.text.x=element_text(angle=60, hjust=1),
                   legend.justification = c("right", "top")) +
             scale_x_date(limit=c(as.Date("2020-01-05"), 
                                  as.Date(max(get(paste0("fechas.", municipios[j], "_", meses[i]))))),
                          date_breaks = "7 days", 
                          date_labels = "%b%d") +
             ylim(0, max(get(paste0("incid.", municipios[j], "_", meses[i])))) +
             scale_color_manual(values = colores)
    )
    
    print(get(paste0("p.", municipios[j], "_", meses[i])))
    
    #Guardamos las gráficas en formato .jpg
    ggsave(paste0("p", i, ".",municipios[j], "_", meses[i], ".jpg"), width = 16, height = 9)
    
  }
  
}






#---------------------------------......---------------------------------.
#----- GRÁFICA DE TODOS LOS MUNICIPIOS Y EL ESTADO -----
dir <- paste0("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021")
setwd(dir)

colores <- c("Guanajuato estado"="#808080",
             "Guanajuato"="#A66999", "León"="#FE6DB6", "Celaya"="#FF944E", 
             "Silao"="#00B2D9", "Irapuato"="#73B761", "Salamanca"="#0050EB")

#Una vez que tenemos todos los municipios y el estado, aquí graficamos...
#todas las gráficas de incidencia traslapadas
i=18
for (i in 18:n) {
  #creamos dos vectores; uno de fechas y otro de incidencia
  assign(paste0("fechas_",meses[i]), as.Date(df.POS_GTO[[i]]$FECHA_SINTOMAS,
                                             format = "%Y-%m-%d"))
  assign(paste0("incid_",meses[i]), df.POS_GTO[[i]]$CLASIFICACION_FINAL)
  
  #guardamos los datos anteriores en un df para usarlos en ggplot
  assign(paste0("datos_",meses[i]), data.frame(day = get(paste0("fechas_",meses[i])),
                                               value = get(paste0("incid_",meses[i]))))
  
  
  assign(paste0("fechas1_",meses[i]), as.Date(df.POS_Guanajuato[[i]]$FECHA_SINTOMAS,
                                              format = "%Y-%m-%d"))
  assign(paste0("incid1_",meses[i]), df.POS_Guanajuato[[i]]$CLASIFICACION_FINAL)
  assign(paste0("datos1_",meses[i]), data.frame(day = get(paste0("fechas1_",meses[i])),
                                                value = get(paste0("incid1_",meses[i]))))
  
  assign(paste0("fechas2_",meses[i]), as.Date(df.POS_León[[i]]$FECHA_SINTOMAS,
                                              format = "%Y-%m-%d"))
  assign(paste0("incid2_",meses[i]), df.POS_León[[i]]$CLASIFICACION_FINAL)
  assign(paste0("datos2_",meses[i]), data.frame(day = get(paste0("fechas2_",meses[i])),
                                                value = get(paste0("incid2_",meses[i]))))
  
  assign(paste0("fechas3_",meses[i]), as.Date(df.POS_Celaya[[i]]$FECHA_SINTOMAS,
                                              format = "%Y-%m-%d"))
  assign(paste0("incid3_",meses[i]), df.POS_Celaya[[i]]$CLASIFICACION_FINAL)
  assign(paste0("datos3_",meses[i]), data.frame(day = get(paste0("fechas3_",meses[i])),
                                                value = get(paste0("incid3_",meses[i]))))
  
  assign(paste0("fechas4_",meses[i]), as.Date(df.POS_Silao[[i]]$FECHA_SINTOMAS,
                                              format = "%Y-%m-%d"))
  assign(paste0("incid4_",meses[i]), df.POS_Silao[[i]]$CLASIFICACION_FINAL)
  assign(paste0("datos4_",meses[i]), data.frame(day = get(paste0("fechas4_",meses[i])),
                                                value = get(paste0("incid4_",meses[i]))))
  
  assign(paste0("fechas5_",meses[i]), as.Date(df.POS_Irapuato[[i]]$FECHA_SINTOMAS,
                                              format = "%Y-%m-%d"))
  assign(paste0("incid5_",meses[i]), df.POS_Irapuato[[i]]$CLASIFICACION_FINAL)
  assign(paste0("datos5_",meses[i]), data.frame(day = get(paste0("fechas5_",meses[i])),
                                                value = get(paste0("incid5_",meses[i]))))
  
  assign(paste0("fechas6_",meses[i]), as.Date(df.POS_Salamanca[[i]]$FECHA_SINTOMAS,
                                              format = "%Y-%m-%d"))
  assign(paste0("incid6_",meses[i]), df.POS_Salamanca[[i]]$CLASIFICACION_FINAL)
  assign(paste0("datos6_",meses[i]), data.frame(day = get(paste0("fechas6_",meses[i])),
                                                value = get(paste0("incid6_",meses[i]))))
  
  p_TOTAL <- ggplot() +
    geom_line(data = get(paste0("datos_",meses[i])),
              aes(x = day, y = value, group = 1, color = "Estado")) +
    geom_point(data = get(paste0("datos_",meses[i])),
               aes(x = day, y = value, group = 1, color = "Estado")) +
    labs(x = "semanas", y = "incidencia",
         color = " ",
         title = "Incidencia en Guanajuato",
         subtitle = paste0(meses[i]),
         caption = paste0("\nTotal casos: ", max(df.POS_GTO[[i]]$POSITIVOS_ACUM),
                          "\nIncidencia máxima: ", max(df.POS_GTO[[i]]$CLASIFICACION_FINAL),
                          "\nDefunciones: ", max(df.MORT_GTO[[i]]$MUERTES_ACUM),
                          "\nMáximo número de defunciones: ", max(df.MORT_GTO[[i]]$CLASIFICACION_FINAL))) +
    theme_ipsum() +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          legend.justification = c("right", "top")) +
    scale_x_date(limit=c(as.Date("2020-01-05"), max(get(paste0("fechas_",meses[i])))),
                 date_breaks = "7 days", 
                 date_labels = "%b%d") +
    ylim(0, max(get(paste0("incid_",meses[i]))))
  
  
  
  p_TOTAL <- p_TOTAL +
    geom_line(data = get(paste0("datos1_",meses[i])),
              aes(x = day, y = value, group=1, color=municipios[2])) +
    geom_point(data = get(paste0("datos1_",meses[i])),
               aes(x = day, y = value, group=1, color=municipios[2])) +
    geom_line(data = get(paste0("datos2_",meses[i])),
              aes(x = day, y = value, group=1, color=municipios[3])) +
    geom_point(data = get(paste0("datos2_",meses[i])),
               aes(x = day, y = value, group=1, color=municipios[3])) +
    geom_line(data = get(paste0("datos3_",meses[i])),
              aes(x = day, y = value, group=1, color=municipios[4])) +
    geom_point(data = get(paste0("datos3_",meses[i])),
               aes(x = day, y = value, group=1, color=municipios[4])) +
    geom_line(data = get(paste0("datos4_",meses[i])),
              aes(x = day, y = value, group=1, color=municipios[5])) +
    geom_point(data = get(paste0("datos4_",meses[i])),
               aes(x = day, y = value, group=1, color=municipios[5])) +
    geom_line(data = get(paste0("datos5_",meses[i])),
              aes(x = day, y = value, group=1, color=municipios[6])) +
    geom_point(data = get(paste0("datos5_",meses[i])),
               aes(x = day, y = value, group=1, color=municipios[6])) +
    geom_line(data = get(paste0("datos6_",meses[i])),
              aes(x = day, y = value, group=1, color=municipios[7])) +
    geom_point(data = get(paste0("datos6_",meses[i])),
               aes(x = day, y = value, group=1, color=municipios[7])) +
    scale_color_manual(values = colores) #esta función cambia los colores de las acotaciones
  
  print(p_TOTAL)
  
  #Guardamos las gráficas en formato .jpg
  ggsave(paste0("p_TOTAL.jpg"), width = 16, height = 9)
}



#---------------------------------......---------------------------------.






# ----- GRAFICAMOS LOS CASOS ACUMULADOS DEL ESTADO HASTA CADA MES DE INTERÉS -----


#falta optimizar este código



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
  assign(paste0("fechas_",meses[i]), as.Date(df.POS_Salamanca[[i]]$FECHA_SINTOMAS,
                                             format = "%Y-%m-%d"))
  assign(paste0("acum_",meses[i]), df.POS_Salamanca[[i]]$POSITIVOS_ACUM)
  
  #guardamos los datos anteriores en un df para usarlos en ggplot
  assign(paste0("datos_",meses[i]), data.frame(day = get(paste0("fechas_",meses[i])),
                                               value = get(paste0("acum_",meses[i]))))
  
  assign(paste0("q_", meses[i]), 
         ggplot() +
           geom_line(data = get(paste0("datos_",meses[i])),
                     aes(x = day, y = value),
                     color="#aedb9f") +
           geom_point(data = get(paste0("datos_",meses[i])),
                      aes(x = day, y = value),
                      color="#aedb9f") +
           labs(x = "semanas", y = "incidencia acumulada",
                title = "Incidencia acumulada en el municipio de Salamanca",
                subtitle = paste0(meses[i]),
                caption = paste0("Total casos: ", max(df.POS_Salamanca[[i]]$POSITIVOS_ACUM),
                                 "\nIncidencia máxima: ", max(df.POS_Salamanca[[i]]$CLASIFICACION_FINAL),
                                 "\nDefunciones: ", max(df.MORT_Salamanca[[i]]$MUERTES_ACUM),
                                 "\nMáximo número de defunciones: ", max(df.MORT_Salamanca[[i]]$CLASIFICACION_FINAL))) +
           theme_ipsum() +
           theme(axis.text.x=element_text(angle=60, hjust=1),
                 legend.justification = c("right", "top")) +
           scale_x_date(limit=c(as.Date("2020-01-05"), max(get(paste0("fechas_",meses[i])))),
                        date_breaks = "7 days", 
                        date_labels = "%b%d") +
           ylim(0, max(get(paste0("acum_",meses[i])))))
  
  print(get(paste0("q_", meses[i])))
  
  #Guardamos las gráficas en formato .png
  ggsave(paste0("q-", i, ". ", meses[i], ".png"), width = 16, height = 9)
}






# ----- NO CORRER HASTA ESTAR SEGUROS DE NO NECESITAR MÁS LOS DATOS -----
# rm(list=ls(pattern = "df"))
# 
# rm(list=ls(pattern = "fechas"))
# rm(list=ls(pattern = "incid"))
# rm(list=ls(pattern = "acum"))
# rm(list=ls(pattern = "datos"))
# 
# rm(list=ls(pattern = "p"))
# rm(list=ls(pattern = "q"))
# 
# rm(file_names, meses)
# rm(i, n)




