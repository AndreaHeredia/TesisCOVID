#---- USANDO LIBRERÍAS Y PAQUETES DE DIEGO VALLE ----
library(inegiR)

#Descargamos los paquetes
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("diegovalle/mxmaps", force=TRUE)
#recordar dar enter y no dar número cuando se esté instalando

#descargamos la librería de DiegoValle
library(mxmaps)


# GRÁFICA 1 DE POBLACIÓN DEL PAÍS CON LOS ESTADOS -.-.-.-.-.-.-
data("df_mxstate_2020")

#definimos la columna value
df_mxstate_2020$value <- df_mxstate_2020$pop #agregamos la columna "value" que contiene la población "pop"


#el estado 11 corresponde a GTO

mxstate_choropleth(df_mxstate_2020,
                   title = "Total population, by state") 


# GRÁFICA 2 DE POBLACIÓN DEL PAÍS CON LOS MUNICIPIOS -.-.-.-.-.-.-
data("df_mxmunicipio_2020")

#definimos la columna value
df_mxmunicipio_2020$value <-  df_mxmunicipio_2020$indigenous_language / 
  df_mxmunicipio_2020$pop * 100

mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,
                       title = "Percentage of the population that speaks\nan indigenous language",
                       legend = "%")

# GRÁFICA 3 DE POBLACIÓN DEL PAÍS CON LOS ESTADOS -.-.-.-.-.-.-
mxmunicipio_choropleth(df_mxmunicipio_2020, num_colors = 1,
                       zoom = subset(df_mxmunicipio_2020, metro_area %in% 
                                       c("Valle de México",
                                         "Puebla-Tlaxcala",
                                         "Cuernavaca",
                                         "Toluca"))$region,
                       title = "Percentage of the population that speaks\nan indigenous language",
                       legend = "%") 


# GRÁFICA 4 DE MAPA CON MUNICIPIOS AGRUPADOS POR CATEGORÍAS -.-.-.-.-.-.-
#mapa con municipios separados según las categorías dadas

#definiendo una nueva columna "value" con las categorías especificadas
df_mxmunicipio_2020$value <- as.factor(sample(c(NA, letters[1:6]),
                                              nrow(df_mxmunicipio_2020),
                                              replace = TRUE) )



gg = MXMunicipioChoropleth$new(df_mxmunicipio_2020)
gg$title <- "Municipios a-f"
gg$set_num_colors(6)
gg$set_zoom(subset(df_mxmunicipio_2020, state_name %in% c("Ciudad de México",                                                                     "México"))$region)
gg$ggplot_scale <- scale_fill_brewer("type", type = "qual", palette = 2,
                                     na.value = "white")
p <- gg$render()
p + theme_void()



# -.-.-.-.-.- MIS INTENTOS  -.-.-.-.-.-.-




# GRÁFICA 3 -.-.-.-.-.-.-
mxmunicipio_choropleth(df_mxmunicipio_2020, num_colors = 1,
                       zoom = subset(df_mxmunicipio_2020, state_name %in% 
                                       c("Guanajuato"))$region,
                       title = "Percentage of the population that speaks\nan indigenous language in Guanajuato",
                       show_states = FALSE,
                       legend = "%")


# GRÁFICA 1 -.-.-.-.-.-.- con zoom
mxstate_choropleth(df_mxstate_2020, num_colors = 1,
                   zoom = subset(df_mxstate_2020, state_name %in% 
                                   c("Guanajuato"))$region,
                   title = "Primer intento",
                   legend = "population")

#casos de COVID
# Estado = 132871 -.-.-.-. state_code==11
# (015) Guanajuato = 5227
# (020) León = 44363 
# (007*) Celaya* = 17405 #Cortazar + Villagrán + Celaya + Salvatierra = 011 + 044 + 007 + 028
# (037) Silao = 3496
# (017) Irapuato = 13572
# (027) Salamanca = 6690

# GRÁFICA 3 -.-.-.-.-.-.- redefiniendo value y creando un subconjunto 
sub.GTO <- subset.data.frame(df_mxmunicipio_2020,
                             df_mxmunicipio_2020$state_code==11)
sub.GTO$value <- NA

sub.GTO$value[20] = 44363 #León
sub.GTO$value[37] = 3496 #Silao
sub.GTO$value[15] = 5227 #Guanajuato
sub.GTO$value[17] = 13572 #Irapuato
sub.GTO$value[27] = 6690 #Salamanca
sub.GTO$value[7] = 17405 #Celaya

sub.GTO$value[11] = 17405 #Cortazar
sub.GTO$value[44] = 17405 #Villagrán
sub.GTO$value[28] = 17405 #Salvatierra

mxmunicipio_choropleth(sub.GTO, num_colors = 1,
                       title = "Primer intento COVID
                       \n Total de casos acumulados: 132 871" ,
                       zoom = subset(sub.GTO, state_name %in% 
                                       c("Guanajuato"))$region,
                       show_states = FALSE,
                       legend = "incidencia acumulada") #cambiar los NA por 0's



# GRÁFICA 4 -.-.-.-.-.-.- usando el subconjunto de datos 

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()


cc <- MXMunicipioChoropleth$new(df_mxmunicipio_2020)

cc$title <- "Casos COVID-19 Guanajuato"
cc$set_num_colors(7)
cc$set_zoom(subset(df_mxmunicipio_2020, state_name %in% c("Guanajuato"))$region)
cc$ggplot_scale <- scale_fill_brewer("type", type = "qual", palette = "RdPu",
                                     na.value = "black")
p <- cc$render()
p + theme_void()


###---- ÉSTA ES LA CHIDAAAAAA!!!!!!!!!!!! ---------------
# GRÁFICA 4 -.-.-.-.-.-.- cambiando color de "na.values"

bb <- MXMunicipioChoropleth$new(sub.GTO)

bb$title <- "Casos COVID-19 Guanajuato"
bb$set_num_colors(9)
bb$set_zoom(subset(sub.GTO, state_name %in% c("Guanajuato"))$region)
bb$ggplot_scale <- scale_fill_brewer("incidencia acumulada", type = "qual", 
                                     palette = "RdPu",
                                     na.value = "white")
p <- bb$render()
p + theme_void()

#para añadir etiquetas de nombres de municipios
library("ggrepel")


labels <- sub.GTO
labels$group <- NA
labels <- subset(labels, municipio_name %in% c("Guanajuato",
                                               "León", "Celaya", 
                                               "Silao de la Victoria", 
                                               "Irapuato",
                                               "Salamanca",
                                               "Villagrán",
                                               "Cortazar",
                                               "Salvatierra"))
p <- p + geom_text_repel(data = labels, 
                  aes(long, lat, label = municipio_name), 
                  nudge_x = .3, 
                  nudge_y = .2) +
  geom_point(data = labels, 
             aes(long, lat), 
             color = "#d6604d",
             size = 1)



# GRÁFICA 1 -.-.-.-.-.-.- intentamos graficar GTO
#definimos la columna value
df_mxstate_2020$value <- NA
df_mxstate_2020$value[11] <- df_mxstate_2020$pop[11]


mxstate_choropleth(df_mxstate_2020,
                   title = "Total population, by state") 


aa <- MXStateChoropleth$new(df_mxstate_2020)#introduzco los datos que quiero graficar

aa$title <- "Guanajuato"
aa$set_num_colors(1)
aa$ggplot_scale <- scale_fill_brewer("Población total", type = "qual", 
                                     palette = "Accent",
                                     na.value = "white")
q <- aa$render()
q + theme_void()



#---- SEGUNDA OPCIÓN PARA GRAFICAR PAÍS O ESTADO(S) SELECCIONADO(S) ----
library(inegiR)
library(maptools)
library(ggplot2)

#obtenemos los datos geográficos de México
data(mxstate.map)

#mapa de todo el país con división territorial por estados
ggplot(mxstate.map, aes(long, lat, group=group)) +
  geom_polygon(fill = "white", color = "black", size = .2) +
  coord_map()

#11 = Guanajuato
#16 = Michoacán
#19 = Nuevo León

#Seleccionamos la región == 11 que corresponde a Guanajuato
sub <- subset.data.frame(mxstate.map, mxstate.map$region==11)

#Graficamos la región seleccionada
ggplot(sub, aes(long, lat, group=group)) +
  geom_polygon(fill = "white", color = "black", size = .2) +
  coord_map()


