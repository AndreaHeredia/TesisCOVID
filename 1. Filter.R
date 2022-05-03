# ----- INSTALAMOS PAQUETES Y LIBRERÍAS NECESARIOS -----
library(readr)
library(lubridate)
library(chron)
library(datetime)
library(data.table)


# ----- PRIMERO FIJAMOS EL DIRECTORIO EN EL QUE TRABAJAREMOS -----
setwd("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021")



# ----- IMPORTAMOS LA BASE DE DATOS (.csv) DE LA ÚLTIMA ACTUALIZACIÓN -----
#df = data frame "=" base de datos
df.30jun2021 <- read_csv("210629COVID19MEXICO.csv", 
                         col_types = cols(FECHA_ACTUALIZACION = col_date(format = "%Y-%m-%d"), 
                                          ENTIDAD_UM = col_double(), ENTIDAD_NAC = col_double(), 
                                          ENTIDAD_RES = col_double(), MUNICIPIO_RES = col_double(), 
                                          FECHA_INGRESO = col_date(format = "%Y-%m-%d"), 
                                          FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"), 
                                          FECHA_DEF = col_date(format = "%Y-%m-%d"),
                                          TIPO_PACIENTE = col_double(),
                                          INTUBADO = col_double(),
                                          UCI = col_logical()))

# ----- FILTRAMOS LOS DATOS QUE NOS INTERESAN DEL ESTADO DEL ARCHIVO ANTERIOR -----
# ENTIDAD_RES==11      #porque Gto es la entidad 11
# FECHA_INGRESO
# FECHA_SINTOMAS
# FECHA_DEF:  NA=no ha muerto
# ID_REGISTRO
# RESULTADO_LAB:  1 =	POSITIVO A SARS-COV-2
#                 2 =	NO POSITIVO A SARS-COV-2 
#                 3 =	RESULTADO PENDIENTE
#                 4 =  RESULTADO NO ADECUADO 	
#                 97 =	NO APLICA (CASO SIN MUESTRA)
# RESULTADO_ANTÍGENO: 1 =	POSITIVO A SARS-COV-2
#                     2 =	NO POSITIVO A SARS-COV-2
#                     97 =	NO APLICA (CASO SIN MUESTRA)
# MUNICIPIO_RES
# TIPO_PACIENTE: 1 = AMBULATORIO
#                2 = HOSPITALIZADO
#               99 = NO-ESPECIFICADO
# SEXO: 1=mujer, 2=hombre
# EDAD
# TOMA_MUESTRA
# CLASIFICACION_FINAL:  1 =	CASO DE COVID-19 CONFIRMADO POR ASOCIACIÓN CLÍNICA EPIDEMIOLÓGICA
#                       2 =	CASO DE COVID-19 CONFIRMADO POR COMITÉ DE  DICTAMINACIÓN
#                       3 =	CASO DE SARS-COV-2  CONFIRMADO
#                       4 =  INVÁLIDO POR LABORATORIO	
#                       5 =	NO REALIZADO POR LABORATORIO
#                       6 =  CASO SOSPECHOSO 	
#                       7 =	NEGATIVO A SARS-COV-2

df.GTO30jun2021 <- subset.data.frame(df.30jun2021, ENTIDAD_RES==11,
                                     select=c(FECHA_SINTOMAS,
                                              FECHA_INGRESO,
                                              FECHA_DEF,
                                              ID_REGISTRO,
                                              MUNICIPIO_RES,
                                              ENTIDAD_RES,
                                              RESULTADO_LAB,
                                              RESULTADO_ANTIGENO,
                                              CLASIFICACION_FINAL,
                                              TIPO_PACIENTE,
                                              INTUBADO, UCI,
                                              SEXO, EDAD), 
                                     drop=FALSE)
#acomodamos por orden ascendente de FECHA_SINTOMAS la base de datos anterior
df.GTO30jun2021 <- df.GTO30jun2021[order(as.Date(df.GTO30jun2021$FECHA_SINTOMAS,
                                                 format="%Y-%m-%d")),]


# ----- GUARDAMOS EN UN .csv LOS DATOS FILTRADOS ANTERIORMENTE -----
write.csv(df.GTO30jun2021, "/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/30jun2021-GTO.csv", 
          row.names = FALSE)


# ----- AHORA FILTRAMOS LOS MUNICIPIOS QUE NOS INTERESAN -----
# León = 020
# Silao = 037
# Guanajuato = 015
# Irapuato = 017
# Salamanca = 027
# Cortazar + Villagrán + Celaya + Salvatierra = 011 + 044 + 007 + 028

df.leon30jun2021 <- subset.data.frame(df.GTO30jun2021, MUNICIPIO_RES==020,
                                      drop=FALSE)
df.silao30jun2021 <- subset.data.frame(df.GTO30jun2021, MUNICIPIO_RES==037,
                                       drop=FALSE)
df.gto30jun2021 <- subset.data.frame(df.GTO30jun2021, MUNICIPIO_RES==015,
                                     drop=FALSE)
df.irap30jun2021 <- subset.data.frame(df.GTO30jun2021, MUNICIPIO_RES==017,
                                      drop=FALSE)
df.salam30jun2021 <- subset.data.frame(df.GTO30jun2021, MUNICIPIO_RES==027,
                                       drop=FALSE)
df.cya30jun2021 <- subset.data.frame(df.GTO30jun2021, (MUNICIPIO_RES==011 | 
                                                         MUNICIPIO_RES==044 |
                                                         MUNICIPIO_RES==007 |  
                                                         MUNICIPIO_RES==028),
                                     drop=FALSE)

#En la siguiente lista guaramos todos los df de los municipios (es como un df gigante)
#Entonces acCederemos a los df de cada municipio a partir de df.municip30jun2021
df.municip30jun2021 <- list(df.gto30jun2021, df.leon30jun2021,
                            df.cya30jun2021, df.silao30jun2021,
                            df.irap30jun2021, df.salam30jun2021)

#Dado que los df están almacenados en la lista anterior, eliminamos los df de cada municipio (los individuales)
rm(df.leon30jun2021, df.silao30jun2021, df.gto30jun2021, df.irap30jun2021,
   df.salam30jun2021, df.cya30jun2021)


# ----- GUARDAMOS EN UN .csv LOS DATOS DE LOS MUNICIPIOS FILTRADOS ANTERIORMENTE -----
#Creamos un vector que designa los municipios
municipios <- c("Guanajuato", "León", "Celaya",
                "Silao", "Irapuato", "Salamanca")

for ( i in 1:length(municipios)) {
  write.csv(df.municip30jun2021[[i]],
              paste0("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/30jun2021-",municipios[i],".csv"))
}


# ----- SEPARAMOS POR MESES LOS DATOS DEL ESTADO -----
#Observemos que debemos acumular las fechas, 
#es decir, todos inician a partir de enero y  acumulamos los datos de todo el mes
#por ejemplo df.ABR20_GTO guarda los datos desde enero hasta el final de abril 2020

df.ENE20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-01-31"),
                                  drop=FALSE)
df.FEB20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-02-28"),
                                  drop=FALSE)
df.MAR20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-03-31"),
                                  drop=FALSE)
df.ABR20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-04-30"),
                                  drop=FALSE)
df.MAY20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-05-31"),
                                  drop=FALSE)
df.JUN20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-06-30"),
                                  drop=FALSE)
df.JUL20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-07-31"),
                                  drop=FALSE)
df.AGO20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-08-31"),
                                  drop=FALSE)
df.SEP20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-09-30"),
                                  drop=FALSE)
df.OCT20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-10-31"),
                                  drop=FALSE)
df.NOV20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-11-30"),
                                  drop=FALSE)
df.DIC20_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-12-31"),
                                  drop=FALSE)
df.ENE21_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-01-31"),
                                  drop=FALSE)
df.FEB21_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-02-28"),
                                  drop=FALSE)
df.MAR21_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-03-31"),
                                  drop=FALSE)
df.ABR21_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-04-30"),
                                  drop=FALSE)
df.MAY21_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-05-31"),
                                  drop=FALSE)
df.JUN21_GTO <- subset.data.frame(df.GTO30jun2021, (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-06-19"),
                                  drop=FALSE)



#Al igual que df.mucip cramos una lista con los df por meses
df.MESES_GTO <- list(df.ENE20_GTO, df.FEB20_GTO, df.MAR20_GTO, df.ABR20_GTO,
                     df.MAY20_GTO, df.JUN20_GTO, df.JUL20_GTO, df.AGO20_GTO,
                     df.SEP20_GTO, df.OCT20_GTO, df.NOV20_GTO, df.DIC20_GTO,
                     df.ENE21_GTO, df.FEB21_GTO, df.MAR21_GTO, df.ABR21_GTO,
                     df.MAY21_GTO, df.JUN21_GTO)

rm(df.ENE20_GTO, df.FEB20_GTO, df.MAR20_GTO, df.ABR20_GTO,
   df.MAY20_GTO, df.JUN20_GTO, df.JUL20_GTO, df.AGO20_GTO,
   df.SEP20_GTO, df.OCT20_GTO, df.NOV20_GTO, df.DIC20_GTO,
   df.ENE21_GTO, df.FEB21_GTO, df.MAR21_GTO, df.ABR21_GTO,
   df.MAY21_GTO, df.JUN21_GTO)


# ----- GUARDAMOS EN DISTINTOS .csv LOS DATOS DE LOS df ANTERIORES DEL ESTADO HASTA EL MES SEÑALADO -----
#cada .csv generado a continuación guarda los datos del estado de Guanajuato hasta el mes indicado
#por ejemplo df.MESES_GTO[[3]] representa marzo y guarda los datos hasta marzo en un archivo llamado "03-GTO.csv"
setwd("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/0. GTO")


n = length(df.MESES_GTO)
for (i in 1:n) {
  if(i <= 9)
    write.csv(df.MESES_GTO[[i]], paste0("0",i,"-GTO.csv"), 
              row.names = FALSE)
  else
    write.csv(df.MESES_GTO[[i]], paste0(i,"-GTO.csv"), 
              row.names = FALSE)
}


# ----- SEPARAMOS POR MESES LOS DATOS DE CADA MUNICIPIO -----
#Observemos que debemos acumular las fechas, 
#es decir, todos inician a partir de enero y  acumulamos los datos de todo el mes
#por ejemplo df.ABR20_municipios[2] guarda los datos...
#...del municipio 2=León desde enero hasta el final de abril 2020


#Así, iteramos el código del estado para cada municipio usando el vector "municipios"
#Recordemos que el vector de municipios es como sigue:
#municipios <- c("Guanajuato", "León", "Celaya",
#                "Silao", "Irapuato", "Salamanca")

for (i in 1:length(municipios)) {
  assign(paste0("df.MESES_", municipios[i]), 
         list(assign(paste0("df.ENE20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-01-31"),
                                       drop=FALSE)),
              assign(paste0("df.FEB20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-02-28"),
                                       drop=FALSE)),
              assign(paste0("df.MAR20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-03-31"),
                                       drop=FALSE)),
              assign(paste0("df.ABR20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-04-30"),
                                       drop=FALSE)),
              assign(paste0("df.MAY20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-05-31"),
                                       drop=FALSE)),
              assign(paste0("df.JUN20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-06-30"),
                                       drop=FALSE)),
              assign(paste0("df.JUL20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-07-31"),
                                       drop=FALSE)),
              assign(paste0("df.AGO20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-08-31"),
                                       drop=FALSE)),
              assign(paste0("df.SEP20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-09-30"),
                                       drop=FALSE)),
              assign(paste0("df.OCT20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-10-31"),
                                       drop=FALSE)),
              assign(paste0("df.NOV20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-11-30"),
                                       drop=FALSE)),
              assign(paste0("df.DIC20_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2020-12-31"),
                                       drop=FALSE)),
              
              assign(paste0("df.ENE21_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-01-31"),
                                       drop=FALSE)),
              assign(paste0("df.FEB21_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-02-28"),
                                       drop=FALSE)),
              assign(paste0("df.MAR21_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-03-31"),
                                       drop=FALSE)),
              assign(paste0("df.ABR21_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-04-30"),
                                       drop=FALSE)),
              assign(paste0("df.MAY21_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-05-31"),
                                       drop=FALSE)),
              assign(paste0("df.JUN21_",municipios[i]),
                     subset.data.frame(df.municip30jun2021[[i]],
                                       (FECHA_SINTOMAS>="2020-01-01" & FECHA_SINTOMAS<="2021-06-19"),
                                       drop=FALSE))
         ))
}
rm(list=ls(pattern = "ENE"))
rm(list=ls(pattern = "FEB"))
rm(list=ls(pattern = "MAR"))
rm(list=ls(pattern = "ABR"))
rm(list=ls(pattern = "MAY"))
rm(list=ls(pattern = "JUN"))
rm(list=ls(pattern = "JUL"))
rm(list=ls(pattern = "AGO"))
rm(list=ls(pattern = "SEP"))
rm(list=ls(pattern = "OCT"))
rm(list=ls(pattern = "NOV"))
rm(list=ls(pattern = "DIC"))


# ----- GUARDAMOS EN DISTINTOS .csv LOS DATOS DE LOS df ANTERIORES DE CADA MUNICIPIO HASTA EL MES SEÑALADO -----
#cada .csv generado a continuación guarda los datos de cada municipio hasta el mes indicado
#por ejemplo df.MESES_(municipio[2])[[3]] representa marzo y guarda los datos...
#del municipio de León hasta marzo en un archivo llamado "03-León.csv" 

n=length(municipios)
for (i in 1:n) {
  setwd(paste0("/Users/andreaheredia/Documents/1. UG/TESIS/COVID/30JUN2021/", i, ". ", municipios[i]))
  
  aux <- get(paste0("df.MESES_", municipios[i]))
  k = length(aux)
  #print(c(k, length(k)))
  for (j in 1:k) {
    if(j <= 9){
      write.csv(aux[[j]], paste0("0", j, "-", municipios[i], ".csv"),
                row.names = FALSE)
      #print(c("0",j,municipios[i]))
      }
    else{
      write.csv(aux[[j]], paste0(j, "-", municipios[i], ".csv"),
                row.names = FALSE)
      #print(c(j, municipios[i]))
      }
  }
}

# ----- NO CORRER HASTA ESTAR SEGUROS DE NO NECESITAR MÁS LOS DATOS -----
# rm(list=ls(pattern = "df"))
# rm(aux, municipios)
# rm(i,j,k,n)

