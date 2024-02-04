#_______________________________________________________________________________

# **********  ENCUESTA NACIONAL DE INCLUCION FINANCIERA (ENIF) 2021  ***********
# **************************  REPORTE DE RESULTADOS ****************************
#_______________________________________________________________________________
#####################################################################################

# LIMPIEZA Y DESCARGA DE MODULOS 2015

#####################################################################################
#-------------------------------------------------------------------------------
# Borrar datos del entorno
#-------------------------------------------------------------------------------
rm(list = ls())
gc() 
#-------------------------------------------------------------------------------
# Directorio
#setwd("Desktop")  # Ajusta la ubicacion segun tu sistema
#raiz <- setwd("C:\\[Usuario]\\[Carpeta donde se va a guardar]")
#raiz <- setwd("C:\\Users\\IDSS3168\\OneDrive - Comision Nacional Bancaria y de Valores\\Documentos")
raiz <- setwd("C:\\Users\\hp\\Documents\\PP")

dir.create("enif", showWarnings = FALSE, recursive = TRUE)
#-------------------------------------------------------------------------------
# Paqueteria
#install.packages("FactoMineR")
#install.packages("missMDA")
#install.packages("haven")
#-------------------------------------------------------------------------------
library(haven) 
library(sjlabelled)      
library(labelled)      
library(labeling)      
library(FactoMineR)      
library(missMDA)      
#---------------------------
if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, stringr,  janitor, showtext)
font_add_google("Poppins", "pop")
showtext_auto()
#-------------------------------------------------------------------------------
# Funcion para descargar y unzip
download_and_unzip <- function(url, dir) {
  # Crear directorio si no existe
  if (!dir.exists(dir)) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Crear el archivo destino
  destfile <- file.path(dir, basename(url))
  
  # Descarga
  download.file(url, destfile)
  
  # Unzip (para archivos .zip)
  if (endsWith(destfile, ".zip")) {
    unzip(destfile, exdir = dir)
  }
}
#-------------------------------------------------------------------------------
# Utiliza la funcion para descargar datos
download_and_unzip("https://www.inegi.org.mx/contenidos/programas/enif/2015/microdatos/enif_2015_bd_sav.zip", "enif/enif_2015")
#-------------------------------------------------------------------------------
# Eliminar archivos zip
file.remove("enif/enif_2015/enif_2015_bd_sav.zip")
#-------------------------------------------------------------------------------
# Leer los datos
#-------------------------------------------------------------------------------
#2015
tmodulo1_2015 <- read_sav("enif/enif_2015/tmodulo1.sav")
tmodulo2_2015 <- read_sav("enif/enif_2015/tmodulo2.sav")
tmodulo3_2015 <- read_sav("enif/enif_2015/tmodulo3.sav")
#-------------------------------------------------------------------------------
# Leer el archivo .sav
tmodulo1_2015 <- haven::read_sav("enif/enif_2015/tmodulo1.sav")

# Obtener los nombres de las columnas que contienen la letra "a", ya que se contestan otras respuestas con palabras,  variables tipo caracter.
columnas_con_a <- names(tmodulo1_2015)[sapply(names(tmodulo1_2015), function(x) any(grepl("a", x, ignore.case = TRUE)))]

# Excluir las columnas que no converitemos a variables numericas
columnas_excluir_tmodulo1_15 <- c(
  "p4_4a", "p4_7a", "p4_8a", "p5_2a", "p5_6a", "p5_7a", "p5_8a", "p5_10a",
  "p5_11_6a", "p5_17a", "p5_23a", "p5_24a", "p5_29_1a", "p5_30_1a",
  "p5_29_2a", "p5_30_2a", "p5_32_2a", "p6_1_5a", "p6_3a"
)

# Convertir las columnas restantes a tipo de dato numérico
columnas_a_convertir <- setdiff(names(tmodulo1_2015), columnas_excluir_tmodulo1_15)
tmodulo1_2015[columnas_a_convertir] <- lapply(tmodulo1_2015[columnas_a_convertir], as.numeric)

# Reemplazar valores nulos en todas las columnas por 0
tmodulo1_2015[] <- lapply(tmodulo1_2015, function(x) ifelse(is.na(x), 0, x))

# Cambiar el tipo de dato de las columnas a caracteres (strings)
tmodulo1_2015$upm_dis <- as.character(tmodulo1_2015$upm_dis)
tmodulo1_2015$viv_sel <- as.character(tmodulo1_2015$viv_sel)
tmodulo1_2015$hogar <- as.character(tmodulo1_2015$hogar)

# Ajustar la longitud de las columnas "upm_dis," "viv_sel," y "hogar"
tmodulo1_2015$upm_dis <- str_pad(tmodulo1_2015$upm_dis, width = 5, side = "left", pad = "0")
tmodulo1_2015$viv_sel <- str_pad(tmodulo1_2015$viv_sel, width = 3, side = "left", pad = "0")
tmodulo1_2015$hogar <- str_pad(tmodulo1_2015$hogar, width = 2, side = "left", pad = "0")

# Crear una nueva columna con la concatenacion de las columnas upm_dis, viv_sel y hogar
tmodulo1_2015 <- tmodulo1_2015 %>%
  mutate(uvh = paste(upm_dis, viv_sel, hogar, sep = ""))

# Verificar 
tmodulo1_2015$uvh

#-------------------------------------------------------------------------------
# Limpieza de datos para el MODULO2 de la ENIF 2015 (archivo.sav)
#-------------------------------------------------------------------------------
# Leer el archivo .sav
tmodulo2_2015 <- haven::read_sav("enif/enif_2015/tmodulo2.sav")

# Obtener los nombres de las columnas que contienen la letra "a", ya que se contestan otras respuestas con palabras,  variables tipo caracter.
columnas_con_a <- names(tmodulo2_2015)[sapply(names(tmodulo2_2015), function(x) any(grepl("a", x, ignore.case = TRUE)))]

# Excluir las columnas que no converitemos a variables numericas
columnas_excluir_tmodulo2_15 <- setdiff(names(tmodulo2_2015), c("p4_4a", "p4_7a", "p4_8a", "p5_2a", "p5_6a", "p5_7a", "p5_8a", "p5_10a", "p5_11_6a", "p5_17a", "p5_23a", "p5_24a", "p5_29_1a", "p5_30_1a", "p5_29_2a", "p5_30_2a", "p5_32_2a", "p6_1_5a", "p6_3a"))

# Convertir las columnas restantes a tipo de dato numérico
columnas_a_convertir <- setdiff(names(tmodulo2_2015), columnas_excluir_tmodulo2_15)
tmodulo2_2015[columnas_a_convertir] <- lapply(tmodulo2_2015[columnas_a_convertir], as.numeric)

# Reemplazar valores nulos en todas las columnas por 0
tmodulo2_2015[] <- lapply(tmodulo2_2015, function(x) ifelse(is.na(x), 0, x))

# Cambiar el tipo de dato de las columnas a caracteres (strings)
tmodulo2_2015$upm_dis <- as.character(tmodulo2_2015$upm_dis)
tmodulo2_2015$viv_sel <- as.character(tmodulo2_2015$viv_sel)
tmodulo2_2015$hogar <- as.character(tmodulo2_2015$hogar)

# Ajustar la longitud de las columnas "upm_dis," "viv_sel," y "hogar"
tmodulo2_2015$upm_dis <- str_pad(tmodulo2_2015$upm_dis, width = 5, side = "left", pad = "0")
tmodulo2_2015$viv_sel <- str_pad(tmodulo2_2015$viv_sel, width = 3, side = "left", pad = "0")
tmodulo2_2015$hogar <- str_pad(tmodulo2_2015$hogar, width = 2, side = "left", pad = "0")

# Crear una nueva columna con la concatenacion de las columnas upm_dis, viv_sel y hogar
tmodulo2_2015 <- tmodulo2_2015 %>%
  mutate(uvh = paste(upm_dis, viv_sel, hogar, sep = ""))

# Verificar 
tmodulo2_2015$uvh

#-------------------------------------------------------------------------------
# Limpieza de datos para el MODULO1 de la ENIF 2015 (archivo.sav)
#-------------------------------------------------------------------------------
# Leer el archivo .sav
tmodulo3_2015 <- haven::read_sav("enif/enif_2015/tmodulo3.sav")

# Obtener los nombres de las columnas que contienen la letra "a", ya que se contestan otras respuestas con palabras,  variables tipo caracter.
columnas_con_a <- names(tmodulo3_2015)[sapply(names(tmodulo3_2015), function(x) any(grepl("a", x, ignore.case = TRUE)))]

# Excluir las columnas que no converitemos a variables numericas
columnas_excluir_tmodulo3_15 <- c("p9_4a", "p9_5a", "p10_2a", "p10_4a", "p10_9a", "p10_16a", "p10_18a", "p10_21a", "p11_1_3a", "p12_5_1a", "p12_5_2a", "p12_5_3a", "p12_5_4a", "p12_5_5a", "p12_3_6a", "p12_5_6a")

# Convertir las columnas restantes a tipo de dato numérico
columnas_a_convertir <- setdiff(names(tmodulo3_2015), columnas_excluir_tmodulo3_15)
tmodulo3_2015[columnas_a_convertir] <- lapply(tmodulo3_2015[columnas_a_convertir], as.numeric)

# Reemplazar valores nulos en todas las columnas por 0
tmodulo3_2015[] <- lapply(tmodulo3_2015, function(x) ifelse(is.na(x), 0, x))

# Cambiar el tipo de dato de las columnas a caracteres (strings)
tmodulo3_2015$upm_dis <- as.character(tmodulo3_2015$upm_dis)
tmodulo3_2015$viv_sel <- as.character(tmodulo3_2015$viv_sel)
tmodulo3_2015$hogar <- as.character(tmodulo3_2015$hogar)

# Ajustar la longitud de las columnas "upm_dis," "viv_sel," y "hogar"
tmodulo3_2015$upm_dis <- str_pad(tmodulo3_2015$upm_dis, width = 5, side = "left", pad = "0")
tmodulo3_2015$viv_sel <- str_pad(tmodulo3_2015$viv_sel, width = 3, side = "left", pad = "0")
tmodulo3_2015$hogar <- str_pad(tmodulo3_2015$hogar, width = 2, side = "left", pad = "0")

# Crear una nueva columna con la concatenacion de las columnas upm_dis, viv_sel y hogar
tmodulo3_2015 <- tmodulo3_2015 %>%
  mutate(uvh = paste(upm_dis, viv_sel, hogar, sep = ""))

# Verificar 
tmodulo3_2015$uvh

#-------------------------------------------------------------------------------

# Columnas de interes

#-------------------------------------------------------------------------------
#_______________________________________________________________________________

#                                 VARIABLES DE INTERES
#_______________________________________________________________________________

# Edad
#_______________________________________________________________________________

# Función para asignar grupos de edad para 2018
grupos_edad_2018 <- function(age) {
  if (age >= 18 & age <= 29) {
    return("De 18 a 29 años")
  } else if (age >= 30 & age <= 44) {
    return("De 30 a 44 años")
  } else if (age >= 45 & age <= 59) {
    return("De 45 a 59 años")
  } else if (age >= 60) {
    return("Mayor a 60")
  } else {
    return(NULL)
  }
}

tmodulo1_2015$grupo_edad <- sapply(tmodulo1_2015$edad, grupos_edad_2018)

#_______________________________________________________________________________

# Rural-Urbana
#_______________________________________________________________________________
tmodulo1_2015 <- tmodulo1_2015 %>%
  mutate(tloc = case_when(
    tloc == 1 ~ 4,
    tloc == 2 ~ 3,
    tloc == 3 ~ 2,
    tloc == 4 ~ 1
  ))

# Etiquetas para tloc
labels <- c("<2,500 habs", "2,500-14,999 habs", "15,00-99,999 habs", ">100,000 habs")

levels(tmodulo1_2015$tloc) <- labels

# Etiqueta de variable para tloc
attr(tmodulo1_2015$tloc, "label") <- "Tamaño de localidad"

# Generamos variable de area para obtener las rurales y urbanas
tmodulo1_2015 <- tmodulo1_2015 %>%
  mutate(area = ifelse(tloc %in% c(1, 2), 1, 0))

# Etiquetas para area
labels_area <- c("Rural", "Urbana")
levels(tmodulo1_2015$area) <- labels_area

# Etiqueta de variable para area
attr(tmodulo1_2015$area, "label") <- "Poblacion rural y urbana 2021"

#_______________________________________________________________________________

# Sexo
#_______________________________________________________________________________
# Definir etiquetas para la variables
tmodulo1_2015$sexo <- factor(tmodulo1_2015$sexo, labels = c("Hombre", "Mujer"))
#_______________________________________________________________________________

# 1) Nivel de escolaridad de la persona elegida (P3_1_1)
#_______________________________________________________________________________
tmodulo1_2015$niv_ed_dgasf <- 0

# Generar la variable niv_ed_dgasf
tmodulo1_2015 <- tmodulo1_2015 %>%
  mutate(niv_ed_dgasf = NA) %>%
  mutate(niv_ed_dgasf = ifelse((niv >= 0 & niv <= 2) | niv == 99, 1, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(niv >= 3 & niv <= 4, 2, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(niv >= 5 & niv <= 7, 3, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(niv >= 8 & niv <= 9, 4, niv_ed_dgasf))


# Etiquetas para la nueva variable
levels(tmodulo1_2015$niv_ed_dgasf) <- c(
  "Hasta primaria",
  "Hasta secundaria",
  "Hasta nivel medio superior",
  "Licenciatura o mas"
)

# Etiquetar la variable niv_ed_dgasf
attr(tmodulo1_2015$niv_ed_dgasf, "label") <- "Nivel educativo"

#Vemos las columnasque tiene la columna niv_ed_dgasf
var_niv_ed_dgasf <- table(tmodulo1_2015$niv_ed_dgasf)
print(var_niv_ed_dgasf)
tmodulo1_2015$niv_ed_dgasf

#_______________________________________________________________________________

# 2) Estado conyugal de la persona elegida (P3_2)
#_______________________________________________________________________________

# Renombrar columnas
# Etiqueta 

# --------------------- Función para asignar grupos de respuestas 
estado_civil <- function(e_c) {
  if (e_c %in% c(1, 5)) {
    return(1)#Casado / Union libre
  } else if (e_c %in% c(2, 3, 4)) {
    return(2)#Separado / Viudo
  } else if (e_c == 6) {
    return(3) #Soltero
  } else {
    return("Ninguno")
  }
}
tmodulo1_2015$edo_civ <- sapply(tmodulo1_2015$p3_2, estado_civil)
attr(tmodulo1_2015$edo_civ, "label") <- "Estado civil"

# Crear variable casado
tmodulo1_2015$casado <- ifelse(tmodulo1_2015$edo_civ == 1, 1, 0)

# Etiquetar variable casado
attr(tmodulo1_2015$casado, "label") <- "¿Está casado o en unión libre?"
levels(tmodulo1_2015$casado) <- c("No", "Sí")

tmodulo1_2015$edo_civ
tmodulo1_2015$casado 
#_______________________________________________________________________________

# 3) Lengua indigena y apoyos gobierno
# No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 4) Situacion laboral
#_______________________________________________________________________________

# Condición de actividad de la persona elegida (P3_5)
# Verificación de actividad (P3_6)
# Posición en la ocupación de la persona elegida (P3_7)
#-------------------------------------------------------------------------------
tmodulo1_2015$ocup <- 0

# Condiciones
tmodulo1_2015$ocup <- ifelse(tmodulo1_2015$p3_5 == 1 | tmodulo1_2015$p3_5 == 2, 1, ifelse(tmodulo1_2015$p3_5 == 3, 0, NA))

tmodulo1_2015$ocup[tmodulo1_2015$p3_6 >= 1 & tmodulo1_2015$p3_6 < 6] <- 1
tmodulo1_2015$ocup[tmodulo1_2015$p3_7 == 1] <- 0

# Etiquetas para la variable 'ocup'
levels(tmodulo1_2015$ocup) <- c("Desocupado", "Ocupado")

#-------------------------------------------------------------------------------
# Crear una variable 'trab'
tmodulo1_2015$trab <- tmodulo1_2015$ocup == 1

# Etiqueta para la variable 'trab'
levels(tmodulo1_2015$trab) <- c("No", "Sí")
#-------------------------------------------------------------------------------
# Crear una variable 'pea'
tmodulo1_2015$pea <- tmodulo1_2015$p3_5 == 1 | tmodulo1_2015$p3_5 == 2 | tmodulo1_2015$p3_5 == 3

# Etiquetas para la variable 'pea'
levels(tmodulo1_2015$pea) <- c("PNEA", "PEA")
#-------------------------------------------------------------------------------

var_ocup <- table(tmodulo1_2015$ocup)
var_trab <- table(tmodulo1_2015$trab)
var_pea <- table(tmodulo1_2015$pea)

# Etiquetar variables
attr(tmodulo1_2015$ocup, "label") <- "Situacion laboral"
attr(tmodulo1_2015$trab, "label") <- "Encuestado trabajo remunerado"
attr(tmodulo1_2015$pea, "label") <- "Poblacion economicamente activa"

var_ocup
var_trab
var_pea

tmodulo1_2015$ocup
tmodulo1_2015$trab
tmodulo1_2015$pea

# ------------------------------------------------------------------------------
# Tiene cuenta bancaria o con institución financiera
# ------------------------------------------------------------------------------
tmodulo1_2015$cuenta<- ifelse(tmodulo1_2015$p5_11_1 == 1 | tmodulo1_2015$p5_11_2 == 1 |
                                tmodulo1_2015$p5_11_3 == 1 | tmodulo1_2015$p5_11_4 == 1 |
                                tmodulo1_2015$p5_11_5 == 1 | tmodulo1_2015$p5_11_6 == 1, 1, 0)

# Etiqueta de variables de cuenta
attr(tmodulo1_2015$CUENTA, "label") <- "Tenencia de de cuenta formal"

# ------------------------------------------------------------------------------
# Crédito formal
# ------------------------------------------------------------------------------

tmodulo1_2015$cred_for <- ifelse(tmodulo2_2015$p6_9_1 == 1 | tmodulo2_2015$p6_9_2 == 1 |
                                   tmodulo2_2015$p6_9_3 == 1 | tmodulo2_2015$p6_9_4 == 1 |
                                   tmodulo2_2015$p6_9_5 == 1 | tmodulo2_2015$p6_9_6 == 1 |
                                   tmodulo2_2015$p6_9_7 == 1 | tmodulo2_2015$p6_9_8 == 1, 1, 0)

# Etiqueta de variables de crédito
attr(tmodulo1_2015$cred_for, "label") <- "Tenencia de crédito formal"

# ------------------------------------------------------------------------------
#  Tenencia de seguros
# ------------------------------------------------------------------------------
tmodulo1_2015$seguro <- ifelse(tmodulo2_2018$p8_1 == 1 | tmodulo2_2018$p8_2 == 1, 1, 0)

# Etiqueta 
attr(tmodulo1_2015$seguro, "label") <- "Tenencia de una de seguros"

# ------------------------------------------------------------------------------
# Tenencia de cuentas de ahorro para el retiro
# ------------------------------------------------------------------------------
tmodulo1_2015$AFORE <- ifelse(tmodulo2_2015$p8_1 == 1, 1, 0) 
# Etiqueta 
attr(tmodulo1_2015$AFORE, "label") <- "Tiene afore o cuenta de retiro"
# ______________________________________________________________________________

# Variables historicas
# ______________________________________________________________________________

#----------------------  Variables ex usuarios de productos financieros formales

tmodulo1_2015$cuenta_ex <- ifelse(tmodulo1_2015$p5_5 == 1, 1, 0)
tmodulo1_2015$credito_ex <- ifelse(tmodulo1_2015$p6_5 == 1, 1, 0)
tmodulo1_2015$seguro_ex <- ifelse(tmodulo2_2015$p7_2 == 1, 1, 0)

attr(tmodulo1_2015$cuenta_ex, "label") <- "Ex usuario de una cuenta de captación formal"
attr(tmodulo1_2015$credito_ex, "label") <- "Ex usuario de que ha tenido un crédito formal de captación formal"
attr(tmodulo1_2015$seguro_ex, "label") <- "Ex usuario de que ha tenido un seguro"


#---------------------- . Variables historicas 

tmodulo1_2015$cuenta_hist <- ifelse(tmodulo1_2015$cuenta == 1 | tmodulo1_2015$cuenta_ == 1, 1, 0)
tmodulo1_2015$cred_hist <- ifelse(tmodulo1_2015$cred_for == 1 | tmodulo1_2015$cred_ex == 1, 1, 0) 
tmodulo1_2015$seg_hist <- ifelse(tmodulo1_2015$seguro == 1 | tmodulo1_2015$seguro_ex == 1, 1, 0)

attr(tmodulo1_2015$cuenta_hist, "label") <- "Ha tenido una cuenta de captación formal"
attr(tmodulo1_2015$cred_hist, "label") <- "Ha tenido un crédito formal de captación formal"
attr(tmodulo1_2015$seguro_hist, "label") <- "Ha tenido un seguro"

# ------------------------------------------------------------------------------
# Producto Financiero Formal
# ------------------------------------------------------------------------------
tmodulo1_2015$prod_fin_hist <- ifelse(tmodulo1_2015$cuenta_hist == 1 | tmodulo1_2015$cred_hist == 1 | tmodulo1_2015$afore == 1 | tmodulo1_2015$seguro_hist == 1, 1, 0)
attr(tmodulo1_2015$prod_fin_hist, "label") <- "Ha tenido un producto financiero formal"

#_______________________________________________________________________________

# Aportaciones voluntarias Afore
#_______________________________________________________________________________
tmodulo1_2015$afore_apor <- ifelse(tmodulo2_2015$p8_4 == "1", 1, 0)

# Etiqueta
attr(tmodulo1_2015$afore_apor, "label") <- "Realiza aportaciones voluntarias a afore"
#_______________________________________________________________________________

# Usos de canales financieros
#_______________________________________________________________________________

# Uso de sucursales
#_______________________________________________________________________________
tmodulo1_2015$sucursal <- ifelse(tmodulo3_2015$p10_1 == 1, 1, 0)

# Etiquetar la columna 
attr(tmodulo1_2015$sucursal, "label") <- "Ha usado sucursal en el último año"
#_______________________________________________________________________________

# Uso de cajero
#_______________________________________________________________________________
tmodulo1_2015$cajero <- ifelse(tmodulo3_2015$p10_8 == 1, 1, 0)

# Etiqueta para la columna cajero en tmodulo1_2015
attr(tmodulo1_2015$cajero, "label") <- "Ha usado cajero en último año"
#_______________________________________________________________________________

# Uso de corresponsal
#_______________________________________________________________________________
tmodulo1_2015$corres <- ifelse(tmodulo3_2015$p10_17 == 1, 1, 0)

# Etiquetar la columna 
attr(tmodulo1_2015$corres, "label") <- "Ha usado corresponsal en último año"
#_______________________________________________________________________________

# Población que compara productos
#_______________________________________________________________________________
#---------------------- Comparación de productos financieros antes de contratarlos

# 2015
tmodulo1_2015$compara_cuenta <- ifelse(tmodulo1_2015$p5_9 == 1, 1, 0)
tmodulo1_2015$compara_cred <- ifelse(tmodulo2_2015$p6_8 == 1, 1, 0)
tmodulo1_2015$compara_seg <- ifelse(tmodulo2_2015$p7_5 == 1, 1, 0)

################################################################################


# Cambia el directorio de trabajo al nuevo directorio "enif"
setwd("enif")
# Guardar csv
write.csv(tmodulo1_2015, file = "tmodulo1_2015_clean.csv", row.names = FALSE)


