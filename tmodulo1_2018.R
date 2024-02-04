#_______________________________________________________________________________

# **********  ENCUESTA NACIONAL DE INCLUCION FINANCIERA (ENIF) 2018  ***********
# **************************  REPORTE DE RESULTADOS ****************************
#_______________________________________________________________________________
#####################################################################################

# LIMPIEZA Y DESCARGA DE tmodulo_2018

#####################################################################################
#-------------------------------------------------------------------------------
# Borrar tmodulo1_2018 del entorno
#-------------------------------------------------------------------------------
rm(list = ls())
#-------------------------------------------------------------------------------
# Directorio
#setwd("Desktop")  # Ajusta la ubicacion segun tu sistema
#raiz <- setwd("C:\\[Tu ubicación]")
#raiz <- setwd("C:\\Users\\IDSS3168\\OneDrive - Comision Nacional Bancaria y de Valores\\Documentos")
raiz <- setwd("C:\\Users\\hp\\Documents\\PP")

## Crear folders de almacenamiento
dir.create("enif", showWarnings = FALSE, recursive = TRUE)
#-------------------------------------------------------------------------------
# Paqueteria
#install.packages("FactoMineR")
#install.packages("missMDA")
#-------------------------------
# Paqueteria
library(FactoMineR)    
library(missMDA)
library(sjlabelled) 
library(labelled)  
library(labeling) 

if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, stringr, janitor, showtext)
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
download_and_unzip("https://www.inegi.org.mx/contenidos/programas/enif/2018/datosabiertos/conjunto_de_datos_enif_2018_csv.zip", "enif/enif_2018")

#-------------------------------------------------------------------------------
# Eliminar archivos zip
file.remove("conjunto_de_datos_enif_2018_csv.zip")

#-------------------------------------------------------------------------------
# Leer los datos
#-------------------------------------------------------------------------------
#2018

tmodulo1_2018 <- read_csv("enif/enif_2018/conjunto_de_datos_tmodulo_enif_2018/conjunto_de_datos/tmodulo.csv") %>%
  janitor::clean_names()

tmodulo2_2018 <- read_csv("enif/enif_2018/conjunto_de_datos_tmodulo2_enif_2018/conjunto_de_datos/conjunto_de_datos_tmodulo2_enif_2018.csv") %>%
  janitor::clean_names()
#_______________________________________________________________________________

# BASES DE DATOS 2018
#_______________________________________________________________________________
#-------------------------------------------------------------------------------
# MODULO1 de la ENIF 2018 (archivo .csv)
#-------------------------------------------------------------------------------
tmodulo1_2018 <- read_csv("enif/enif_2018/conjunto_de_datos_tmodulo_enif_2018/conjunto_de_datos/tmodulo.csv") 

# Cambiar el tipo de dato de las columnas "UPM_DIS," "viv_sel," y "HOGAR" a caracteres (strings)
columnas_string <- c("upm_dis", "viv_sel", "hogar")
tmodulo1_2018 <- tmodulo1_2018 %>% mutate(across(all_of(columnas_string), as.character))

# Ajustar la longitud de las columnas "upm_dis," "viv_sel," y "hogar"
tmodulo1_2018$upm_dis <- str_pad(tmodulo1_2018$upm_dis, width = 5, side = "left", pad = "0")
tmodulo1_2018$viv_sel <- str_pad(tmodulo1_2018$viv_sel, width = 3, side = "left", pad = "0")
tmodulo1_2018$hogar <- str_pad(tmodulo1_2018$hogar, width = 2, side = "left", pad = "0")

# Crear una nueva columna con la concatenacion de las columnas UPM_DIS, VIV_SEL y HOGAR
tmodulo1_2018 <- tmodulo1_2018 %>%
  mutate(uvh = paste(upm_dis, viv_sel, hogar, sep = ""))

# Verificar 
tmodulo1_2018$uvh
#-------------------------------------------------------------------------------
# MODULO2 de la ENIF 2018 (archivo .csv)
#-------------------------------------------------------------------------------
tmodulo2_2018 <- read_csv("enif/enif_2018/conjunto_de_datos_tmodulo2_enif_2018/conjunto_de_datos/conjunto_de_datos_tmodulo2_enif_2018.csv") 

# Cambiar el tipo de dato de las columnas "UPM_DIS," "viv_sel," y "HOGAR" a caracteres (strings)
columnas_string <- c("upm_dis", "viv_sel", "hogar")
tmodulo2_2018 <- tmodulo2_2018 %>% mutate(across(all_of(columnas_string), as.character))

# Ajustar la longitud de las columnas "upm_dis," "viv_sel," y "hogar"
tmodulo2_2018$upm_dis <- str_pad(tmodulo2_2018$upm_dis, width = 5, side = "left", pad = "0")
tmodulo2_2018$viv_sel <- str_pad(tmodulo2_2018$viv_sel, width = 3, side = "left", pad = "0")
tmodulo2_2018$hogar <- str_pad(tmodulo2_2018$hogar, width = 2, side = "left", pad = "0")

# Crear una nueva columna con la concatenacion de las columnas UPM_DIS, VIV_SEL y HOGAR
tmodulo2_2018 <- tmodulo2_2018 %>%
  mutate(uvh = paste(upm_dis, viv_sel, hogar, sep = ""))

# Verificar 
tmodulo2_2018$uvh
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

tmodulo1_2018$grupo_edad <- sapply(tmodulo1_2018$edad, grupos_edad_2018)
#_______________________________________________________________________________

# Región
#_______________________________________________________________________________
# Función para asignar grupos de región
grupos_region <- function(reg) {
  if (reg == 1) {
    return("Noroeste")
  } else if (reg == 2) {
    return("Noreste")
  } else if (reg == 3) {
    return("Occidente y Bajgráficoo")
  } else if (reg == 4) {
    return("Ciudad de Mgráficoxico")
  } else if (reg == 5) {
    return("Centro Sur y Oriente")
  } else if (reg == 6) {
    return("Sur")
  } else {
    return("Otro")
  }
}

# Crear nueva columna "region" para crear los grupos de regiones:
tmodulo1_2018$region <- sapply(tmodulo1_2018$region, grupos_region)  
#_______________________________________________________________________________

# Rural-Urbana
#_______________________________________________________________________________
tmodulo1_2018 <- tmodulo1_2018 %>%
  mutate(tloc = case_when(
    tloc == 1 ~ 4,
    tloc == 2 ~ 3,
    tloc == 3 ~ 2,
    tloc == 4 ~ 1
  ))

# Diccionario de etiquetas para TLOC
labels <- c("<2,500 habs", "2,500-14,999 habs", "15,00-99,999 habs", ">100,000 habs")
levels(tmodulo1_2018$tloc) <- labels

# Etiqueta de variable para TLOC
attr(tmodulo1_2018$tloc, "label") <- "Tamaño de localidad"

# Generamos variable de area para obtener las rurales y urbanas

tmodulo1_2018 <- tmodulo1_2018 %>%
  mutate(AREA = ifelse(tloc %in% c(1, 2), 1, 0))

# Etiquetas para area
labels_area <- c("Rural", "Urbana")
levels(tmodulo1_2018$AREA) <- labels_area

# Etiqueta de variable para rural
attr(tmodulo1_2018$AREA, "label") <- "Poblacion rural y urbana 2018"

#_______________________________________________________________________________

# Sexo
#_______________________________________________________________________________
# Definir etiquetas para la variables
tmodulo1_2018$sexo<- factor(tmodulo1_2018$sexo, labels = c("Hombre", "Mujer"))
#_______________________________________________________________________________


# 1) Nivel de escolaridad de la persona elegida (P3_1_1)
#_______________________________________________________________________________
tmodulo1_2018$niv_ed_dgasf <- 0

# Generar la variable niv_ed_dgasf
tmodulo1_2018 <- tmodulo1_2018 %>%
  mutate(niv_ed_dgasf = NA) %>%
  mutate(niv_ed_dgasf = ifelse((niv >= 0 & niv <= 2) | niv == 99, 1, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(niv >= 3 & niv <= 4, 2, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(niv >= 5 & niv <= 7, 3, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(niv >= 8 & niv <= 9, 4, niv_ed_dgasf))


# Etiquetas para la nueva variable
levels(tmodulo1_2018$niv_ed_dgasf) <- c(
  "Hasta primaria",
  "Hasta secundaria",
  "Hasta nivel medio superior",
  "Licenciatura o mas"
)

# Etiquetar la variable niv_ed_dgasf
attr(tmodulo1_2018$niv_ed_dgasf, "label") <- "Nivel educativo"

#Vemos las columnasque tiene la columna niv_ed_dgasf
var_niv_ed_dgasf <- table(tmodulo1_2018$niv_ed_dgasf)
print(var_niv_ed_dgasf)
tmodulo1_2018$niv_ed_dgasf

#_______________________________________________________________________________

# 2) Estado conyugal de la persona elegida (P3_2)
#_______________________________________________________________________________

# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p3_2"] <- "edo_civ"

# Etiqueta 
attr(tmodulo1_2018$edo_civ, "label") <- "Estado civil"

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
tmodulo1_2018$edo_civ <- sapply(tmodulo1_2018$edo_civ, estado_civil)

# Crear variable casado
tmodulo1_2018$casado <- ifelse(tmodulo1_2018$edo_civ == 1, 1, 0)

# Etiquetar variable casado
attr(tmodulo1_2018$casado, "label") <- "¿Está casado o en unión libre?"
levels(tmodulo1_2018$casado) <- c("No", "Sí")

tmodulo1_2018$edo_civ
tmodulo1_2018$casado 
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
tmodulo1_2018$ocup <- 0

# Condiciones
tmodulo1_2018$ocup <- ifelse(tmodulo1_2018$p3_5 == 1 | tmodulo1_2018$p3_5 == 2, 1, ifelse(tmodulo1_2018$p3_5 == 3, 0, NA))

tmodulo1_2018$ocup[tmodulo1_2018$p3_6 >= 1 & tmodulo1_2018$p3_6 < 6] <- 1
tmodulo1_2018$ocup[tmodulo1_2018$p3_7 == 1] <- 0

# Etiquetas para la variable 'ocup'
levels(tmodulo1_2018$ocup) <- c("Desocupado", "Ocupado")

#-------------------------------------------------------------------------------
# Crear una variable 'trab'
tmodulo1_2018$trab <- tmodulo1_2018$ocup == 1

# Etiqueta para la variable 'trab'
levels(tmodulo1_2018$trab) <- c("No", "Sí")
#-------------------------------------------------------------------------------
# Crear una variable 'pea'
tmodulo1_2018$pea <- tmodulo1_2018$p3_5 == 1 | tmodulo1_2018$p3_5 == 2 | tmodulo1_2018$p3_5 == 3

# Etiquetas para la variable 'pea'
levels(tmodulo1_2018$pea) <- c("PNEA", "PEA")
#-------------------------------------------------------------------------------
#Vemos las variables
var_ocup <- table(tmodulo1_2018$ocup)
var_trab <- table(tmodulo1_2018$trab)
var_pea <- table(tmodulo1_2018$pea)

# Etiquetar variables
attr(tmodulo1_2018$ocup, "label") <- "Situacion laboral"
attr(tmodulo1_2018$trab, "label") <- "Encuestado trabajo remunerado"
attr(tmodulo1_2018$pea, "label") <- "Poblacion economicamente activa"

var_ocup
var_trab
var_pea

tmodulo1_2018$ocup
tmodulo1_2018$trab
tmodulo1_2018$pea
#_______________________________________________________________________________

# 5) Posicion laboral
#_______________________________________________________________________________
# Monto total de ingresos por trabajo de la persona elegida (p3_8a)
# Periodo de ingreso por trabajo de la persona elegida (p3_8b)
var_p3_8a <- table(tmodulo1_2018$p3_8a)


# Crear la variable 'Ingmen' en el tmodulo1_2018frame tmodulo1_2018
tmodulo1_2018$Ingmen <- with(tmodulo1_2018, ifelse(p3_8a == 0, 0,
                                                   ifelse(p3_8b == 1 & p3_8a != 99888, p3_8a * 4,
                                                          ifelse(p3_8b == 2 & p3_8a != 99888, p3_8a * 2,
                                                                 ifelse(p3_8b == 3 & p3_8a != 99888, p3_8a,
                                                                        ifelse(p3_8b == 4 & p3_8a != 99888, p3_8a / 12, NA))))))

# Etiquetas para la variable 'pos_ocu'
attr(tmodulo1_2018$Ingmen, "label") <- "Ingreso mensual, pesos"
tmodulo1_2018$Ingmen
#_______________________________________________________________________________

# 6) Ingreso fijo o variable
#_______________________________________________________________________________
# Crear la variable 'ing_fijo' 
tmodulo1_2018$ing_fijo <- ifelse(tmodulo1_2018$p3_9 == 1, 1,
                                 ifelse(tmodulo1_2018$p3_9 == 2, 0, NA))

# Etiqueta para la variable 'ing_fijo'
attr(tmodulo1_2018$ing_fijo, "label") <- "Ingreso fijo o variable"

# Definir etiquetas específicas
levels(tmodulo1_2018$ing_fijo) <- c("Ingreso variable", "Ingreso fijo")

#Vemos las variables
var_ing_fijo <- table(tmodulo1_2018$ing_fijo)
tmodulo1_2018$ing_fijo
#_______________________________________________________________________________

# 7) Acceso a servicios medicos
#_______________________________________________________________________________
# Derechohabiencia de la persona elegida (p3_12)
# Crear la variable 'Serv_med' 
tmodulo1_2018$Serv_med <- ifelse(tmodulo1_2018$p3_11 >= 1 & tmodulo1_2018$p3_11 < 6, TRUE, FALSE)

# Etiqueta para la variable 'ing_fijo'
attr(tmodulo1_2018$Serv_med, "label") <- "Tiene acceso a servicios médicos"

# Definir etiquetas específicas
levels(tmodulo1_2018$Serv_med) <- c("No", "Sí")
tmodulo1_2018$Serv_med 
#_______________________________________________________________________________

# 8) Trabajador formal
#_______________________________________________________________________________
# Crear la variable 'formal' 
tmodulo1_2018$formal <- ifelse(tmodulo1_2018$trab == TRUE, 1,
                               ifelse(!is.na(tmodulo1_2018$formal) & tmodulo1_2018$Serv_med == TRUE, 1, 0))

# Etiqueta para la variable 'formal'
attr(tmodulo1_2018$formal, "label") <- "Trabajador formal"

# Definir etiquetas específicas
levels(tmodulo1_2018$formal) <- c("Informal", "Formal")
tmodulo1_2018$formal

#Vemos las variables
var_formal <- table(tmodulo1_2018$formal)
var_formal

#_______________________________________________________________________________

# 9) Tiene telefono inteligente
#_______________________________________________________________________________
# Tenencia de celular (p3_12)
# Crear la variable 'celular' 
tmodulo1_2018$celular <- ifelse(tmodulo1_2018$p3_12 == 2, 0, tmodulo1_2018$p3_12)

# Etiqueta para la variable 'celular'
attr(tmodulo1_2018$celular, "label") <- "Tiene celular inteligente"

# Definir etiquetas específicas
levels(tmodulo1_2018$celular) <- c("No", "Sí")
tmodulo1_2018$celular

#Vemos las variables
var_celular <- table(tmodulo1_2018$celular)
var_celular
#####################################################################################

# ACTITUDES Y COMPORTAMIENTOS FINANCIEROS

#####################################################################################
#_______________________________________________________________________________

# 10) Lleva presupuesto
#_______________________________________________________________________________
# Crear la variable 'presup' 
tmodulo1_2018$presup <- ifelse(tmodulo1_2018$p4_1 == 2, 0, tmodulo1_2018$p4_1)

# Etiqueta para la columna presup en tmodulo1_2018
attr(tmodulo1_2018$presup, "label") <- "Lleva un presupuesto"

# Definir etiquetas específicas
levels(tmodulo1_2018$presup) <- c("No", "Sí")
tmodulo1_2018$presup
#_______________________________________________________________________________

# 11) Control de gastos                                                       
#_______________________________________________________________________________
#Formas de registro (P4_2) de gastos
#Renombrar variables de la columna
tmodulo1_2018$contr_comput <- ifelse(tmodulo1_2018$p4_2 == 1, 1, 0)
tmodulo1_2018$contr_anogas <- ifelse(tmodulo1_2018$p4_2 == 2, 1, 0)
tmodulo1_2018$contr_mental <- ifelse(tmodulo1_2018$p4_2 == 3, 1, 0)
tmodulo1_2018$contr_aplcel <- ifelse(tmodulo1_2018$p4_2 == 4, 1, 0)

# Etiquetas para las columnas
attr(tmodulo1_2018$contr_comput, "label") <- "Anota sus gastos en computadora"
attr(tmodulo1_2018$contr_anogas, "label") <- "Anota sus gastos en papel"
attr(tmodulo1_2018$contr_mental, "label") <- "registro de gastos mentalmente"
attr(tmodulo1_2018$contr_aplcel, "label") <- "registro de gastos en aplicación móvil"

#_______________________________________________________________________________

# 12) Sobregasto__
#_______________________________________________________________________________
tmodulo1_2018$sobregas <- ifelse(tmodulo1_2018$p4_5 == 1, 0,
                                 ifelse(tmodulo1_2018$p4_5 == 2, 1, NA))

# Definir etiquetas específicas con set_label
levels(tmodulo1_2018$sobregas) <- c("No", "Sí")

# Etiqueta para la variable sobregas en tmodulo1_2018
attr(tmodulo1_2018$sobregas, "label") <- "Ingreso insuficiente para cubrir gastos en algún mes"

#_______________________________________________________________________________

# 13) Accion despues de no cubrir gastos
#_______________________________________________________________________________
# Crear columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_6_1"] <- "sobregas_pfa"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_6_2"] <- "sobregas_aho"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_6_3"] <- "sobregas_bie"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_6_4"] <- "sobregas_ade"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_6_5"] <- "sobregas_cre"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_6_6"] <- "sobregas_atr"

#Renombrar variables de la columna
tmodulo1_2018$sobregas_pfa <- ifelse(tmodulo1_2018$sobregas_pfa == 1, 1, 0)
tmodulo1_2018$sobregas_aho <- ifelse(tmodulo1_2018$sobregas_aho == 1, 1, 0)
tmodulo1_2018$sobregas_bie <- ifelse(tmodulo1_2018$sobregas_bie == 1, 1, 0)
tmodulo1_2018$sobregas_ade <- ifelse(tmodulo1_2018$sobregas_ade == 1, 1, 0)
tmodulo1_2018$sobregas_cre <- ifelse(tmodulo1_2018$sobregas_cre == 1, 1, 0)
tmodulo1_2018$sobregas_atr <- ifelse(tmodulo1_2018$sobregas_atr == 1, 1, 0)

# Etiquetas para las columnas
attr(tmodulo1_2018$sobregas_pfa, "label") <- "Pidió préstamo a familiares o conocidos para cubrir gastos"
attr(tmodulo1_2018$sobregas_aho, "label") <- "Utilizó ahorros para cubrir gastos"
attr(tmodulo1_2018$sobregas_bie, "label") <- "Empeñó o vendió bienes para cubrir gastos"
attr(tmodulo1_2018$sobregas_ade, "label") <- "Solicitó adelanto de salario para cubrir gastos"
attr(tmodulo1_2018$sobregas_cre, "label") <- "Solicitó crédito o utilizó tarjeta para cubrir gastos"
attr(tmodulo1_2018$sobregas_atr, "label") <- "Se atrasó en crédito para afrontar sobregasto"

#-----------------------------------------------------------------------------
# Crear la variable sobregas_aho18 
tmodulo1_2018$sobregas_aho18 <- ifelse(tmodulo1_2018$sobregas_aho == 1, 1, 0)

# Reemplazar valores NA
tmodulo1_2018$sobregas_aho18[is.na(tmodulo1_2018$sobregas_aho18) & tmodulo1_2018$sobregas == 0] <- 0

# Etiqueta para la variable 
attr(tmodulo1_2018$sobregas_aho18, "label") <- "Redujo gastos o utilizó ahorro para afrontar sobregasto"

# Definir etiquetas específicas
levels(tmodulo1_2018$sobregas_aho18) <- c("No", "Sí")
tmodulo1_2018$sobregas_aho18
#_______________________________________________________________________________

# 14) Cursos sobre temas financieros
#_______________________________________________________________________________
#  Variable curso_fin 
tmodulo1_2018$curso_fin <- ifelse(tmodulo1_2018$p4_7 == 2, 0, tmodulo1_2018$p4_7)

# Definir etiquetas específicas
levels(tmodulo1_2018$curso_fin) <- c("No", "Sí")

# Etiqueta para la variable curso_fin en tmodulo1_2018
attr(tmodulo1_2018$curso_fin, "label") <- "Tomó curso de educación financiera"
tmodulo1_2018$curso_fin

#_______________________________________________________________________________

# 15) Comportamiento financiero
#_______________________________________________________________________________
#  variable 
codificar_comp_pga <- function(valor) {
  if (valor == 1) {
    return(2)
  } else if (valor %in% c(2, 8, 9)) {
    return(1)
  } else if (valor == 3) {
    return(0)
  } else {
    return(NA)  
  }
}

# Crear una nueva variable comp_pga2 
tmodulo1_2018$comp_pga2 <- sapply(tmodulo1_2018$p4_8_3, codificar_comp_pga)

# Definir etiquetas específicas
levels(tmodulo1_2018$comp_pga2) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
tmodulo1_2018$comp_pga2


#-----------------------------------------------------------------------------
# renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_8_1"] <- "comp_ant"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_8_2"] <- "comp_pat"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_8_3"] <- "comp_pga"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p4_8_4"] <- "comp_mlp"


# Función para recodificar las variables de las columnas
Comportamiento_financiero <- function(valor) {
  if (valor == 1) {
    return(2)
  } else if (valor == 2) {
    return(1)
  } else if (valor %in% c(3, 8, 9)) {
    return(0)
  } else {
    return(valor)
  }
}

# Aplicar la función a las columnas
tmodulo1_2018$comp_ant <- sapply(tmodulo1_2018$comp_ant, Comportamiento_financiero)
tmodulo1_2018$comp_pat <- sapply(tmodulo1_2018$comp_pat, Comportamiento_financiero)
tmodulo1_2018$comp_pga <- sapply(tmodulo1_2018$comp_pga, Comportamiento_financiero)
tmodulo1_2018$comp_mlp <- sapply(tmodulo1_2018$comp_mlp, Comportamiento_financiero)

# Etiquetas para las variables
attr(tmodulo1_2018$comp_ant, "label") <- "Piensa si puede pagar antes de comprar"
attr(tmodulo1_2018$comp_pat, "label") <- "Paga cuentas a tiempo"
attr(tmodulo1_2018$comp_pga, "label") <- "Prefiere gastar antes de ahorrar"
attr(tmodulo1_2018$comp_mlp, "label") <- "Se pone metas económicas de largo plazo"

# Etiquetas para las variables
levels(tmodulo1_2018$comp_ant) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
levels(tmodulo1_2018$comp_pat) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
levels(tmodulo1_2018$comp_pga) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
levels(tmodulo1_2018$comp_mlp) <- c("Nunca / No sabe", "Algunas veces", "Siempre")


#_______________________________________________________________________________

# 16) Riesgos y diversificacion
#_______________________________________________________________________________
# Crear nuevas columnas
tmodulo1_2018$cono_inf <- ifelse(tmodulo1_2018$p4_9_1 == 1, 1, 0)
tmodulo1_2018$cono_rie <- ifelse(tmodulo1_2018$p4_9_2 == 1, 1, 0)
tmodulo1_2018$cono_div <- ifelse(tmodulo1_2018$p4_9_3 == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo1_2018$cono_inf, "label") <- "Conoce concepto de inflación"
attr(tmodulo1_2018$cono_rie, "label") <- "Conoce concepto de riesgo inversión"
attr(tmodulo1_2018$cono_div, "label") <- "Conoce concepto de diversificación de riesgo"

# Definir etiquetas específicas
levels(tmodulo1_2018$cono_inf) <- c("No", "Sí")
levels(tmodulo1_2018$cono_rie) <- c("No", "Sí")
levels(tmodulo1_2018$cono_div) <- c("No", "Sí")

tmodulo1_2018$cono_inf
tmodulo1_2018$cono_rie
tmodulo1_2018$cono_div
#_______________________________________________________________________________

# 17) Otros comportamientos   
# No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 18) Aprovechar oportunidades
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 19) Vulnerabilidad financiera
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________
#####################################################################################

# Ahorro formal e informal

#####################################################################################

#_______________________________________________________________________________

# 20) Tiene ahorro informal
#_______________________________________________________________________________
# Crear variable aho_inf e inicializarla con 0
tmodulo1_2018$aho_inf <- 0

# Actualizar aho_inf a 1 si se cumple alguna de las condiciones
tmodulo1_2018$aho_inf[tmodulo1_2018$p5_1_1 == 1 | tmodulo1_2018$p5_1_2 == 1 | tmodulo1_2018$p5_1_3 == 1 |
                        tmodulo1_2018$p5_1_4 == 1 | tmodulo1_2018$p5_1_5 == 1 | tmodulo1_2018$p5_1_6 == 1] <- 1

# Etiqueta
attr(tmodulo1_2018$aho_inf, "label") <- "Tiene ahorro informal (tandas, familiares, cajas de ahorro)"

#_______________________________________________________________________________

# 21) Tipo de ahorro informal
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_1_1"] <- "aho_inf_pre"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_1_2"] <- "aho_inf_bie"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_1_3"] <- "aho_inf_caj"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_1_4"] <- "aho_inf_fam"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_1_5"] <- "aho_inf_tan"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_1_6"] <- "aho_inf_cas"

#Renombrar variables de la columna
tmodulo1_2018$aho_inf_pre <- ifelse(tmodulo1_2018$aho_inf_pre == 1, 1, 0)
tmodulo1_2018$aho_inf_bie <- ifelse(tmodulo1_2018$aho_inf_bie == 1, 1, 0)
tmodulo1_2018$aho_inf_caj <- ifelse(tmodulo1_2018$aho_inf_caj == 1, 1, 0)
tmodulo1_2018$aho_inf_fam <- ifelse(tmodulo1_2018$aho_inf_fam == 1, 1, 0)
tmodulo1_2018$aho_inf_tan <- ifelse(tmodulo1_2018$aho_inf_tan == 1, 1, 0)
tmodulo1_2018$aho_inf_cas <- ifelse(tmodulo1_2018$aho_inf_cas == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo1_2018$aho_inf_pre, "label") <- "Ahorro prestando dinero"
attr(tmodulo1_2018$aho_inf_bie, "label") <- "Ahorro comprando animales o bienes"
attr(tmodulo1_2018$aho_inf_caj, "label") <- "Ahorro en caja del trabajo o conocidos"
attr(tmodulo1_2018$aho_inf_fam, "label") <- "Ahorro guardando con familiares o conocidos"
attr(tmodulo1_2018$aho_inf_tan, "label") <- "Ahorro participando en tanda"
attr(tmodulo1_2018$aho_inf_cas, "label") <- "Ahorro dinero en su casa"

#_______________________________________________________________________________

# 22) Destino del ahorro informal
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_1"] <- "aho_inf_u_gas"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_2"] <- "aho_inf_u_eme"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_3"] <- "aho_inf_u_cas"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_4"] <- "aho_inf_u_sal"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_5"] <- "aho_inf_u_edu"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_6"] <- "aho_inf_u_vac"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_7"] <- "aho_inf_u_neg"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_8"] <- "aho_inf_u_ret"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_2_9"] <- "aho_inf_u_otr"

# Etiquetas para las variables
attr(tmodulo1_2018$aho_inf_u_gas, "label") <- "Uso en gastos personales"
attr(tmodulo1_2018$aho_inf_u_eme, "label") <- "Uso en emergencias"
attr(tmodulo1_2018$aho_inf_u_cas, "label") <- "Uso en compra casa, terreno, remod"
attr(tmodulo1_2018$aho_inf_u_sal, "label") <- "Uso en gastos salud"
attr(tmodulo1_2018$aho_inf_u_edu, "label") <- "Uso en gastos educacion"
attr(tmodulo1_2018$aho_inf_u_vac, "label") <- "Uso en vacaciones y fiestas"
attr(tmodulo1_2018$aho_inf_u_neg, "label") <- "Uso en negocio"
attr(tmodulo1_2018$aho_inf_u_ret, "label") <- "Uso en vejez o retiro"
attr(tmodulo1_2018$aho_inf_u_otr, "label") <- "Uso en otro"

#_______________________________________________________________________________

# 23) Equivalencia del ahorro informal
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 24) Tiene cuenta bancaria o con institución financiera
#_______________________________________________________________________________

tmodulo1_2018$cuenta  <- ifelse(
  tmodulo1_2018$p5_9_1 == 1 | tmodulo1_2018$p5_9_2 == 1 |
    tmodulo1_2018$p5_9_3 == 1 | tmodulo1_2018$p5_9_4 == 1 |
    tmodulo1_2018$p5_9_5 == 1 | tmodulo1_2018$p5_9_6 == 1 |
    tmodulo1_2018$p5_9_7 == 1 | tmodulo1_2018$p5_9_8 == 1, 1, 0)

# Etiqueta 
attr(tmodulo1_2018$cuenta, "label") <- "Tiene alguna cuenta"
#_______________________________________________________________________________

# 24) Tipos de cuentas
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_9_1"] <- "cuenta_nom"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_9_2"] <- "cuenta_pen"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_9_3"] <- "cuenta_gob"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_9_4"] <- "cuenta_aho"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_9_5"] <- "cuenta_che"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_9_6"] <- "cuenta_pla"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_9_7"] <- "cuenta_inv"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_9_8"] <- "cuenta_otr"

# Etiquetas para las variables
attr(tmodulo1_2018$cuenta_nom, "label") <- "Tiene cuenta de nómina"
attr(tmodulo1_2018$cuenta_pen, "label") <- "Tiene cuenta de pensión"
attr(tmodulo1_2018$cuenta_gob, "label") <- "Tiene cuenta de apoyos de gobierno"
attr(tmodulo1_2018$cuenta_aho, "label") <- "Tiene cuenta de ahorro"
attr(tmodulo1_2018$cuenta_che, "label") <- "Tiene cuenta de cheques"
attr(tmodulo1_2018$cuenta_pla, "label") <- "Tiene depósito a plazo fijo"
attr(tmodulo1_2018$cuenta_inv, "label") <- "Tiene cuenta de inversión"
attr(tmodulo1_2018$cuenta_otr, "label") <- "Tiene cuenta de otro tipo"

# Crear variables
tmodulo1_2018$cuenta_invpla <- ifelse(tmodulo1_2018$cuenta_inv == 1 | tmodulo1_2018$cuenta_pla == 1, 1, 0)
tmodulo1_2018$cuenta_ahoche <- ifelse(tmodulo1_2018$cuenta_aho == 1 | tmodulo1_2018$cuenta_che == 1, 1, 0)
tmodulo1_2018$cuenta_otrgob <- ifelse(tmodulo1_2018$cuenta_pen == 1 | tmodulo1_2018$cuenta_gob == 1 | tmodulo1_2018$cuenta_otr == 1, 1, 0)

# Etiquetas para las variables
attr(tmodulo1_2018$cuenta_invpla, "label") <- "Tiene cuenta de inversión o a plazos"
attr(tmodulo1_2018$cuenta_ahoche, "label") <- "Tiene cuenta de ahorro o de cheques"
attr(tmodulo1_2018$cuenta_otrgob, "label") <- "Tiene otro tipo de cuenta (pensión, gobierno, otros)"

#_______________________________________________________________________________

# 25) Tarjeta de debito
#_______________________________________________________________________________
# Crear variable tarj_deb e inicializarla con 0
tmodulo1_2018$tarj_deb <- 0

# Actualizar tarj_deb a 1 si se cumple alguna de las condiciones
tmodulo1_2018$tarj_deb[tmodulo1_2018$p5_12_1 == 1 | tmodulo1_2018$p5_12_2 == 1 |
                         tmodulo1_2018$p5_12_3 == 1 | tmodulo1_2018$p5_12_4 == 1 |
                         tmodulo1_2018$p5_12_5 == 1] <- 1

# Etiqueta para la variable tarj_deb en tmodulo1_2018
attr(tmodulo1_2018$tarj_deb, "label") <- "Tiene tarjeta de débito"

#_______________________________________________________________________________

# 26) Ahorro formal efectivo
#_______________________________________________________________________________
# Crear variable aho_for e inicializarla con 0
tmodulo1_2018$aho_for <- 0

# Actualizar aho_for a 1 si se cumple alguna de las condiciones
tmodulo1_2018$aho_for[tmodulo1_2018$p5_13_1 == 1 | tmodulo1_2018$p5_13_2 == 1 |
                        tmodulo1_2018$p5_13_3 == 1 | tmodulo1_2018$p5_13_4 == 1 |
                        tmodulo1_2018$p5_13_5 == 1 | tmodulo1_2018$p5_13_6 == 1 |
                        tmodulo1_2018$p5_13_7 == 1 | tmodulo1_2018$p5_13_8 == 1 ] <- 1

# Etiqueta 
attr(tmodulo1_2018$aho_for, "label") <- "Tiene ahorro formal"

#_______________________________________________________________________________

# 27) Tipo de ahorro formal
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_13_1"] <- "aho_for_nom"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_13_2"] <- "aho_for_pen"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_13_3"] <- "aho_for_gob"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_13_4"] <- "aho_for_aho"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_13_5"] <- "aho_for_che"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_13_6"] <- "aho_for_pla"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_13_7"] <- "aho_for_inv"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_13_8"] <- "aho_for_otr"

# Etiquetas 
attr(tmodulo1_2018$aho_for_nom, "label") <- "Ahorro en cuenta de nómina"
attr(tmodulo1_2018$aho_for_pen, "label") <- "Ahorro en cuenta de pensión"
attr(tmodulo1_2018$aho_for_gob, "label") <- "Ahorro en cuenta de apoyos de gobierno"
attr(tmodulo1_2018$aho_for_aho, "label") <- "Ahorro en cuenta de ahorro"
attr(tmodulo1_2018$aho_for_che, "label") <- "Ahorro en cuenta de cheques"
attr(tmodulo1_2018$aho_for_pla, "label") <- "Ahorro en depósito a plazo fijo"
attr(tmodulo1_2018$aho_for_inv, "label") <- "Ahorro en cuenta de inversión"
attr(tmodulo1_2018$aho_for_otr, "label") <- "Ahorro en cuenta de otro tipo"

# Crear variables adicionales
tmodulo1_2018$aho_for_invpla <- ifelse(tmodulo1_2018$aho_for_inv == 1 | tmodulo1_2018$aho_for_pla == 1, 1, 0)
tmodulo1_2018$aho_for_ahoche <- ifelse(tmodulo1_2018$aho_for_aho == 1 | tmodulo1_2018$aho_for_che == 1, 1, 0)
tmodulo1_2018$aho_for_otrgob <- ifelse(tmodulo1_2018$aho_for_pen == 1 | tmodulo1_2018$aho_for_gob == 1 | tmodulo1_2018$aho_for_otr == 1, 1, 0)

# Etiquetas 
attr(tmodulo1_2018$aho_for_invpla, "label") <- "Tiene cuenta de inversión o a plazos"
attr(tmodulo1_2018$aho_for_ahoche, "label") <- "Tiene cuenta de ahorro o de cheques"
attr(tmodulo1_2018$aho_for_otrgob, "label") <- "Tiene otro tipo de cuenta (pensión, gobierno, otros)"

# Crear variable ahorro
tmodulo1_2018$ahorro <- ifelse(tmodulo1_2018$aho_for == 1 | tmodulo1_2018$aho_inf == 1, 1, 0)

# Etiqueta para la variable ahorro en tmodulo1_2018
attr(tmodulo1_2018$ahorro, "label") <- "Tiene algún tipo de ahorro (formal o informal)"

#_______________________________________________________________________________

# 28) Razones para adquirir cuenta
#No estan estas variables en la encuesta de este año

#_______________________________________________________________________________

# 29) Destino del ahorro formal
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_1"] <- "aho_for_u_eme"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_2"] <- "aho_for_u_gas"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_3"] <- "aho_for_u_cas"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_4"] <- "aho_for_u_edu"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_5"] <- "aho_for_u_vac"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_6"] <- "aho_for_u_sal"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_7"] <- "aho_for_u_neg"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_8"] <- "aho_for_u_ret"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_14_9"] <- "aho_for_u_otr"

# Etiquetas para las variables
attr(tmodulo1_2018$aho_for_u_eme, "label") <- "Uso en emergencias"
attr(tmodulo1_2018$aho_for_u_gas, "label") <- "Uso en gastos personales"
attr(tmodulo1_2018$aho_for_u_cas, "label") <- "Uso en compra casa, terreno, remodelación"
attr(tmodulo1_2018$aho_for_u_edu, "label") <- "Uso en gastos educación"
attr(tmodulo1_2018$aho_for_u_vac, "label") <- "Uso en vacaciones y fiestas"
attr(tmodulo1_2018$aho_for_u_sal, "label") <- "Uso en gastos salud"
attr(tmodulo1_2018$aho_for_u_neg, "label") <- "Uso en negocio"
attr(tmodulo1_2018$aho_for_u_ret, "label") <- "Uso en vejez o retiro"
attr(tmodulo1_2018$aho_for_u_otr, "label") <- "Uso en otro"

#_______________________________________________________________________________

# 30) Equivalencia del ahorro formal
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 31) Sobre cuentas de nomina gratuita
#_______________________________________________________________________________
tmodulo1_2018$cuenta_nom_gra <- ifelse(tmodulo1_2018$p5_15 == 1, 1, 0)

# Etiqueta 
attr(tmodulo1_2018$cuenta_nom_gra, "label") <- "Sabe que puede cambiar por cuenta de nómina gratuita"

#_______________________________________________________________________________

# 32) Frecuencia de uso de tarjeta de debito
#_______________________________________________________________________________
# Frecuencia de uso de tarjeta de débito
tmodulo1_2018$tarj_deb_frec <- tmodulo1_2018$p5_18

# Etiqueta 
attr(tmodulo1_2018$tarj_deb_frec, "label") <- "Frecuencia uso mensual de tarjeta de débito"


tmodulo1_2018$tarj_deb_frecg <- NA
tmodulo1_2018$tarj_deb_frecg[tmodulo1_2018$tarj_deb_frec == 0] <- 1
tmodulo1_2018$tarj_deb_frecg[tmodulo1_2018$tarj_deb_frec >= 1 & tmodulo1_2018$tarj_deb_frec < 10 | tmodulo1_2018$tarj_deb_frec == 88] <- 2
tmodulo1_2018$tarj_deb_frecg[tmodulo1_2018$tarj_deb_frec >= 10 & tmodulo1_2018$tarj_deb_frec != 88 & !is.na(tmodulo1_2018$tarj_deb_frec)] <- 3

# Cambiar nombres de niveles para tarj_deb_frecg
set_label(tmodulo1_2018$tarj_deb_frecg, c("1" = "No la utiliza", "2" = "Menos de 10 veces al mes", "3" = "10 veces al mes o más"))

# Etiqueta 
attr(tmodulo1_2018$tarj_deb_frecg, "label") <- "Frecuencia uso mensual de tarjeta de débito"


#_______________________________________________________________________________

# 33) Razones de no uso de tarjetas de debito
#_______________________________________________________________________________
tmodulo1_2018$tarj_deb_raznu <- tmodulo1_2018$p5_19

# Etiqueta de variables de la columna
set_label(tmodulo1_2018$tarj_deb_raznu, c(
  "1" = "Prefiere pagar efectivo",
  "2" = "Desconfianza",
  "3" = "No lo aceptan",
  "4" = "Prefiere tarjeta de crédito",
  "5" = "Montos bajos",
  "6" = "No lleva control gastos",
  "7" = "No sabe que puede usar",
  "8" = "Establecimiento cobra comisiones",
  "9" = "Otro",
  "10" = "No puede comprar con la tarjeta"
))

# Etiqueta
attr(tmodulo1_2018$tarj_deb_raznu, "label") <- "Razon principal para no usar tarjeta de débito"
#_______________________________________________________________________________

# 34) Preferencia efectivo
#_______________________________________________________________________________
# Preferencia efectivo
tmodulo1_2018$pref_efec <- tmodulo1_2018$p5_20

# Etiqueta para la variable 'pref_efec' con set_label y set_names
set_label(tmodulo1_2018$pref_efec, c(
  "1" = "Solo aceptan efectivo en establecimientos",
  "2" = "Montos bajos",
  "3" = "Permite llevar control gastos",
  "4" = "Desconfianza en débito",
  "5" = "Por costumbre",
  "6" = "Establecimiento cobra comisiones",
  "7" = "Otro"
))

# Etiquetza
attr(tmodulo1_2018$pref_efec, "label") <- "Razon principal preferir efectivo"

#_______________________________________________________________________________

# 35) Solo recibe apoyo gobierno
#_______________________________________________________________________________
# Crear la variable cuenta_gob_unica
tmodulo1_2018$cuenta_gob_unica <- ifelse(tmodulo1_2018$cuenta_gob == 1, 1, 0)

# Etiqueta 
attr(tmodulo1_2018$cuenta_gob_unica, "label") <- "Solo tiene cuenta gobierno"

#_______________________________________________________________________________

# 36) Comparacion de productos ahorro
#_______________________________________________________________________________
# Recodificar la columna
tmodulo1_2018$compara_cuenta <- ifelse(tmodulo1_2018$p5_21 == 1, 1, 0)

# Etiqueta 
attr(tmodulo1_2018$compara_cuenta, "label") <- "Compara productos de ahorro antes de contratar"

#_______________________________________________________________________________

# 37) Medios e comparacion de cuentas
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_22_1"] <- "compara_cuenta_ins"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_22_2"] <- "compara_cuenta_ami"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_22_3"] <- "compara_cuenta_con"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_22_4"] <- "compara_cuenta_anu"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_22_5"] <- "compara_cuenta_esp"

#Renombrar variables de la columna
tmodulo1_2018$compara_cuenta_ins <- ifelse(tmodulo1_2018$compara_cuenta_ins == 1, 1, 0)
tmodulo1_2018$compara_cuenta_ami <- ifelse(tmodulo1_2018$compara_cuenta_ami == 1, 1, 0)
tmodulo1_2018$compara_cuenta_con <- ifelse(tmodulo1_2018$compara_cuenta_con == 1, 1, 0)
tmodulo1_2018$compara_cuenta_anu <- ifelse(tmodulo1_2018$compara_cuenta_anu == 1, 1, 0)
tmodulo1_2018$compara_cuenta_esp <- ifelse(tmodulo1_2018$compara_cuenta_esp == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo1_2018$compara_cuenta_ins, "label") <- "Compara con info dada por banco"
attr(tmodulo1_2018$compara_cuenta_ami, "label") <- "Compara con recomendacion amistades"
attr(tmodulo1_2018$compara_cuenta_con, "label") <- "Compara con paginas Condusef o Banxico"
attr(tmodulo1_2018$compara_cuenta_anu, "label") <- "Compara con anuncios comerciales"
attr(tmodulo1_2018$compara_cuenta_esp, "label") <- "Compara con recomendacion de especialistas"

# Crear la variable compara_cuenta_cfo
tmodulo1_2018$compara_cuenta_cfo <- NA
tmodulo1_2018$compara_cuenta_cfo[tmodulo1_2018$compara_cuenta_con == 1 | tmodulo1_2018$compara_cuenta_esp == 1] <- 1
tmodulo1_2018$compara_cuenta_cfo[tmodulo1_2018$compara_cuenta == 1 & is.na(tmodulo1_2018$compara_cuenta_cfo)] <- 0

#_______________________________________________________________________________

# 38) Donde contrato cuenta
#No estan estas variables en la encuesta de este año

#_______________________________________________________________________________
# 39) Medios para hacer movimientos en cuentas o checar saldos
#No estan estas variables en la encuesta de este año

#_______________________________________________________________________________

# 40) Poblacion ex-usuaria de cuentas
#_______________________________________________________________________________
# Poblacion ex-usuaria de cuentas
tmodulo1_2018$cuenta_ex <- ifelse(tmodulo1_2018$p5_6 == 1, 1, 0)

# Etiqueta para la variable 'cuenta_ex'
attr(tmodulo1_2018$cuenta_ex, "label") <- "Habia tenido cuenta, tarjeta de banco o de apoyo de gob"
#_______________________________________________________________________________

# 41) Razon para no tener cuenta
#_______________________________________________________________________________
# Crear la variable cuenta_nt_raz
tmodulo1_2018$cuenta_nt_raz <- tmodulo1_2018$p5_7

# Reemplazar los valores 99 con missing
tmodulo1_2018$cuenta_nt_raz[tmodulo1_2018$cuenta_nt_raz == 99] <- 0

# Etiquetas para la nueva variable
levels(tmodulo1_2018$cuenta_nt_raz) <- c(
  "Sucursal lejana o no hay",
  "Intereses bajos/Comisiones altas",
  "No confia inst fin",
  "Piden requisitos",
  "Prefiere otras formas de ahorrar",
  "No la necesita",
  "Ingresos insuficientes o variables",
  "No sabe como usarla",
  "Otro"
)

# Etiqueta para la variable 'cuenta_nt_raz'
attr(tmodulo1_2018$cuenta_nt_raz, "label") <- "Principal razon para no tener cuenta o tarjeta"

#_______________________________________________________________________________

# 42) Razon para dejar de tener cuenta
#_______________________________________________________________________________
# Razon para dejar de tener cuenta
tmodulo1_2018$cuenta_ex_raz <- tmodulo1_2018$p5_8
tmodulo1_2018$cuenta_ex_raz[tmodulo1_2018$cuenta_ex_raz == 99 ] <- 0
# Etiquetas para la nueva variable
levels(tmodulo1_2018$cuenta_ex_raz) <- c(
  "Dejo de trabajar y le pagaban ahi",
  "Dejo de recibir apoyo gob",
  "No la utilizaba",
  "Mala experiencia inst fin",
  "No cumplia saldo min / cobro comisiones",
  "Intereses pagados muy bajos",
  "Cerro sucursal o inst fin",
  "Fue victima fraude",
  "Otro"
)

# Etiqueta para la variable 'cuenta_ex_raz'
attr(tmodulo1_2018$cuenta_ex_raz, "label") <- "Principal razon para dejar de cuenta o tarjeta"


#_______________________________________________________________________________

# 43) Conoce proteccion al ahorro bancario
#_______________________________________________________________________________
names(tmodulo1_2018)[names(tmodulo1_2018) == "p5_29"] <- "cono_prot"

# Reemplazar los valores 2 con 0
tmodulo1_2018$cono_prot[tmodulo1_2018$cono_prot == 2] <- 0

# Etiqueta
attr(tmodulo1_2018$cono_prot, "label") <- "Conoce que ahorros están protegidos"

#_______________________________________________________________________________

# 44) Conoce IPAB
#_______________________________________________________________________________
#####################################################################################

# CREDITO FORMAL E INFORMAL

#####################################################################################
#_______________________________________________________________________________

# 45) Credito informal
#_______________________________________________________________________________
# Crear la variable cred_inf
tmodulo1_2018$cred_inf <- ifelse(tmodulo1_2018$p6_1_1 == 1 | tmodulo1_2018$p6_1_2 == 1 
                                 | tmodulo1_2018$p6_1_3 == 1 | tmodulo1_2018$p6_1_4 == 1 
                                 | tmodulo1_2018$p6_1_5 == 1, 1, 0)

# Etiqueta para la variable 'cred_inf'
attr(tmodulo1_2018$cred_inf, "label") <- "Tiene crédito informal"


#_______________________________________________________________________________

# 45) Tipo de credito informal
#_______________________________________________________________________________

# renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_1_1"] <- "cred_caj"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_1_2"] <- "cred_emp"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_1_3"] <- "cred_ami"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_1_4"] <- "cred_fam"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_1_5"] <- "cred_oti"

#Renombrar variables de la columna
tmodulo1_2018$cred_caj <- ifelse(tmodulo1_2018$cred_caj == 1, 1, 0)
tmodulo1_2018$cred_emp <- ifelse(tmodulo1_2018$cred_emp == 1, 1, 0)
tmodulo1_2018$cred_ami <- ifelse(tmodulo1_2018$cred_ami == 1, 1, 0)
tmodulo1_2018$cred_fam <- ifelse(tmodulo1_2018$cred_fam == 1, 1, 0)
tmodulo1_2018$cred_oti <- ifelse(tmodulo1_2018$cred_oti == 1, 1, 0)

# Etiquetas 
attr(tmodulo1_2018$cred_caj, "label") <- "Crédito prestando dinero"
attr(tmodulo1_2018$cred_emp, "label") <- "Crédito casa empeño"
attr(tmodulo1_2018$cred_ami, "label") <- "Crédito en caja del trabajo o conocidos"
attr(tmodulo1_2018$cred_fam, "label") <- "Crédito guardando con familiares o conocidos"
attr(tmodulo1_2018$cred_oti, "label") <- "Crédito participando en tanda"


#_______________________________________________________________________________

# 46) Crédito formal
#_______________________________________________________________________________
# Credito formal
tmodulo1_2018$cred_for <- ifelse(tmodulo1_2018$p6_8_1 == 1 | tmodulo1_2018$p6_8_2 == 1 |
                                   tmodulo1_2018$p6_8_3 == 1 | tmodulo1_2018$p6_8_4 == 1 |
                                   tmodulo1_2018$p6_8_5 == 1 | tmodulo1_2018$p6_8_6 == 1 |
                                   tmodulo1_2018$p6_8_7 == 1 | tmodulo1_2018$p6_8_8 == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo1_2018$cred_for, "label") <- "Tiene crédito formal"
#_______________________________________________________________________________

# 47) Tipo de credito formal
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_8_1"] <- "tarj_dep"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_8_2"] <- "tarj_cred"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_8_3"] <- "cred_nom"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_8_4"] <- "cred_per"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_8_5"] <- "cred_aut"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_8_6"] <- "cred_viv"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_8_7"] <- "cred_gru"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_8_8"] <- "cred_otf"

#Renombrar variables de la columna
tmodulo1_2018$tarj_dep <- ifelse(tmodulo1_2018$tarj_dep == 1, 1, 0)
tmodulo1_2018$tarj_cred <- ifelse(tmodulo1_2018$tarj_cred == 1, 1, 0)
tmodulo1_2018$cred_nom <- ifelse(tmodulo1_2018$cred_nom == 1, 1, 0)
tmodulo1_2018$cred_per <- ifelse(tmodulo1_2018$cred_per == 1, 1, 0)
tmodulo1_2018$cred_aut <- ifelse(tmodulo1_2018$cred_aut == 1, 1, 0)
tmodulo1_2018$cred_viv <- ifelse(tmodulo1_2018$cred_viv == 1, 1, 0)
tmodulo1_2018$cred_gru <- ifelse(tmodulo1_2018$cred_gru == 1, 1, 0)
tmodulo1_2018$cred_otf <- ifelse(tmodulo1_2018$cred_otf == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo1_2018$tarj_dep, "label") <- "Tiene tarjeta departamental"
attr(tmodulo1_2018$tarj_cred, "label") <- "Tiene tarjeta crédito bancaria"
attr(tmodulo1_2018$cred_nom, "label") <- "Tiene crédito nómina"
attr(tmodulo1_2018$cred_per, "label") <- "Tiene crédito personal"
attr(tmodulo1_2018$cred_aut, "label") <- "Tiene crédito automotriz"
attr(tmodulo1_2018$cred_viv, "label") <- "Tiene crédito vivienda"
attr(tmodulo1_2018$cred_gru, "label") <- "Tiene crédito grupal"
attr(tmodulo1_2018$cred_otf, "label") <- "Tiene otro crédito formal"
#_______________________________________________________________________________

# 48) Numero de creditos por tipo
#_______________________________________________________________________________
#Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_9_1"] <- "tarj_dep_n"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_9_2"] <- "tarj_cred_n"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_9_3"] <- "cred_nom_n"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_9_4"] <- "cred_per_n"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_9_5"] <- "cred_aut_n"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_9_6"] <- "cred_viv_n"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_9_7"] <- "cred_gru_n"

# Etiquetas para el número de tarjetas y créditos
attr(tmodulo1_2018$tarj_dep_n, "label") <- "No. tarjetas departamental"
attr(tmodulo1_2018$tarj_cred_n, "label") <- "No. tarjetas crédito bancaria"
attr(tmodulo1_2018$cred_nom_n, "label") <- "No. créditos nómina"
attr(tmodulo1_2018$cred_per_n, "label") <- "No. créditos personales"
attr(tmodulo1_2018$cred_aut_n, "label") <- "No. créditos automotrices"
attr(tmodulo1_2018$cred_viv_n, "label") <- "No. créditos vivienda"
attr(tmodulo1_2018$cred_gru_n, "label") <- "No. créditos grupales"

#_______________________________________________________________________________

# 49) Atrasos en creditos
#_______________________________________________________________________________
# Atrasos en créditos
tmodulo1_2018$cred_atr <- ifelse(
  tmodulo1_2018$p6_10_1 == 1 | tmodulo1_2018$p6_10_2 == 1 | tmodulo1_2018$p6_10_3 == 1 | 
    tmodulo1_2018$p6_10_4 == 1 | tmodulo1_2018$p6_10_5 == 1 | tmodulo1_2018$p6_10_6 == 1 | 
    tmodulo1_2018$p6_10_7 == 1, 1, 0)

# Etiqueta 
attr(tmodulo1_2018$cred_atr, "label") <- "Se ha atrasado con pagos crédito formal"

#_______________________________________________________________________________

# 50) Atrasos por tipo de credito 
#_______________________________________________________________________________
#Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10_1"] <- "tarj_dep_atr"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10_2"] <- "tarj_cred_atr"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10_3"] <- "cred_nom_atr"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10_4"] <- "cred_per_atr"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10_5"] <- "cred_aut_atr"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10_6"] <- "cred_viv_atr"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10_7"] <- "cred_gru_atr"


#Renombrar variables de la columna
tmodulo1_2018$tarj_dep_atr <- ifelse(tmodulo1_2018$tarj_dep_atr == 1, 1, 0)
tmodulo1_2018$tarj_cred_atr <- ifelse(tmodulo1_2018$tarj_cred_atr == 1, 1, 0)
tmodulo1_2018$cred_nom_atr <- ifelse(tmodulo1_2018$cred_nom_atr == 1, 1, 0)
tmodulo1_2018$cred_per_atr <- ifelse(tmodulo1_2018$cred_per_atr == 1, 1, 0)
tmodulo1_2018$cred_aut_atr <- ifelse(tmodulo1_2018$cred_aut_atr == 1, 1, 0)
tmodulo1_2018$cred_viv_atr <- ifelse(tmodulo1_2018$cred_viv_atr == 1, 1, 0)
tmodulo1_2018$cred_gru_atr <- ifelse(tmodulo1_2018$cred_gru_atr == 1, 1, 0)


# Etiquetas para atrasos en créditos
attr(tmodulo1_2018$tarj_dep_atr, "label") <- "Atraso en tarjetas departamental"
attr(tmodulo1_2018$tarj_cred_atr, "label") <- "Atraso en tarjetas crédito bancaria"
attr(tmodulo1_2018$cred_nom_atr, "label") <- "Atraso en créditos nómina"
attr(tmodulo1_2018$cred_per_atr, "label") <- "Atraso en créditos personales"
attr(tmodulo1_2018$cred_aut_atr, "label") <- "Atraso en créditos automotrices"
attr(tmodulo1_2018$cred_viv_atr, "label") <- "Atraso en créditos vivienda"
attr(tmodulo1_2018$cred_gru_atr, "label") <- "Atraso en créditos grupales"

#_______________________________________________________________________________

# 51) Tiempo de atraso
#_______________________________________________________________________________
#Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10a_1"] <- "tarj_dep_t"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10a_2"] <- "tarj_cred_t"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10a_3"] <- "cred_nom_t"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10a_4"] <- "cred_per_t"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10a_5"] <- "cred_aut_t"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10a_6"] <- "cred_viv_t"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_10a_7"] <- "cred_gru_t"

# Etiquetas para las nuevas variables
attr(tmodulo1_2018$tarj_dep_t, "label") <- "Veces atraso en tarjetas departamental"
attr(tmodulo1_2018$tarj_cred_t, "label") <- "Veces atraso en tarjetas crédito bancaria"
attr(tmodulo1_2018$cred_nom_t, "label") <- "Veces atraso en créditos nómina"
attr(tmodulo1_2018$cred_per_t, "label") <- "Veces atraso en créditos personales"
attr(tmodulo1_2018$cred_aut_t, "label") <- "Veces atraso en créditos automotrices"
attr(tmodulo1_2018$cred_viv_t, "label") <- "Veces atraso en créditos vivienda"
attr(tmodulo1_2018$cred_gru_t, "label") <- "Veces atraso en créditos grupales"

#_______________________________________________________________________________

# 52) Tiene algun tipo de credito
#_______________________________________________________________________________
# Generar la variable credito
tmodulo1_2018$credito <- ifelse(tmodulo1_2018$cred_for == 1 | tmodulo1_2018$cred_inf == 1, 1, 0)

#Etiqueta
attr(tmodulo1_2018$credito, "label") <- "Tiene crédito formal o informal"

#_______________________________________________________________________________

# 53) Utilizar el CAT para comparar productos
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo1_2018$p6_11_3 <- ifelse(tmodulo1_2018$p6_11_3 == 1, 1, 0)

# Crear la variable compara_cat
tmodulo1_2018$compara_cat <- ifelse(tmodulo1_2018$p6_11_3 == 2, 0, tmodulo1_2018$p6_11_3)

# Etiquetas para la variable
attr(tmodulo1_2018$compara_cat, "label") <- "Utiliza CAT para contratar crédito"

#_______________________________________________________________________________

# 54) Sobreendeudamiento
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 55) Donde contrato credito
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 56) Frecuencia de uso de tarjeta departamental o credito
#_______________________________________________________________________________
#Ver variables de p5_26
var_p5_26 <- table(tmodulo1_2018$p5_26)
print(var_p5_26)

# Crear la variable 
tmodulo1_2018$tarj_cred_frec <- tmodulo1_2018$p5_26 

#Etiqueta
attr(tmodulo1_2018$tarj_cred_frec, "label") <- "Frecuencia uso mensual de tarjeta departamental o crédito"

#Ver variables de tarj_cred_frec
var_tarj_cred_frec <- table(tmodulo1_2018$tarj_cred_frec)
print(var_tarj_cred_frec)

#condiciones para crear la nueva columna
tmodulo1_2018$tarj_cred_frecg <- 0
tmodulo1_2018$tarj_cred_frecg[tmodulo1_2018$tarj_cred_frec == 0] <- 1
tmodulo1_2018$tarj_cred_frecg[tmodulo1_2018$tarj_cred_frec >= 1 & tmodulo1_2018$tarj_cred_frec < 10 | tmodulo1_2018$tarj_cred_frec == 88] <- 2
tmodulo1_2018$tarj_cred_frecg[tmodulo1_2018$tarj_cred_frec >= 10 & tmodulo1_2018$tarj_cred_frec != 88 & !is.na(tmodulo1_2018$tarj_cred_frec)] <- 3

#Etiqueta
attr(tmodulo1_2018$tarj_cred_frecg, "label") <- "Frecuencia uso mensual de tarjeta crédito"

#_______________________________________________________________________________

# 57) Razones de no uso de tarjetas de credito o departamentales
#_______________________________________________________________________________
# Razones de no uso de tarjetas de crédito o departamentales
tmodulo1_2018$tarj_cred_raznu <- tmodulo1_2018$p6_14

#Ver variables
var_tarj_cred_raznu <- table(tmodulo1_2018$tarj_cred_raznu)
print(var_tarj_cred_raznu)

#Etiqueta
attr(tmodulo1_2018$tarj_cred_raznu, "label") <- "Razon principal para no usar tarjeta credito"

# Etiquetas para la nueva variable
levels(tmodulo1_2018$tarj_cred_raznu) <- c(
  "Solo para emergencias",
  "Intereses o comisiones altas",
  "Prefiere pagos al contado",
  "No la aceptan en establecimientos",
  "No le gusta endeudarse",
  "Otro"
)

#_______________________________________________________________________________

# 58) Comparacion de productos de credito
#_______________________________________________________________________________
# Comparación de productos de crédito
tmodulo1_2018$compara_cred <- tmodulo1_2018$p6_16

#Etiqueta
attr(tmodulo1_2018$compara_cred, "label") <- "Compara productos de crédito antes de contratar"

#_______________________________________________________________________________

# 59) Medios e comparacion de creditos
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo1_2018$p6_17_1 <- ifelse(tmodulo1_2018$p6_17_1 == 1, 1, 0)
tmodulo1_2018$p6_17_2 <- ifelse(tmodulo1_2018$p6_17_2 == 1, 1, 0)
tmodulo1_2018$p6_17_3 <- ifelse(tmodulo1_2018$p6_17_3 == 1, 1, 0)
tmodulo1_2018$p6_17_4 <- ifelse(tmodulo1_2018$p6_17_4 == 1, 1, 0)
tmodulo1_2018$p6_17_5 <- ifelse(tmodulo1_2018$p6_17_5 == 1, 1, 0)

# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_17_1"] <- "compara_cred_ins"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_17_2"] <- "compara_cred_ami"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_17_3"] <- "compara_cred_con"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_17_4"] <- "compara_cred_anu"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_17_5"] <- "compara_cred_esp"

# Etiquetas 
attr(tmodulo1_2018$compara_cred_ins, "label") <- "Compara con info dada por banco"
attr(tmodulo1_2018$compara_cred_ami, "label") <- "Compara con recomendación de amistades"
attr(tmodulo1_2018$compara_cred_con, "label") <- "Compara con páginas de Condusef o Banxico"
attr(tmodulo1_2018$compara_cred_anu, "label") <- "Compara con anuncios comerciales"
attr(tmodulo1_2018$compara_cred_esp, "label") <- "Compara con recomendación de especialistas"

# Variable compara_cred_cfo
tmodulo1_2018$compara_cred_cfo <- ifelse(tmodulo1_2018$compara_cred_con == 1 | tmodulo1_2018$compara_cred_esp == 1, 1, NA)
tmodulo1_2018$compara_cred_cfo <- ifelse(tmodulo1_2018$compara_cred == 1 & is.na(tmodulo1_2018$compara_cred_cfo), 0, tmodulo1_2018$compara_cred_cfo)

# Etiqueta 
attr(tmodulo1_2018$compara_cred_cfo, "label") <- "Compara con info de Condusef o especialistas"
###################################################
#_______________________________________________________________________________

# 60) Ha traspado su saldo a otra institucion
#_______________________________________________________________________________
#Renombrar variables de la columna
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 61) Poblacion ex-usuaria de creditos
#_______________________________________________________________________________
#Ex usuarios de crédito formal
tmodulo1_2018$cred_ex <- ifelse(tmodulo1_2018$p6_5 == 1, 1, 0)

# Etiquetas para la nueva variable
attr(tmodulo1_2018$cred_ex, "label") <- "Población ex-usuaria de créditos"
#_______________________________________________________________________________

# 62) Razon para nunca tener credito formal
#_______________________________________________________________________________
# Crear la variable cred_nt_raz y recodificar
#Razón principal de la persona elegida de no tenencia de un crédito formal
tmodulo1_2018$cred_nt_raz <- tmodulo1_2018$p6_6

# Etiquetas para la nueva variable
levels(tmodulo1_2018$cred_nt_raz) <- c(
  "No cumple requisitos",
  "Sucursal lejana o no hay",
  "Cree que la rechazarán",
  "No confía en la institución financiera",
  "Intereses o comisiones altas",
  "No la necesita o no le interesa",
  "No le gusta endeudarse",
  "Otro"
)

attr(tmodulo1_2018$cred_nt_raz, "label") <- "Principal razón para no tener crédito"
#_______________________________________________________________________________

# 63) Razon para dejar de tener credito
#_______________________________________________________________________________
# Crear la variable cred_nt_raz y recodificar
#Razón principal de la persona elegida por la que dejó de tener tarjeta de crédito o un crédito formal 
tmodulo1_2018$cred_nt_raz <- tmodulo1_2018$p6_7

# Etiquetas para la nueva variable
levels(tmodulo1_2018$cred_nt_raz) <- c(
  "Cobraban intereses altos",
  "No se quiere volver a endeudar",
  "Ya no lo necesite",
  "Mala experiencia inst fin",
  "Ya no cumple con requisitos",
  "Prefiere otros tipos de prestamo",
  "otro"
)

attr(tmodulo1_2018$cred_nt_raz, "label") <- "Principal razon para dejar de tener credito"
#_______________________________________________________________________________

# 64) Rechazo de creditos
#_______________________________________________________________________________
#Alguna vez a la persona elegida, le han rechazado una solicitud de crédito formal (P6_17)
#Crear columnas
tmodulo1_2018$cred_rech <- ifelse(tmodulo1_2018$p6_18== 1, 1, 0)
tmodulo1_2018$cred_nunca <- ifelse(tmodulo1_2018$p6_18 == 3, 1, 0)

#Etiqueta
attr(tmodulo1_2018$cred_rech, "label") <- "Le han rechazado un crédito"
attr(tmodulo1_2018$cred_nunca, "label") <- "Nunca ha solicitado crédito"

#_______________________________________________________________________________

# 65)Razones de rechazo de credito
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo1_2018$p6_19_1 <- ifelse(tmodulo1_2018$p6_19_1 == 1, 1, 0)
tmodulo1_2018$p6_19_2 <- ifelse(tmodulo1_2018$p6_19_2 == 1, 1, 0)
tmodulo1_2018$p6_19_3 <- ifelse(tmodulo1_2018$p6_19_3 == 1, 1, 0)
tmodulo1_2018$p6_19_4 <- ifelse(tmodulo1_2018$p6_19_4 == 1, 1, 0)
tmodulo1_2018$p6_19_5 <- ifelse(tmodulo1_2018$p6_19_5 == 1, 1, 0)
tmodulo1_2018$p6_19_6 <- ifelse(tmodulo1_2018$p6_19_6 == 1, 1, 0)
tmodulo1_2018$p6_19_9 <- ifelse(tmodulo1_2018$p6_19_9 == 1, 1, 0)

# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_19_1"] <- "cred_rech_bur"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_19_2"] <- "cred_rech_ing"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_19_3"] <- "cred_rech_doc"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_19_4"] <- "cred_rech_his"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_19_5"] <- "cred_rech_gar"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_19_6"] <- "cred_rech_otr"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p6_19_9"] <- "cred_rech_ns"

# Etiquetas 
attr(tmodulo1_2018$cred_rech_bur, "label") <- "Rechazaron por problemas con buro"
attr(tmodulo1_2018$cred_rech_ing, "label") <- "Rechazaron por ingresos insuficientes o no comprobables"
attr(tmodulo1_2018$cred_rech_doc, "label") <- "Rechazaron por falta de documentos"
attr(tmodulo1_2018$cred_rech_his, "label") <- "Rechazaron por falta de historial crediticio"
attr(tmodulo1_2018$cred_rech_gar, "label") <- "Rechazaron por falta de garantia o fiador"
attr(tmodulo1_2018$cred_rech_otr, "label") <- "Otra razon de rechazo"
attr(tmodulo1_2018$cred_rech_ns, "label") <- "No sabe razon rechazo"

# Crear la variable cred_rech_burhis
tmodulo1_2018$cred_rech_burhis <- ifelse(tmodulo1_2018$cred_rech_bur == 1 | tmodulo1_2018$cred_rech_his == 1, 1, 0)
tmodulo1_2018$cred_rech_burhis[tmodulo1_2018$cred_rech == 1 & tmodulo1_2018$cred_rech_burhis == 0] <- NA

# Etiquetar la variable
attr(tmodulo1_2018$cred_rech_burhis, "label") <- "Rechazaron por problemas con buro o falta de historial"

#####################################################################################

# PAGOS

#####################################################################################
#_______________________________________________________________________________

# 66) Forma de pago mas frecuente
#_______________________________________________________________________________
#Forma de pago utilizada con más frecuencia por la persona elegida, cuando realiza compras de 500 pesos o menos (P7_1_1)
#Forma de pago utilizada con más frecuencia por la persona elegida, cuando realiza compras de 501 pesos o mas (P7_1_2)

tmodulo1_2018$p7_1_1 <- tmodulo2_2018$p7_1_1
tmodulo1_2018$p7_1_2 <- tmodulo2_2018$p7_1_2

# Etiquetas para la nueva variable
levels(tmodulo1_2018$p7_1_1) <- c(
  "Transferencia electrónica",
  "Cargo automático",
  "Tarjeta débito",
  "Tarjeta crédito",
  "Cheques",
  "Tarjeta prepagada",
  "Efectivo",
  "Otro"
)

levels(tmodulo1_2018$p7_1_2) <- c(
  "Transferencia electrónica",
  "Cargo automático",
  "Tarjeta débito",
  "Tarjeta crédito",
  "Cheques",
  "Tarjeta prepagada",
  "Efectivo",
  "Otro"
)

# Recodificación
tmodulo1_2018$p7_1_1[tmodulo1_2018$p7_1_1 == 9] <- NA
tmodulo1_2018$p7_1_2[tmodulo1_2018$p7_1_2 == 9] <- NA

# Creación de las nuevas variables
tmodulo1_2018$pago_menos500 <- tmodulo1_2018$p7_1_1
tmodulo1_2018$pago_mas500 <- tmodulo1_2018$p7_1_2

# Etiquetas 
attr(tmodulo1_2018$pago_menos500, "label") <- "Medio de pago más frecuente, <$500"
attr(tmodulo1_2018$pago_mas500, "label") <- "Medio de pago más frecuente, >$500"

#Ver variables de P6_9
var_pago_menos500 <- table(tmodulo1_2018$pago_menos500)
print(var_pago_menos500) 

var_pago_mas500 <- table(tmodulo1_2018$pago_mas500)
print(var_pago_mas500)
#_______________________________________________________________________________

# 67) Conoce CoDi
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 68) Utilizacion de CoDi
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 69) Preferencia de recibir fondos o efectivo
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 70) Recepcion de remesas
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año
#_______________________________________________________________________________

# 71) Medio de recepcion de remesas
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año

#_______________________________________________________________________________

# 72) Realizacion de compras
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año

#_______________________________________________________________________________

# 73) Metodo de pago por tipo de compra
#_______________________________________________________________________________
#No estan estas variables en la encuesta de este año

#####################################################################################

# SEGUROS

#####################################################################################
#_______________________________________________________________________________

# 74) Tenencia de seguros
#_______________________________________________________________________________
#Renombrar variables de la columnas
tmodulo2_2018$p8_1 <- ifelse(tmodulo2_2018$p8_1 == 1, 1, 0)
tmodulo2_2018$p8_2 <- ifelse(tmodulo2_2018$p8_2 == 1, 1, 0)
#-------------------------------------------------------------------------------
tmodulo1_2018$seguro <- ifelse(tmodulo2_2018$p8_1 ==1 | tmodulo2_2018$p8_2 == 1, 1, 0)
tmodulo1_2018$seguro_priv <- ifelse(tmodulo2_2018$p8_1 %in% c(2, 9), 0, tmodulo2_2018$p8_1)
tmodulo1_2018$seguro_madre <- ifelse(tmodulo2_2018$p8_2 %in% c(2, 9), 0, tmodulo2_2018$p8_2)

attr(tmodulo1_2018$seguro, "label") <- "Tiene algun seguro"
attr(tmodulo1_2018$seguro_priv, "label") <- "Tiene seguro privado"
attr(tmodulo1_2018$seguro_madre, "label") <- "Tiene seguro de madres trabajadoras"

#_______________________________________________________________________________

# 75) Ex-usuarios de seguros
#_______________________________________________________________________________
tmodulo2_2018$p8_3 <- ifelse(tmodulo2_2018$p8_3 %in% c(2), 0, tmodulo2_2018$p8_3)
tmodulo1_2018$seguro_ex <- ifelse(tmodulo2_2018$p8_3 %in% c(2), 0, tmodulo2_2018$p8_3)

#Etiqueta
attr(tmodulo1_2018$seguro_ex, "label") <- "Ex-usuario de seguro"

#_______________________________________________________________________________

# 76) Razon para no tener seguro
#_______________________________________________________________________________
tmodulo1_2018$seguro_nt_raz <- tmodulo2_2018$p8_4

# Etiquetas para la nueva variable
levels(tmodulo1_2018$seguro_nt_raz) <- c(
  "Tiene ahorro para imprevistos",
  "No confía en las aseguradoras",
  "No se lo han ofrecido",
  "No tiene dinero, no tiene trabajo o sus ingresos son variables",
  "No sabe qué son, cómo funcionan o dónde solicitarlos",
  "Son muy caros",
  "No los necesita o no le interesan",
  "Otro"
)

attr(tmodulo1_2018$seguro_nt_raz, "label") <- "Principal razon para no tener seguro"

#Ver varibles de la columna
var_seguro_nt_raz <- table(tmodulo1_2018$seguro_nt_raz)
print(var_seguro_nt_raz)

#_______________________________________________________________________________

# 77) Razon para dejar de tener seguro
#_______________________________________________________________________________
tmodulo1_2018$seguro_ex_raz <- tmodulo2_2018$p8_5

# Etiquetas para la nueva variable
levels(tmodulo1_2018$seguro_ex_raz) <- c(
  "Dejo de trabajar o cambio de trabajo",
  "Son muy caros",
  "No los utilizaba",
  "Lo tenía por un crédito y ya lo pagó",
  "Tuvo mala experiencia con la aseguradora",
  "Dejó de recibir un programa de gobierno	",
  "Dejó de tener el bien asegurado (auto, casa, etcétera)",
  "Otro"
)

attr(tmodulo1_2018$seguro_ex_raz, "label") <- "Principal razon para dejar de seguro"
#_______________________________________________________________________________

# 78) Tipo de seguros
#_______________________________________________________________________________
tmodulo1_2018$p8_6_1 <- ifelse(tmodulo2_2018$p8_6_1 == 1, 1, 0)
tmodulo1_2018$p8_6_2 <- ifelse(tmodulo2_2018$p8_6_2 == 1, 1, 0)
tmodulo1_2018$p8_6_3 <- ifelse(tmodulo2_2018$p8_6_3 == 1, 1, 0)
tmodulo1_2018$p8_6_4 <- ifelse(tmodulo2_2018$p8_6_4 == 1, 1, 0)
tmodulo1_2018$p8_6_5 <- ifelse(tmodulo2_2018$p8_6_5 == 1, 1, 0)
tmodulo1_2018$p8_6_6 <- ifelse(tmodulo2_2018$p8_6_6 == 1, 1, 0)
tmodulo1_2018$p8_6_7 <- ifelse(tmodulo2_2018$p8_6_7 == 1, 1, 0)
tmodulo1_2018$p8_6_8 <- ifelse(tmodulo2_2018$p8_6_8 == 1, 1, 0)

# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_6_1"] <- "seguro_vid"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_6_2"] <- "seguro_med"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_6_3"] <- "seguro_aut"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_6_4"] <- "seguro_acc"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_6_5"] <- "seguro_cas"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_6_6"] <- "seguro_edu"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_6_7"] <- "seguro_ret"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_6_8"] <- "seguro_otr"

# Etiquetas
attr(tmodulo1_2018$seguro_vid, "label") <- "Tiene seguro de vida"
attr(tmodulo1_2018$seguro_med, "label") <- "Tiene seguro de gastos medicos mayores"
attr(tmodulo1_2018$seguro_aut, "label") <- "Tiene seguro de auto"
attr(tmodulo1_2018$seguro_acc, "label") <- "Tiene seguro personal contra accidentes"
attr(tmodulo1_2018$seguro_cas, "label") <- "Tiene seguro de casa"
attr(tmodulo1_2018$seguro_edu, "label") <- "Tiene seguro de educacion"
attr(tmodulo1_2018$seguro_ret, "label") <- "Tiene plan privado de retiro"
attr(tmodulo1_2018$seguro_otr, "label") <- "Tiene otro tipo de seguro"

# Crear la variable seguro_gen en base a otras variables
tmodulo1_2018$seguro_gen <- ifelse(
  tmodulo1_2018$seguro_acc == 1 | tmodulo1_2018$seguro_cas == 1 | 
    tmodulo1_2018$seguro_edu == 1 | tmodulo1_2018$seguro_ret == 1 | 
    tmodulo1_2018$seguro_otr == 1, 1, 0
)

#Etiqueta
attr(tmodulo1_2018$seguro_gen, "label") <- "Tiene otro tipo de seguro (casa, educación, retiro, accidentes)"
#_______________________________________________________________________________

# 79) Tipo de contratacion de seguro
#_______________________________________________________________________________
tmodulo1_2018$p8_7_1 <- ifelse(tmodulo2_2018$p8_7_1 == 1, 1, 0)
tmodulo1_2018$p8_7_2 <- ifelse(tmodulo2_2018$p8_7_2 == 1, 1, 0)
tmodulo1_2018$p8_7_3 <- ifelse(tmodulo2_2018$p8_7_3 == 1, 1, 0)
tmodulo1_2018$p8_7_4 <- ifelse(tmodulo2_2018$p8_7_4 == 1, 1, 0)
tmodulo1_2018$p8_7_5 <- ifelse(tmodulo2_2018$p8_7_5 == 1, 1, 0)

# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_7_1"] <- "seguro_vid_dir"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_7_2"] <- "seguro_med_dir"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_7_3"] <- "seguro_aut_dir"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_7_4"] <- "seguro_acc_dir"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p8_7_5"] <- "seguro_cas_dir"

# Etiquetas
attr(tmodulo1_2018$seguro_vid_dir, "label") <- "Contrato directamente seguro de vida"
attr(tmodulo1_2018$seguro_med_dir, "label") <- "Contrato directamente seguro de gastos medicos mayores"
attr(tmodulo1_2018$seguro_aut_dir, "label") <- "Contrato directamente seguro de auto"
attr(tmodulo1_2018$seguro_acc_dir, "label") <- "Contrato directamente seguro personal contra accidentes"
attr(tmodulo1_2018$seguro_cas_dir, "label") <- "Contrato directamente seguro de casa"

# Crear la variable seguro_dir en base a otras variables
tmodulo1_2018$seguro_dir <- ifelse(tmodulo1_2018$seguro_vid_dir == 1 | tmodulo1_2018$seguro_med_dir == 1 | 
                                     tmodulo1_2018$seguro_aut_dir == 1 | tmodulo1_2018$seguro_acc_dir == 1 |  
                                     tmodulo1_2018$seguro_cas_dir == 1, 1, 0)

#Etiqueta
attr(tmodulo1_2018$seguro_dir, "label") <- "Contrato directamente su seguro"

#_______________________________________________________________________________

# 80) Satisfaccion con el seguro
#_______________________________________________________________________________
# Asignar valores en la columnaS según las condiciones dadas

tmodulo1_2018$seguro_vid_sat <- ifelse(tmodulo2_2018$p8_10_1 == 1, 1, 0)
tmodulo1_2018$seguro_med_sat <- ifelse(tmodulo2_2018$p8_10_2 == 1, 1, 0)
tmodulo1_2018$seguro_aut_sat <- ifelse(tmodulo2_2018$p8_10_3 == 1, 1, 0)
tmodulo1_2018$seguro_acc_sat <- ifelse(tmodulo2_2018$p8_10_4 == 1, 1, 0)
tmodulo1_2018$seguro_cas_sat <- ifelse(tmodulo2_2018$p8_10_5 == 1, 1, 0)

# Etiquetas 
attr(tmodulo1_2018$seguro_vid_sat, "label") <- "Satisfecho con seguro de vida"
attr(tmodulo1_2018$seguro_med_sat, "label") <- "Satisfecho con seguro de gastos medicos mayores"
attr(tmodulo1_2018$seguro_aut_sat, "label") <- "Satisfecho con seguro de auto"
attr(tmodulo1_2018$seguro_acc_sat, "label") <- "Satisfecho con seguro personal contra accidentes"
attr(tmodulo1_2018$seguro_cas_sat, "label") <- "Satisfecho con seguro de casa"
#_______________________________________________________________________________

# 81) Uso de seguro
#_______________________________________________________________________________
# Asignar valores en la columnaS según las condiciones dadas
#_______________________________________________________________________________

# 82) Comparacion de seguros
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo1_2018$compara_seguro <- ifelse(tmodulo2_2018$p8_13 == 1, 1, 0)
attr(tmodulo1_2018$compara_seguro, "label") <- "Compara seguros antes de contratar"
#_______________________________________________________________________________

# 83) Medios e comparacion de seguros
#_______________________________________________________________________________
#Renombrar variables de las columnas
tmodulo1_2018$compara_seguro_ins <- ifelse(tmodulo2_2018$p8_14_1 == 1, 1, 0)
tmodulo1_2018$compara_seguro_ami <- ifelse(tmodulo2_2018$p8_14_2 == 1, 1, 0)
tmodulo1_2018$compara_seguro_con <- ifelse(tmodulo2_2018$p8_14_3 == 1, 1, 0)
tmodulo1_2018$compara_seguro_anu <- ifelse(tmodulo2_2018$p8_14_4 == 1, 1, 0)
tmodulo1_2018$compara_seguro_esp <- ifelse(tmodulo2_2018$p8_14_5 == 1, 1, 0)

# Etiquetas 
attr(tmodulo1_2018$compara_seguro_ins, "label") <- "Compara con info dada por institucion"
attr(tmodulo1_2018$compara_seguro_ami, "label") <- "Compara con recomendacion amistades"
attr(tmodulo1_2018$compara_seguro_con, "label") <- "Compara con paginas Condusef o CNSF"
attr(tmodulo1_2018$compara_seguro_anu, "label") <- "Compara con anuncios comerciales"
attr(tmodulo1_2018$compara_seguro_esp, "label") <- "Compara con recomendacion de especialistas"

# Crear la variable compara_seguro_cfo
tmodulo1_2018$compara_seguro_cfo <- 0

# Asignar valores a compara_seguro_cfo según las condiciones dadas
tmodulo1_2018$compara_seguro_cfo <- ifelse(
  tmodulo1_2018$compara_seguro_con == 1 | tmodulo1_2018$compara_seguro_esp == 1, 1,
  ifelse(tmodulo1_2018$compara_seguro == 1 & is.na(tmodulo1_2018$compara_seguro_cfo), 0, tmodulo1_2018$compara_seguro_cfo)
)

# Etiqueta
attr(tmodulo1_2018$compara_seguro_cfo, "label") <- "Compara con info de Condusef o especialistas"

var_compara_seguro_cfo <- table(tmodulo1_2018$compara_seguro_cfo)
print(var_compara_seguro_cfo)

#####################################################################################

# AHORRO PARA EL RETIRO

#####################################################################################
#_______________________________________________________________________________

# 84) Tenencia de cuentas de ahorro para el retiro
#_______________________________________________________________________________
tmodulo1_2018$afore <- ifelse(tmodulo2_2018$p9_1 == 1, 1, 0)

# Etiquetar la variable 
attr(tmodulo1_2018$afore, "label") <- "Tiene afore o cuenta de retiro"
#_______________________________________________________________________________

# 85) Ex-usuarios de afores
#_______________________________________________________________________________
# Asignar valores en la columnaS según las condiciones dadas

#_______________________________________________________________________________

# 86) Razon para no tener afore
#_______________________________________________________________________________
# Crear la variable afore_nt_raz
tmodulo1_2018$afore_nt_raz <- tmodulo2_2018$p9_2

# Etiquetar la variable 
attr(tmodulo1_2018$afore_nt_raz, "label") <- "Principal razón para no tener afore"

# Etiquetas para la nueva variable
levels(tmodulo1_2018$afore_nt_raz) <- c(
  "No trabaja/ Nunca ha trabajado",
  "No sabe que es una cuenta para el retiro",
  "No tiene dinero o es insuficiente para ahorrar",
  "No sabe cómo tramitarla",
  "No le interesa o piensa que no le conviene",
  "La Afores le dan desconfianza",
  "Es jubilada, pensionada o tiene sus necesidades resueltas",
  "Otra"
)

var_afore_nt_raz <- table(tmodulo1_2018$afore_nt_raz)
print(var_afore_nt_raz)
#_______________________________________________________________________________

# 87) Como contrato afore
#_______________________________________________________________________________
# Asignar valores en la columnaS según las condiciones dadas
#_______________________________________________________________________________

# 88) Aportaciones voluntarias
#_______________________________________________________________________________
tmodulo1_2018$afore_apor <- ifelse(tmodulo2_2018$p9_4 == 1, 1, 0)

# Etiqueta
attr(tmodulo1_2018$afore_apor, "label") <- "Realiza aportaciones voluntarias a afore"

#_______________________________________________________________________________

# 89) Razon de no hacer aportaciones voluntarias
#_______________________________________________________________________________
# Crear la variable 
tmodulo1_2018$afore_apor_nt_raz <- ifelse(tmodulo2_2018$p9_5 == 1, 1, 0)

# Etiquetar la variable 
attr(tmodulo1_2018$afore_apor_nt_raz, "label") <- "principal razon para no hacer aportaciones a afore"

# Etiquetas para la nueva variable
levels(tmodulo1_2018$afore_apor_nt_raz) <- c(
  "No le queda dinero para ahorrar",
  "No sabe que es",
  "Ahorra de otra forma",
  "Desconoce las ventajes",
  "No confia en Afores",
  "Otro"
)

var_afore_apor_nt_raz <- table(tmodulo1_2018$afore_apor_nt_raz)
print(var_afore_apor_nt_raz)
#_______________________________________________________________________________

# 90) Satisfaccion con afore
#_______________________________________________________________________________
tmodulo1_2018$afore_sat <- ifelse(tmodulo2_2018$p9_7 == 1, 1, 0)

# Etiquetar la variable 
attr(tmodulo1_2018$afore_sat, "label") <- "Satisfecho con atención de Afore"
#_______________________________________________________________________________

# 91) Razon para no estar satisfecho
#_______________________________________________________________________________
tmodulo1_2018$afore_raz_insat <- ifelse(tmodulo2_2018$p9_8 == 1, 1, 0)

# Etiquetar la variable 
attr(tmodulo1_2018$afore_raz_insat, "label") <- " Razon para no estar satisfecho Afore"

# Etiquetas para la nueva variable
levels(tmodulo1_2018$afore_raz_insat) <- c(
  "No lo(a) atendieron",
  "Esperó mucho tiempo para ser atendido(a)",
  "No le dieron información que necesitaba",
  "No ha podido consultar su trámite",
  "Lo(a) trataron mal",
  "Bajos rendimientos o altas comisiones",
  "Lo(a) cambiaron de Afore sin autorización",
  "No recibe los estados de cuenta",
  "Otro"
)

var_afore_raz_insat <- table(tmodulo1_2018$afore_raz_insat)
print(var_afore_raz_insat)
#_______________________________________________________________________________

# 92) Actitudes financieras en vejez
#_______________________________________________________________________________
tmodulo1_2018$act_vej_gob <- ifelse(tmodulo2_2018$p9_9_1 == 1, 1, 0)
tmodulo1_2018$act_vej_afore <- ifelse(tmodulo2_2018$p9_9_2 == 1, 1, 0)
tmodulo1_2018$act_vej_bie <- ifelse(tmodulo2_2018$p9_9_3 == 1, 1, 0)
tmodulo1_2018$act_vej_trab <- ifelse(tmodulo2_2018$p9_9_4 == 1, 1, 0)
tmodulo1_2018$act_vej_otr <- ifelse(tmodulo2_2018$p9_9_5 == 1, 1, 0)

# Etiqueta para laS columnas
attr(tmodulo1_2018$act_vej_gob, "label") <- "Piensa cubrir gastos vejez con apoyos gob"
attr(tmodulo1_2018$act_vej_afore, "label") <- "Piensa cubrir gastos vejez con ahorro retiro"
attr(tmodulo1_2018$act_vej_bie, "label") <- "Piensa cubrir gastos vejez con venta de bienes"
attr(tmodulo1_2018$act_vej_trab, "label") <- "Piensa cubrir gastos vejez trabajando"
attr(tmodulo1_2018$act_vej_otr, "label") <- "Piensa cubrir gastos vejez de otra forma"

#####################################################################################

# USO DE CANALES FINANCIEROS

#####################################################################################
#_______________________________________________________________________________

# 93) Uso de sucursales
#_______________________________________________________________________________
tmodulo1_2018$sucursal <- ifelse(tmodulo2_2018$p10_1 == 1, 1, 0)

# Etiquetar la columna 
attr(tmodulo1_2018$sucursal, "label") <- "Ha usado sucursal en el último año"

#_______________________________________________________________________________

# 94) Razon para no utilizar sucursal
#_______________________________________________________________________________
# Crear la variable 
tmodulo1_2018$sucursal_nt_raz <- tmodulo2_2018$p10_2

# Etiqueta 
attr(tmodulo1_2018$sucursal_nt_raz, "label") <- "Principal razon para no usar sucursal"

# Etiquetas para la nueva variable
levels(tmodulo1_2018$sucursal_nt_raz) <- c(
  "No tiene cuenta o tarjeta",
  "Ingresos insuficientes",
  "Prefiere otros medios",
  "Esta muy lejos o no hay",
  "Son inseguras o le dan desconfianza",
  "Otra persona hace sus tramites",
  "Mal servicio",
  "Otra"
)

var_afore_apor_nt_raz <- table(tmodulo1_2018$sucursal_nt_raz)
print(var_afore_apor_nt_raz)
#_______________________________________________________________________________

# 95) Tiempos de traslado a sucursal
#_______________________________________________________________________________
# Recodificar los valores 99 a NA (missing)
tmodulo2_2018$sucursal_tras <- ifelse(tmodulo2_2018$p10_4_1 %in% c(99), NA, tmodulo2_2018$p10_4_1 * 60) + ifelse(tmodulo2_2018$p10_4_2 %in% c(99), NA, tmodulo2_2018$p10_4_2 * 60)

# Reemplazar 'sucursal_tras' con 'p10_4_1 * 60' si 'p10_4_2' es NA
tmodulo2_2018$sucursal_tras <- ifelse(is.na(tmodulo2_2018$sucursal_tras) & !is.na(tmodulo2_2018$p10_4_2), tmodulo2_2018$p10_4_2 * 60, tmodulo2_2018$sucursal_tras)

# Reemplazar 'sucursal_tras' con 'p10_4_2' si 'p10_4_1' es NA
tmodulo2_2018$sucursal_tras <- ifelse(is.na(tmodulo2_2018$sucursal_tras) & !is.na(tmodulo2_2018$p10_4_1), tmodulo2_2018$p10_4_1 * 60, tmodulo2_2018$sucursal_tras)

# Crear sucursal_tras en tmodulo1_2018
tmodulo1_2018$sucursal_tras <- tmodulo2_2018$sucursal_tras 

# Etiqueta 
attr(tmodulo1_2018$sucursal_tras, "label") <- "Tiempo de traslado sucursal, minutos"
tmodulo1_2018$sucursal_tras
#_______________________________________________________________________________

# 96) Uso de cajero
#_______________________________________________________________________________
tmodulo1_2018$cajero <- ifelse(tmodulo2_2018$p10_8 == 1, 1, 0)

# Etiqueta para la columna cajero en tmodulo1_2018
attr(tmodulo1_2018$cajero, "label") <- "Ha usado cajero en último año"

#_______________________________________________________________________________

# 97) Razon para no utilizar cajero
#_______________________________________________________________________________
# Crear la variable 
tmodulo1_2018$cajero_nt_raz <- tmodulo2_2018$p10_9

# Etiquetas para la nueva variable
levels(tmodulo1_2018$cajero_nt_raz) <- c(
  "No tiene cuenta o tarjeta",
  "Ingresos insuficientes",
  "No conoce o no sabe usarlos",
  "Prefiere otros medios",
  "Otra persona hace sus tramites",
  "Son inseguras o le dan desconfianza",
  "Estan muy lejos o no hay",
  "Otro"
)

var_cajero_nt_raz <- table(tmodulo1_2018$cajero_nt_raz)
print(var_cajero_nt_raz)

# Etiqueta para la variable cajero_nt_raz en tmodulo1_2018
attr(tmodulo1_2018$cajero_nt_raz, "label") <- "Principal razón para no usar cajero"

#_______________________________________________________________________________

# 98) Tiempo de traslado al cajero automático que usa regularmente la persona elegida
#_______________________________________________________________________________
# Cambiar los valores 99 y 100 por NA 
tmodulo2_2018$p10_11_1[tmodulo2_2018$p10_11_1 %in% c(99)] <- NA
tmodulo2_2018$p10_11_2[tmodulo2_2018$p10_11_2 %in% c(99)] <- NA

# Crear la variable cajero_tras
tmodulo2_2018$cajero_tras <- tmodulo2_2018$p10_11_1 * 60 + tmodulo2_2018$p10_11_2

# Reemplazar los valores con p10_11_1*60 si p10_11_2 es NA
tmodulo2_2018$cajero_tras[is.na(tmodulo2_2018$p10_11_2)] <- tmodulo2_2018$p10_11_1[is.na(tmodulo2_2018$p10_11_2)] * 60

# Reemplazar los valores con p10_11_2 si p10_11_1 es NA
tmodulo2_2018$cajero_tras[is.na(tmodulo2_2018$p10_11_1)] <- tmodulo2_2018$p10_11_2[is.na(tmodulo2_2018$p10_11_1)]

# Crear la variable 
tmodulo1_2018$cajero_tras <- tmodulo2_2018$cajero_tras

# Etiquetar la variable 
attr(tmodulo2_2018$cajero_tras, "label") <- "Tiempo de traslado cajero, minutos"

#Ver variables de la columna:
var_cajero_tras <- table(tmodulo1_2018$cajero_tras)
print(var_cajero_tras)
#_______________________________________________________________________________

# 99) Uso de corresponsal
#_______________________________________________________________________________
tmodulo1_2018$corres <- ifelse(tmodulo2_2018$p10_13 == 1, 1, 0)

# Etiquetar la columna 
attr(tmodulo1_2018$corres, "label") <- "Ha usado corresponsal en último año"

#_______________________________________________________________________________

# 100) Razón principal de la persona elegida por la que no ha utilizado estas tiendas o comercios para realizar operaciones financieras
#_______________________________________________________________________________
# Crear la variable 
tmodulo1_2018$corres_nt_raz <- tmodulo2_2018$p10_14

# Etiquetas para la nueva variable
levels(tmodulo1_2018$corres_nt_raz) <- c(
  "Prefiere usar cajero o sucursal",
  "Otra persona hace sus tramites",
  "Comisiones altas",
  "No sabia que podia hacerlo",
  "Estan muy lejos o no hay",
  "Son inseguros o le dan desconfianza",
  "Lo obligan a realizar compras en tienda",
  "Otra"
)

# Etiqueta para la variable corres_nt_raz en tmodulo1_2018
attr(tmodulo1_2018$corres_nt_raz, "label") <- "Principal razón para no usar corresponsal"

var_corres_nt_raz <- table(tmodulo1_2018$corres_nt_raz)
print(var_corres_nt_raz)
#_______________________________________________________________________________

# 101) Tiempos de traslado a cajero
#_______________________________________________________________________________
# Cambiar los valores 99 y 100 por NA
tmodulo2_2018$p10_15_1[tmodulo2_2018$p10_15_1 %in% c(99)] <- NA
tmodulo2_2018$p10_15_2[tmodulo2_2018$p10_15_2 %in% c(99)] <- NA

###########
var_p10_15_1 <- table(tmodulo2_2018$p10_15_1)
print(var_p10_15_1)

var_p10_15_2 <- table(tmodulo2_2018$p10_15_2)
print(var_p10_15_2)

##########

# Crear la variable corres_tras
tmodulo2_2018$corres_tras <- tmodulo2_2018$p10_15_1 * 60 + tmodulo2_2018$p10_15_2

# Reemplazar los valores con p10_15_1*60 si p10_15_2 es NA
tmodulo2_2018$corres_tras[is.na(tmodulo2_2018$p10_15_2)] <- tmodulo2_2018$p10_15_1[is.na(tmodulo2_2018$p10_15_2)] * 60

# Reemplazar los valores con p10_15_2 si p10_15_1 es NA
tmodulo2_2018$corres_tras[is.na(tmodulo2_2018$p10_15_1)] <- tmodulo2_2018$p10_15_2[is.na(tmodulo2_2018$p10_15_1)]

# Crear la variable en tmodulo1_2018
tmodulo1_2018$corres_tras <- tmodulo2_2018$corres_tras

# Etiquetar la variable corres_tras
attr(tmodulo1_2018$corres_tras, "label") <- "Tiempo de traslado cajero, minutos"


#####################################################################################

# CONFIANZA Y PROTECCIÓN DE USUARIOS

#####################################################################################

#_______________________________________________________________________________

# 103) Tipos de problemas con productos financieros
#_______________________________________________________________________________
# Crear la variable prob_fin y asignarle el valor 0
tmodulo1_2018$prob_fin <- 0

# Crear la variable prob_fin con ifelse
tmodulo1_2018$prob_fin <- ifelse(tmodulo2_2018$p11_2_1 == 1 | tmodulo2_2018$p11_2_2 == 1 | tmodulo2_2018$p11_2_3 == 1 | tmodulo2_2018$p11_2_4 == 1, 1, 0)

# Etiquetar la variable prob_fin
attr(tmodulo1_2018$prob_fin, "label") <- "Tuvo algún problema con algún producto financiero"

#-------------------------------------------------------------------------------
tmodulo1_2018$prob_clo <- ifelse(tmodulo2_2018$p11_1_1 == 1, 1, 0)
tmodulo1_2018$prob_ide <- ifelse(tmodulo2_2018$p11_1_2 == 1, 1, 0)
tmodulo1_2018$prob_fra <- ifelse(tmodulo2_2018$p11_1_3 == 1, 1, 0)

# Etiqueta para laS columnas
attr(tmodulo1_2018$prob_clo, "label") <- "Ha sufrido de clonación de tarjetas"
attr(tmodulo1_2018$prob_ide, "label") <- "Ha sufrido de robo de identidad"
attr(tmodulo1_2018$prob_fra, "label") <- "Ha invertido en productos fraudulentos"

#_______________________________________________________________________________

# 104) Lugar de reclamo en caso de problema
#_______________________________________________________________________________
tmodulo1_2018$p11_2_1 <- ifelse(tmodulo2_2018$p11_2_1 == 1, 1, 0)
tmodulo1_2018$p11_2_2 <- ifelse(tmodulo2_2018$p11_2_2 == 1, 1, 0)
tmodulo1_2018$p11_2_3 <- ifelse(tmodulo2_2018$p11_2_3 == 1, 1, 0)
tmodulo1_2018$p11_2_4 <- ifelse(tmodulo2_2018$p11_2_4 == 1, 1, 0)
tmodulo1_2018$p11_2_9 <- ifelse(tmodulo2_2018$p11_2_9 == 1, 1, 0)

#_______________________________________________________________________________

# 105) No sabría donde reclamar
#_______________________________________________________________________________
# Crear la variable prob_q_ns
tmodulo1_2018$prob_q_ns <- ifelse(tmodulo2_2018$p11_2_1 == 0 & tmodulo2_2018$p11_2_2 == 0 & tmodulo2_2018$p11_2_3 == 0 & tmodulo2_2018$p11_2_4 == 0, 1, 0)

# Reemplazar con N/A si p11_3_4 es 1
tmodulo1_2018$prob_q_ns[tmodulo2_2018$p11_2_4 == 1] <- NA

# Etiquetar la variable
attr(tmodulo1_2018$prob_q_ns, "label") <- "No sabe dónde levantar queja"
#_______________________________________________________________________________

# 106) Sabria donde reclamar
#_______________________________________________________________________________
# Crear la variable prob_q_sd
tmodulo1_2018$prob_q_sd <- ifelse(tmodulo2_2018$p11_2_1 == 1 | tmodulo2_2018$p11_2_2 == 1 | tmodulo2_2018$p11_2_3 == 1, 1, 0)

# Etiquetar la variable
attr(tmodulo1_2018$prob_q_sd, "label") <- "Sabe dónde levantar queja"


#_______________________________________________________________________________

# 107) Generamos canales de queja que conoce
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p11_2_1"] <- "prob_q_ban"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p11_2_2"] <- "prob_q_condusef"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p11_2_3"] <- "prob_q_profeco"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p11_2_4"] <- "prob_q_otr"

# Etiquetas para las columnas
attr(tmodulo1_2018$prob_q_ban, "label") <- "En caso de problema, se quejaría en banco"
attr(tmodulo1_2018$prob_q_condusef, "label") <- "En caso de problema, se quejaría en Condusef"
attr(tmodulo1_2018$prob_q_profeco, "label") <- "En caso de problema, se quejaría en Profeco"
attr(tmodulo1_2018$prob_q_otr, "label") <- "En caso de problema, se quejaría en otro"

#_______________________________________________________________________________

# 108) Condicion de reclamo
#_______________________________________________________________________________
tmodulo1_2018$prob_fin_rec <- ifelse(tmodulo2_2018$p11_3 == 1, 1, 0)

# Etiqueta para la columna 
attr(tmodulo1_2018$prob_fin_rec, "label") <- "Ha presentado reclamo a una institución"

#####################################################################################

# CAPACIDADES FINANCIERAS

#####################################################################################
#_______________________________________________________________________________

# 113) Interes simple
#_______________________________________________________________________________
# Interes simple
tmodulo1_2018$cono_ints <- ifelse(tmodulo2_2018$p12_1 == 1, 1, 0)

# Etiquetar variable 
attr(tmodulo1_2018$cono_ints, "label") <- "Conoce concepto de interés simple"

#_______________________________________________________________________________

# 114) Interes simple con calculo
#_______________________________________________________________________________
# Interes simple con calculo
tmodulo1_2018$cono_ints_c <- ifelse(tmodulo2_2018$p12_2 == 2, 1, 0)

# Etiquetar variable cono_ints_c
attr(tmodulo1_2018$cono_ints_c, "label") <- "Cálculo correcto de interés simple"

#_______________________________________________________________________________

# 115) Interes compuesto
#_______________________________________________________________________________
# Interes compuesto
tmodulo1_2018$cono_intc <- ifelse(tmodulo2_2018$p12_3 == 1, 1, 0)

# Etiquetar variable cono_intc
attr(tmodulo1_2018$cono_intc, "label") <- "Cálculo correcto de interés compuesto"

#_______________________________________________________________________________

# 116) Calculo correcto de inflacion
#_______________________________________________________________________________
# Cálculo correcto de inflación
tmodulo1_2018$cono_infc <- ifelse(tmodulo2_2018$p12_4 == 3, 1, 0)

# Etiquetar variable 
attr(tmodulo1_2018$cono_infc, "label") <- "Cálculo correcto de impacto inflación"

#####################################################################################

# TOMA DE DESICIONES

#####################################################################################
#_______________________________________________________________________________

# 117) Toma de decision
#_______________________________________________________________________________
# Comparación de productos de crédito
tmodulo1_2018$deci <- tmodulo2_2018$p13_1

# Etiquetas 
levels(tmodulo1_2018$deci) <- c(
  "Solo usted",
  "Usted y otra persona",
  "Solo otra persona del hogar",
  "Otras personas del hogar"
)

# Etiquetar variable 
attr(tmodulo1_2018$deci, "label") <- "Persona que toma decisiones economicas en hogar"

tmodulo1_2018$deci_p <- ifelse(tmodulo1_2018$deci == 1 | tmodulo1_2018$deci == 2, 1, 0)
attr(tmodulo1_2018$deci_p, "label") <- "Tiene poder de decisión económico"

#_______________________________________________________________________________

# 118) Propiedad de activos
#_______________________________________________________________________________
# Cambiar variables de las columnas
tmodulo1_2018$prop_viv <- ifelse(tmodulo2_2018$p13_2_1 == 1, 1, 0)
tmodulo1_2018$prop_auto <- ifelse(tmodulo2_2018$p13_2_2 == 1, 1, 0)
tmodulo1_2018$prop_ter <- ifelse(tmodulo2_2018$p13_2_3 == 1, 1, 0)
tmodulo1_2018$prop_otr<- ifelse(tmodulo2_2018$p13_2_4 == 1, 1, 0)

# Etiquetar variables
attr(tmodulo1_2018$prop_viv, "label") <- "Tiene vivienda o departamento"
attr(tmodulo1_2018$prop_auto, "label") <- "Tiene automovil, moto"
attr(tmodulo1_2018$prop_ter, "label") <- "Tiene terreno o tierra de cultivo"
attr(tmodulo1_2018$prop_otr, "label") <- "Tiene otro tipo de propiedad"

#_______________________________________________________________________________

# 119) Forma de adquisicion de bienes
#_______________________________________________________________________________
tmodulo1_2018$prop_viv_adq <- tmodulo2_2018$p13_3_1
tmodulo1_2018$prop_auto_adq <- tmodulo2_2018$p13_3_2
tmodulo1_2018$prop_ter_adq <- tmodulo2_2018$p13_3_3

# Etiquetar variables
attr(tmodulo1_2018$prop_viv_adq, "label") <- "Principal forma de adquisición vivienda"
attr(tmodulo1_2018$prop_auto_adq, "label") <- "Principal forma de adquisición automóvil"
attr(tmodulo1_2018$prop_ter_adq, "label") <- "Principal forma de adquisición terreno"
#_______________________________________________________________________________

# 120) Libertad para diposicion de bienes
#_______________________________________________________________________________
# Renombrar variables
tmodulo1_2018$prop_viv_disp <- tmodulo2_2018$p13_4_1
tmodulo1_2018$prop_auto_disp <- tmodulo2_2018$p13_4_2
tmodulo1_2018$prop_ter_disp <- tmodulo2_2018$p13_4_3

# Etiquetar variables
attr(tmodulo1_2018$prop_viv_disp, "label") <- "Libertad de disposición de vivienda"
attr(tmodulo1_2018$prop_auto_disp, "label") <- "Libertad de disposición de automóvil"
attr(tmodulo1_2018$prop_ter_disp, "label") <- "Libertad de disposición de terreno"

#####################################################################################

# INDICADORES DE USO

#####################################################################################
#_______________________________________________________________________________

# 120) Poblacion con al menos un producto financiero
#_______________________________________________________________________________
# Crear variable
tmodulo1_2018$prod_fin <- ifelse(tmodulo1_2018$cuenta == 1 | tmodulo1_2018$cred_for == 1 | tmodulo1_2018$seguro == 1 | tmodulo1_2018$afore == 1, 1, 0)

# Etiquetar variable
attr(tmodulo1_2018$prod_fin, "label") <- "Cuenta con al menos un producto financiero"

#_______________________________________________________________________________

# 121) usuarios de cuenta
#_______________________________________________________________________________
# Generar variable
tmodulo1_2018$cuenta_usuario <- 0

# Actualizar valores
tmodulo1_2018$cuenta_usuario[tmodulo1_2018$cuenta == 1] <- 1
tmodulo1_2018$cuenta_usuario[tmodulo1_2018$cuenta_ex == 1] <- 2

# Etiquetar variable
levels(tmodulo1_2018$cuenta_usuario) <- c("Nunca ha tenido", "Tiene producto", "Dejo de tener")
attr(tmodulo1_2018$cuenta_usuario, "label") <- "Poblacion por tenencia de cuenta"

#_______________________________________________________________________________

# 122) Poblacion por numero de tipos de productos
#_______________________________________________________________________________
# Generar variables
tmodulo1_2018$prod_uno <- 0
tmodulo1_2018$prod_dos <- 0
tmodulo1_2018$prod_tres <- 0
tmodulo1_2018$prod_cuatro <- 0
tmodulo1_2018$prod_cero <- 0


tmodulo1_2018$prod_uno <- ifelse((tmodulo1_2018$cuenta == 1 & tmodulo1_2018$cred_for == 0 & tmodulo1_2018$seguro == 0 & tmodulo1_2018$afore == 0) |
                        (tmodulo1_2018$cuenta == 0 & tmodulo1_2018$cred_for == 1 & tmodulo1_2018$seguro == 0 & tmodulo1_2018$afore == 0) |
                        (tmodulo1_2018$cuenta == 0 & tmodulo1_2018$cred_for == 0 & tmodulo1_2018$seguro == 1 & tmodulo1_2018$afore == 0) |
                        (tmodulo1_2018$cuenta == 0 & tmodulo1_2018$cred_for == 0 & tmodulo1_2018$seguro == 0 & tmodulo1_2018$afore == 1), 1, 0)

tmodulo1_2018$prod_dos <- ifelse((tmodulo1_2018$cuenta == 1 & tmodulo1_2018$cred_for == 1 & tmodulo1_2018$seguro == 0 & tmodulo1_2018$afore == 0) |
                        (tmodulo1_2018$cuenta == 1 & tmodulo1_2018$cred_for == 0 & tmodulo1_2018$seguro == 1 & tmodulo1_2018$afore == 0) |
                        (tmodulo1_2018$cuenta == 1 & tmodulo1_2018$cred_for == 0 & tmodulo1_2018$seguro == 0 & tmodulo1_2018$afore == 1) |
                        (tmodulo1_2018$cuenta == 0 & tmodulo1_2018$cred_for == 1 & tmodulo1_2018$seguro == 1 & tmodulo1_2018$afore == 1) |
                        (tmodulo1_2018$cuenta == 0 & tmodulo1_2018$cred_for == 1 & tmodulo1_2018$seguro == 0 & tmodulo1_2018$afore == 1) |
                        (tmodulo1_2018$cuenta == 0 & tmodulo1_2018$cred_for == 0 & tmodulo1_2018$seguro == 1 & tmodulo1_2018$afore == 1), 1, 0)

tmodulo1_2018$prod_tres <- ifelse((tmodulo1_2018$cuenta == 1 & tmodulo1_2018$cred_for == 1 & tmodulo1_2018$seguro == 1 & tmodulo1_2018$afore == 0) |
                         (tmodulo1_2018$cuenta == 1 & tmodulo1_2018$cred_for == 1 & tmodulo1_2018$seguro == 0 & tmodulo1_2018$afore == 1) |
                         (tmodulo1_2018$cuenta == 1 & tmodulo1_2018$cred_for == 0 & tmodulo1_2018$seguro == 1 & tmodulo1_2018$afore == 1) |
                         (tmodulo1_2018$cuenta == 0 & tmodulo1_2018$cred_for == 1 & tmodulo1_2018$seguro == 1 & tmodulo1_2018$afore == 1), 1, 0)

tmodulo1_2018$prod_cuatro <- ifelse(tmodulo1_2018$cuenta == 1 & tmodulo1_2018$cred_for == 1 & tmodulo1_2018$seguro == 1 & tmodulo1_2018$afore == 1, 1, 0)

tmodulo1_2018$prod_cero <- ifelse(tmodulo1_2018$cuenta == 0 & tmodulo1_2018$cred_for == 0 & tmodulo1_2018$seguro == 0 & tmodulo1_2018$afore == 0, 1, 0)

########################

# Etiquetar variables
attr(tmodulo1_2018$prod_uno, "label") <- "Cuenta con solo un tipo de producto financiero"
attr(tmodulo1_2018$prod_dos, "label") <- "Cuenta con dos tipos de productos financieros"
attr(tmodulo1_2018$prod_tres, "label") <- "Cuenta con tres tipos de productos financieros"
attr(tmodulo1_2018$prod_cuatro, "label") <- "Cuenta con los cuatro tipos de productos financieros"
attr(tmodulo1_2018$prod_cero, "label") <- "Cuenta con ningun producto financiero"


var_prod_uno <- table(tmodulo1_2018$prod_uno)
var_prod_dos <- table(tmodulo1_2018$prod_dos)
var_prod_tres <- table(tmodulo1_2018$prod_tres)
var_prod_cuatro <- table(tmodulo1_2018$prod_cuatro)
var_prod_cero <- table(tmodulo1_2018$prod_cero)

var_prod_uno
var_prod_dos 
var_prod_tres
var_prod_cero
#_______________________________________________________________________________

# 123) Variables historicas
#_______________________________________________________________________________
# Variables históricas
tmodulo1_2018$cuenta_hist <- ifelse(tmodulo1_2018$cuenta == 1 | tmodulo1_2018$cuenta_ex == 1, 1, 0)
tmodulo1_2018$seguro_hist <- ifelse(tmodulo1_2018$seguro == 1 | tmodulo1_2018$seguro_ex == 1, 1, 0)
tmodulo1_2018$cred_hist <- ifelse(tmodulo1_2018$cred_for == 1 | tmodulo1_2018$cred_ex == 1, 1, 0)
tmodulo1_2018$afore_hist <- ifelse(tmodulo1_2018$afore == 1, 1, 0)
tmodulo1_2018$prod_fin_hist <- ifelse(tmodulo1_2018$cuenta_hist == 1 | tmodulo1_2018$cred_hist == 1 | tmodulo1_2018$afore == 1 | tmodulo1_2018$seguro_hist == 1, 1, 0)

# Etiquetar variables
attr(tmodulo1_2018$cuenta_hist, "label") <- "Tiene o ha tenido cuenta"
attr(tmodulo1_2018$seguro_hist, "label") <- "Tiene o ha tenido seguro"
attr(tmodulo1_2018$cred_hist, "label") <- "Tiene o ha tenido crédito formal"
attr(tmodulo1_2018$afore_hist, "label") <- "Tiene o ha tenido cuenta para retiro o afore"
attr(tmodulo1_2018$prod_fin_hist, "label") <- "Tiene o ha tenido un producto financiero"

#_______________________________________________________________________________

# 124) variable de usuario
#_______________________________________________________________________________
# Generar variable de usuario
tmodulo1_2018$usuario <- 0
tmodulo1_2018$usuario[tmodulo1_2018$prod_fin == 1] <- 1
tmodulo1_2018$usuario[tmodulo1_2018$prod_fin_hist == 1 & tmodulo1_2018$prod_fin == 0] <- 2

# Etiquetar la variable
attr(tmodulo1_2018$usuario, "label") <- "Población por tenencia de producto financiero"
levels(tmodulo1_2018$usuario) <- c("Nunca ha tenido", "Tiene producto", "Dejó de tener")

var_usuario <- table(tmodulo1_2018$usuario)
var_usuario

#####################################################################################

# INDICE DE ALFABETIZACIÓN FINANCIERA

#####################################################################################
#_______________________________________________________________________________

# 125) Subindice de conocimientos
#_______________________________________________________________________________
tmodulo1_2018$cono_intc2 <- tmodulo1_2018$cono_intc
tmodulo1_2018$cono_intc2[tmodulo1_2018$cono_ints_c == 0] <- 0

tmodulo1_2018$ind_cono <- tmodulo1_2018$cono_infc +
  tmodulo1_2018$cono_ints +
  tmodulo1_2018$cono_ints_c +
  tmodulo1_2018$cono_intc2 +
  tmodulo1_2018$cono_rie +
  tmodulo1_2018$cono_inf +
  tmodulo1_2018$cono_div

# Etiquetar la variable
attr(tmodulo1_2018$ind_cono, "label") <- "Subíndice de conocimientos"

#_______________________________________________________________________________

# 126)   *Subindice de comportamientos 2018
#_______________________________________________________________________________

# --------------Elaboración de presupuesto y toma de desiciones 
tmodulo1_2018$presup_deci <- ifelse((tmodulo2_2018$p13_1 == 1 | tmodulo2_2018$p13_1 == 2) & tmodulo1_2018$p4_1 == 1, 1, 0)

# --------------Compras cuidadosas
tmodulo1_2018$Compras_cuidadosas <- ifelse(tmodulo1_2018$comp_ant == 1, 1.333, 0)

# --------------pago puntual de deudas
tmodulo1_2018$pago_puntual_deudas <- ifelse(tmodulo1_2018$comp_pat == 1, 1.333, 0)

# --------------Establecimiento de metas a largo plazo
tmodulo1_2018$Metas_largo_plazo <- ifelse(tmodulo1_2018$comp_mlp == 1, 1.333, 0)

#-----------------------
# Ahorro Informal 
tmodulo1_2018$aho_inf 

# Ahorro Formal 
tmodulo1_2018$aho_for 

# Ahorro activo
tmodulo1_2018$ahorro
#-----------------------

#Suficiente Gasto
tmodulo1_2018$suficiente_gasto <- ifelse(tmodulo1_2018$p4_5 == 1, 1, 0)

#La última vez que no pudo cubrir sus gastos, ¿usted utilizó dinero que tenía ahorrado o redujo sus gastos?
tmodulo1_2018$sobregasto <- ifelse(tmodulo1_2018$sobregas_aho == 1, 1, 0)

# --------------préstamos para cubrir gastos del mes
tmodulo1_2018$sobregas2 <- ifelse(tmodulo1_2018$suficiente_gasto == 1 | tmodulo1_2018$sobregasto == 1, 1, 0)

# --------------Comparación de productos antes de adquirirlos
tmodulo1_2018$comp_prod_antes <- ifelse(tmodulo1_2018$compara_cuenta == 1 | tmodulo1_2018$compara_cred == 1 | tmodulo1_2018$compara_seguro == 1, 1, 0)
#	*SPF_04
#gen compara2=(compara_cuenta==1 | compara_cred==1 | compara_seguro==1)


#----------------Asesoramiento Independiente
tmodulo1_2018$ase_ind <- ifelse(tmodulo1_2018$compara_cuenta_con == 1 | tmodulo1_2018$compara_cuenta_esp == 1 |       # Cuentas
                                tmodulo1_2018$compara_cred_con == 1 | tmodulo1_2018$compara_cred_esp == 1 |           # Créditos
                                tmodulo1_2018$compara_seguro_con == 1 | tmodulo1_2018$compara_seguro_esp == 1, 1, 0)  # Seguros

# ------------------------------------------------------------------------------
# Calcular el subíndice de comportamientos 'ind_comp' 
# ------------------------------------------------------------------------------
tmodulo1_2018$ind_comp <- tmodulo1_2018$presup_deci + tmodulo1_2018$Compras_cuidadosas + tmodulo1_2018$pago_puntual_deudas + tmodulo1_2018$Metas_largo_plazo + tmodulo1_2018$ahorro + tmodulo1_2018$sobregas2 + tmodulo1_2018$comp_prod_antes + tmodulo1_2018$ase_ind
tmodulo1_2018$ind_comp

# Etiquetar la variable
attr(tmodulo1_2018$ind_comp, "label") <- "Subíndice de comportamientos"

#_______________________________________________________________________________

# 126) Subindice de actitudes
#_______________________________________________________________________________
tmodulo1_2018$ind_act <- tmodulo1_2018$comp_pga2 

# Etiquetar la variable
attr(tmodulo1_2018$ind_act, "label") <- "Subíndice de actitudes"

#_______________________________________________________________________________

# 127) Indice de alfabetizacion financiera
#_______________________________________________________________________________
# Indice de alfabetizacion financiera
tmodulo1_2018$ind_fin <- tmodulo1_2018$ind_comp + tmodulo1_2018$ind_act + tmodulo1_2018$ind_cono

# Etiquetar la variable
attr(tmodulo1_2018$ind_fin, "label") <- "Índice de alfabetización financiera"

#_______________________________________________________________________________

# 127) Estandariamos los indices
#_______________________________________________________________________________
# Estandarizar los indices
# ------------------------------------------------------------------------------
tmodulo1_2018$ind_cono <- tmodulo1_2018$ind_cono * 100 / 7
tmodulo1_2018$ind_comp <- tmodulo1_2018$ind_comp * 100 / 9
tmodulo1_2018$ind_act <- tmodulo1_2018$ind_act * 100 / 5
tmodulo1_2018$ind_fin <- tmodulo1_2018$ind_fin * 100 / 21


#####################################################################################

# PRUEBA DE INDICE CON ANALISIS DE CORRESPONDENCIA

#####################################################################################
# Subindice de conocimientos
tmodulo1_2018$ind_cono_mca <- scale(tmodulo1_2018$cono_infc + tmodulo1_2018$cono_ints + tmodulo1_2018$cono_ints_c + tmodulo1_2018$cono_intc2 + tmodulo1_2018$cono_rie + tmodulo1_2018$cono_inf + tmodulo1_2018$cono_div)

# Subindice de comportamientos

tmodulo1_2018$comp_mlp2 <- ifelse(tmodulo1_2018$comp_mlp == 2, 1, 0)
tmodulo1_2018$comp_ant2 <- ifelse(tmodulo1_2018$comp_ant == 2, 1, 0)
tmodulo1_2018$comp_pat2 <- ifelse(tmodulo1_2018$comp_pat == 2, 1, 0)


tmodulo1_2018$ind_comp_mca <- scale(tmodulo1_2018$ahorro + 
                                      tmodulo1_2018$sobregas2 + 
                                      tmodulo1_2018$comp_prod_antes + 
                                      tmodulo1_2018$ase_ind + 
                                      tmodulo1_2018$comp_mlp2 + 
                                      tmodulo1_2018$comp_ant2 + 
                                      tmodulo1_2018$comp_pat2)


# Subindice de actitudes
tmodulo1_2018$ind_act_mca <- scale(tmodulo1_2018$ind_act)

# Indice de alfabetizacion por pca
tmodulo1_2018$ind_fin_pca <- scale(tmodulo1_2018$ind_cono_mca + tmodulo1_2018$ind_comp_mca + tmodulo1_2018$ind_act_mca)

################################################################################
#-------------------------------------------------------------------------------
# Directorio
#setwd("Desktop")  # Ajusta la ubicación según tu sistema
## Crear folders de almacenamiento
dir.create("enif", showWarnings = FALSE, recursive = TRUE)
#-----------------------------------------------------------------

# Cambia el directorio de trabajo al nuevo directorio "enif"
setwd("enif")
# Guardar csv
write.csv(tmodulo1_2018, file = "tmodulo1_2018_clean.csv", row.names = FALSE)
