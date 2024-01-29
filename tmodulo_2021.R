
#_______________________________________________________________________________

# **********  ENCUESTA NACIONAL DE INCLUCION FINANCIERA (ENIF) 2021  ***********
# **************************  REPORTE DE RESULTADOS ****************************
#_______________________________________________________________________________
#####################################################################################

# LIMPIEZA Y DESCARGA DE tmodulo_2021
# tmodulo_2021

#####################################################################################
#-------------------------------------------------------------------------------
# Borrar datos del entorno
#-------------------------------------------------------------------------------
rm(list = ls())
#-------------------------------------------------------------------------------
# Directorio

#setwd("Desktop")  # Ajusta la ubicacion segun tu sistema
#raiz <- setwd("C:\\[Tu ubicación]")
raiz <- setwd("C:\\Users\\hp\\Documents\\PP")

## Crear folders de almacenamiento
dir.create("enif", showWarnings = FALSE, recursive = TRUE)
#-------------------------------------------------------------------------------
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
download_and_unzip("https://www.inegi.org.mx/contenidos/programas/enif/2021/microdatos/enif_2021_bd_csv.zip", "enif/enif_2021")

#-------------------------------------------------------------------------------
# Eliminar archivos zip
file.remove("enif/enif_2021_bd_csv.zip")
#-------------------------------------------------------------------------------
# Leer los tmodulo_2021
#-------------------------------------------------------------------------------
# 2021
tmodulo_2021 <- read_csv("enif/enif_2021/TMODULO.csv") %>%
  janitor::clean_names()
#-------------------------------------------------------------------------------                          
# Limpieza de tmodulo_2021 para el MODULO de la ENIF 2021 (archivo CSV)
#-------------------------------------------------------------------------------
tmodulo_2021 <- read.csv("enif/enif_2021/TMODULO.csv")

# Reemplazar los valores NA por 100 en todo el tmodulo_2021frame
#tmodulo_2021[is.na(tmodulo_2021) & names(tmodulo_2021) != "P3_8A"] <- 100

# Cambiar el tipo de dato de las columnas "UPM_DIS," "VIV_SEL," y "HOGAR" a caracteres (strings)
columnas_string <- c("UPM_DIS", "VIV_SEL", "HOGAR")
tmodulo_2021 <- tmodulo_2021 %>% mutate(across(all_of(columnas_string), as.character))

# Ajustar la longitud de las columnas "UPM_DIS," "VIV_SEL," y "HOGAR"
tmodulo_2021$UPM_DIS <- str_pad(tmodulo_2021$UPM_DIS, width = 5, side = "left", pad = "0")
tmodulo_2021$VIV_SEL <- str_pad(tmodulo_2021$VIV_SEL, width = 3, side = "left", pad = "0")
tmodulo_2021$HOGAR <- str_pad(tmodulo_2021$HOGAR, width = 2, side = "left", pad = "0")

# Crear una nueva columna con la concatenacion de las columnas UPM_DIS, VIV_SEL y HOGAR
tmodulo_2021 <- tmodulo_2021 %>%
  mutate(UVH = paste(UPM_DIS, VIV_SEL, HOGAR, sep = ""))

# Verificar 
tmodulo_2021$UVH

#_______________________________________________________________________________

#                                 VARIABLES DE INTERES
#_______________________________________________________________________________

# 1) Nivel de escolaridad de la persona elegida (P3_1_1)
#_______________________________________________________________________________
tmodulo_2021$niv_ed_dgasf <- 0

# Generar la variable niv_ed_dgasf
tmodulo_2021 <- tmodulo_2021 %>%
  mutate(niv_ed_dgasf = NA) %>%
  mutate(niv_ed_dgasf = ifelse((P3_1_1 >= 0 & P3_1_1 <= 2) | P3_1_1 == 99, 1, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(P3_1_1 >= 3 & P3_1_1 <= 4, 2, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(P3_1_1 >= 5 & P3_1_1 <= 7, 3, niv_ed_dgasf),
         niv_ed_dgasf = ifelse(P3_1_1 >= 8 & P3_1_1 <= 9, 4, niv_ed_dgasf))

# Etiquetas para la nueva variable
levels(tmodulo_2021$niv_ed_dgasf) <- c(
  "Hasta primaria",
  "Hasta secundaria",
  "Hasta nivel medio superior",
  "Licenciatura o mas"
)

# Etiquetar la variable niv_ed_dgasf
attr(tmodulo_2021$niv_ed_dgasf, "label") <- "Nivel educativo"

#Vemos las columnasque tiene la columna niv_ed_dgasf
var_niv_ed_dgasf <- table(tmodulo_2021$niv_ed_dgasf)
print(var_niv_ed_dgasf)


tmodulo_2021$niv_ed_dgasf

#_______________________________________________________________________________

# 2) Estado conyugal de la persona elegida (P3_2)
#_______________________________________________________________________________

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P3_2"] <- "edo_civ"

# Etiqueta 
attr(tmodulo_2021$edo_civ, "label") <- "Estado civil"

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
tmodulo_2021$edo_civ <- sapply(tmodulo_2021$edo_civ, estado_civil)

# Crear variable casado
tmodulo_2021$casado <- ifelse(tmodulo_2021$edo_civ == 1, 1, 0)

# Etiquetar variable casado
attr(tmodulo_2021$casado, "label") <- "¿Está casado o en unión libre?"
levels(tmodulo_2021$casado) <- c("No", "Sí")

tmodulo_2021$niv_ed_dgasf
tmodulo_2021$edo_civ
tmodulo_2021$casado 
#_______________________________________________________________________________

# 3) Lengua indigena y apoyos gobierno
#_______________________________________________________________________________

# Condición de habla lengua indígena de la persona elegida (P3_3)
# La persona elegida es beneficiaria(o) de programas sociales (P3_4)
#-------------------------------------------------------------------------------
# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P3_3"] <- "lengua_ind"
names(tmodulo_2021)[names(tmodulo_2021) == "P3_4"] <- "apoyo_gob"

# Etiquetar variable casado
attr(tmodulo_2021$lengua_ind, "label") <- "Habla lengua indigena"
attr(tmodulo_2021$apoyo_gob, "label") <- "Recibe apoyos de gobierno"

# --------------------- Función de respuesta  
respuesta <- function(resp) {
  if (resp == 1) {
    return(1) 
  } else if (resp == 2) {
    return(0) 
  } else {
    return("Ninguno")
  }
}

tmodulo_2021$lengua_ind <- sapply(tmodulo_2021$lengua_ind, respuesta)
tmodulo_2021$apoyo_gob <- sapply(tmodulo_2021$apoyo_gob, respuesta)

#Vemos las variables
var_lengua_ind <- table(tmodulo_2021$lengua_ind)
var_apoyo_gob <- table(tmodulo_2021$apoyo_gob)

tmodulo_2021$lengua_ind
tmodulo_2021$apoyo_gob 
#_______________________________________________________________________________

# 4) Situacion laboral
#_______________________________________________________________________________

# Condición de actividad de la persona elegida (P3_5)
# Verificación de actividad (P3_6)
# Posición en la ocupación de la persona elegida (P3_7)
#-------------------------------------------------------------------------------
tmodulo_2021$ocup <- 0

# Condiciones
tmodulo_2021$ocup <- ifelse(tmodulo_2021$P3_5 == 1 | tmodulo_2021$P3_5 == 2, 1, ifelse(tmodulo_2021$P3_5 == 3, 0, NA))

tmodulo_2021$ocup[tmodulo_2021$P3_6 >= 1 & tmodulo_2021$P3_6 < 6] <- 1
tmodulo_2021$ocup[tmodulo_2021$P3_7 == 1] <- 0

# Etiquetas para la variable 'ocup'
levels(tmodulo_2021$ocup) <- c("Desocupado", "Ocupado")

#-------------------------------------------------------------------------------
# Crear una variable 'trab'
tmodulo_2021$trab <- tmodulo_2021$ocup == 1

# Etiqueta para la variable 'trab'
levels(tmodulo_2021$trab) <- c("No", "Sí")
#-------------------------------------------------------------------------------
# Crear una variable 'pea'
tmodulo_2021$pea <- tmodulo_2021$P3_5 == 1 | tmodulo_2021$P3_5 == 2 | tmodulo_2021$P3_5 == 3

# Etiquetas para la variable 'pea'
levels(tmodulo_2021$pea) <- c("PNEA", "PEA")
#-------------------------------------------------------------------------------
#Vemos las variables
var_ocup <- table(tmodulo_2021$ocup)
var_trab <- table(tmodulo_2021$trab)
var_pea <- table(tmodulo_2021$pea)

# Etiquetar variables
attr(tmodulo_2021$ocup, "label") <- "Situacion laboral"
attr(tmodulo_2021$trab, "label") <- "Encuestado trabajo remunerado"
attr(tmodulo_2021$pea, "label") <- "Poblacion economicamente activa"

var_ocup
var_trab
var_pea

tmodulo_2021$ocup
tmodulo_2021$trab
tmodulo_2021$pea
#_______________________________________________________________________________

# 5) Posicion laboral
#_______________________________________________________________________________
# Monto total de ingresos por trabajo de la persona elegida (P3_8A)
# Periodo de ingreso por trabajo de la persona elegida (P3_8B)
var_P3_8A <- table(tmodulo_2021$P3_8A)


# Crear la variable 'Ingmen' en el tmodulo_2021frame tmodulo_2021
tmodulo_2021$Ingmen <- with(tmodulo_2021, ifelse(P3_8A == 0, 0,
                                                 ifelse(P3_8B == 1 & P3_8A != 99888, P3_8A * 4,
                                                        ifelse(P3_8B == 2 & P3_8A != 99888, P3_8A * 2,
                                                               ifelse(P3_8B == 3 & P3_8A != 99888, P3_8A,
                                                                      ifelse(P3_8B == 4 & P3_8A != 99888, P3_8A / 12, NA))))))

# Etiquetas para la variable 'pos_ocu'
attr(tmodulo_2021$Ingmen, "label") <- "Ingreso mensual, pesos"
tmodulo_2021$Ingmen
#_______________________________________________________________________________

# 6) Ingreso fijo o variable
#_______________________________________________________________________________
# Crear la variable 'ing_fijo' 
tmodulo_2021$ing_fijo <- ifelse(tmodulo_2021$P3_9 == 1, 1,
                                ifelse(tmodulo_2021$P3_9 == 2, 0, NA))

# Etiqueta para la variable 'ing_fijo'
attr(tmodulo_2021$ing_fijo, "label") <- "Ingreso fijo o variable"

# Definir etiquetas específicas
levels(tmodulo_2021$ing_fijo) <- c("Ingreso variable", "Ingreso fijo")

#Vemos las variables
var_ing_fijo <- table(tmodulo_2021$ing_fijo)
tmodulo_2021$ing_fijo
#_______________________________________________________________________________

# 7) Acceso a servicios medicos
#_______________________________________________________________________________
# Derechohabiencia de la persona elegida (P3_10)
# Crear la variable 'Serv_med' 
tmodulo_2021$Serv_med <- ifelse(tmodulo_2021$P3_10 >= 1 & tmodulo_2021$P3_10 < 6, TRUE, FALSE)

# Etiqueta para la variable 'ing_fijo'
attr(tmodulo_2021$Serv_med, "label") <- "Tiene acceso a servicios médicos"

# Definir etiquetas específicas
levels(tmodulo_2021$Serv_med) <- c("No", "Sí")
tmodulo_2021$Serv_med 
#_______________________________________________________________________________

# 8) Trabajador formal
#_______________________________________________________________________________
# Crear la variable 'formal' 
tmodulo_2021$formal <- ifelse(tmodulo_2021$trab == 1, 0,
                              ifelse(!is.na(tmodulo_2021$formal) & tmodulo_2021$serv_med == "Sí", 1, NA))

# Etiqueta para la variable 'formal'
attr(tmodulo_2021$formal, "label") <- "Trabajador formal"

# Definir etiquetas específicas
levels(tmodulo_2021$formal) <- c("Informal", "Formal")
tmodulo_2021$formal
#_______________________________________________________________________________

# 9) Tiene telefono inteligente
#_______________________________________________________________________________
# La persona elegida tiene un celular inteligente (smartphone) (P3_11)
# Crear la variable 'celular' 
tmodulo_2021$celular <- ifelse(tmodulo_2021$P3_11 == 2, 0, tmodulo_2021$P3_11)

# Etiqueta para la variable 'celular'
attr(tmodulo_2021$celular, "label") <- "Tiene celular inteligente"

# Definir etiquetas específicas
levels(tmodulo_2021$celular) <- c("No", "Sí")
tmodulo_2021$celular
#####################################################################################

# ACTITUDES Y COMPORTAMIENTOS FINANCIEROS

#####################################################################################
#_______________________________________________________________________________

# 10) Lleva presupuesto
#_______________________________________________________________________________
# Crear la variable 'presup' 
tmodulo_2021$presup <- ifelse(tmodulo_2021$P4_1 == 2, 0, tmodulo_2021$P4_1)

# Etiqueta para la columna presup en tmodulo_2021
attr(tmodulo_2021$presup, "label") <- "Lleva un presupuesto"

# Definir etiquetas específicas
levels(tmodulo_2021$presup) <- c("No", "Sí")
tmodulo_2021$presup
#_______________________________________________________________________________

# 11) Control de gastos                                                       
#_______________________________________________________________________________
names(tmodulo_2021)[names(tmodulo_2021) == "P4_2_1"] <- "contr_anogas"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_2_2"] <- "contr_sepdin"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_2_3"] <- "contr_regdeu"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_2_4"] <- "contr_aplcel"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_2_5"] <- "contr_cobaut"

#Renombrar variables de la columna
tmodulo_2021$contr_anogas <- ifelse(tmodulo_2021$contr_anogas == 1, 1, 0)
tmodulo_2021$contr_sepdin <- ifelse(tmodulo_2021$contr_sepdin == 1, 1, 0)
tmodulo_2021$contr_regdeu <- ifelse(tmodulo_2021$contr_regdeu == 1, 1, 0)
tmodulo_2021$contr_aplcel <- ifelse(tmodulo_2021$contr_aplcel == 1, 1, 0)
tmodulo_2021$contr_cobaut <- ifelse(tmodulo_2021$contr_cobaut == 1, 1, 0)

# Etiquetas para las columnas
attr(tmodulo_2021$contr_anogas, "label") <- "Anota sus gastos"
attr(tmodulo_2021$contr_sepdin, "label") <- "Separa dinero de gasto diario de deuda"
attr(tmodulo_2021$contr_regdeu, "label") <- "Lleva registro de deudas pendientes"
attr(tmodulo_2021$contr_aplcel, "label") <- "Utiliza aplicación celular para seguir gastos"
attr(tmodulo_2021$contr_cobaut, "label") <- "Utiliza cobro automático"

#_______________________________________________________________________________

# 12) Sobregasto
#_______________________________________________________________________________
tmodulo_2021$sobregas <- ifelse(tmodulo_2021$P4_3 == 1, 0,
                                ifelse(tmodulo_2021$P4_3 == 2, 1, NA))

# Definir etiquetas específicas con set_label
levels(tmodulo_2021$sobregas) <- c("No", "Sí")

# Etiqueta para la variable sobregas en tmodulo_2021
attr(tmodulo_2021$sobregas, "label") <- "Ingreso insuficiente para cubrir gastos en algún mes"

#_______________________________________________________________________________

# 13) Accion despues de no cubrir gastos
#_______________________________________________________________________________
names(tmodulo_2021)[names(tmodulo_2021) == "P4_4_1"] <- "sobregas_pfa"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_4_2"] <- "sobregas_aho"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_4_3"] <- "sobregas_rga"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_4_4"] <- "sobregas_bie"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_4_5"] <- "sobregas_ade"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_4_6"] <- "sobregas_cre"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_4_7"] <- "sobregas_atr"

#Renombrar variables de la columna
tmodulo_2021$sobregas_pfa <- ifelse(tmodulo_2021$sobregas_pfa == 1, 1, 0)
tmodulo_2021$sobregas_aho <- ifelse(tmodulo_2021$sobregas_aho == 1, 1, 0)
tmodulo_2021$sobregas_rga <- ifelse(tmodulo_2021$sobregas_rga == 1, 1, 0)
tmodulo_2021$sobregas_bie <- ifelse(tmodulo_2021$sobregas_bie == 1, 1, 0)
tmodulo_2021$sobregas_ade <- ifelse(tmodulo_2021$sobregas_ade == 1, 1, 0)
tmodulo_2021$sobregas_cre <- ifelse(tmodulo_2021$sobregas_cre == 1, 1, 0)
tmodulo_2021$sobregas_atr <- ifelse(tmodulo_2021$sobregas_atr == 1, 1, 0)

# Etiquetas para las columnas
attr(tmodulo_2021$sobregas_pfa, "label") <- "Pidió préstamo a familiares o conocidos para cubrir gastos"
attr(tmodulo_2021$sobregas_aho, "label") <- "Utilizó ahorros para cubrir gastos"
attr(tmodulo_2021$sobregas_rga, "label") <- "Redució gastos para afrontar sobregasto"
attr(tmodulo_2021$sobregas_bie, "label") <- "Empeñó o vendió bienes para cubrir gastos"
attr(tmodulo_2021$sobregas_ade, "label") <- "Solicitó adelanto de salario para cubrir gastos"
attr(tmodulo_2021$sobregas_cre, "label") <- "Solicitó crédito o utilizó tarjeta para cubrir gastos"
attr(tmodulo_2021$sobregas_atr, "label") <- "Se atrasó en crédito para afrontar sobregasto"

#-----------------------------------------------------------------------------
# Crear la variable sobregas_aho18 
tmodulo_2021$sobregas_aho18 <- ifelse(tmodulo_2021$sobregas_rga == 1 | tmodulo_2021$sobregas_aho == 1, 1, 0)

# Reemplazar valores NA
tmodulo_2021$sobregas_aho18[is.na(tmodulo_2021$sobregas_aho18) & tmodulo_2021$sobregas == 0] <- 0

# Etiqueta para la variable 
attr(tmodulo_2021$sobregas_aho18, "label") <- "Redujo gastos o utilizó ahorro para afrontar sobregasto"

# Definir etiquetas específicas
levels(tmodulo_2021$sobregas_aho18) <- c("No", "Sí")
tmodulo_2021$sobregas_aho18
#_______________________________________________________________________________

# 14) Cursos sobre temas financieros
#_______________________________________________________________________________
#  Variable curso_fin 
tmodulo_2021$curso_fin <- ifelse(tmodulo_2021$P4_5 == 2, 0, tmodulo_2021$P4_5)

# Definir etiquetas específicas
levels(tmodulo_2021$curso_fin) <- c("No", "Sí")

# Etiqueta para la variable curso_fin en tmodulo_2021
attr(tmodulo_2021$curso_fin, "label") <- "Tomó curso de educación financiera"
tmodulo_2021$curso_fin

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
tmodulo_2021$comp_pga2 <- sapply(tmodulo_2021$P4_6_3, codificar_comp_pga)

# Definir etiquetas específicas
levels(tmodulo_2021$comp_pga2) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
tmodulo_2021$comp_pga2
#-----------------------------------------------------------------------------
# renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P4_6_1"] <- "comp_ant"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_6_2"] <- "comp_pat"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_6_3"] <- "comp_pga"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_6_4"] <- "comp_mlp"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_6_5"] <- "comp_igc"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_6_6"] <- "comp_sdm"

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
tmodulo_2021$comp_ant <- sapply(tmodulo_2021$comp_ant, Comportamiento_financiero)
tmodulo_2021$comp_pat <- sapply(tmodulo_2021$comp_pat, Comportamiento_financiero)
tmodulo_2021$comp_pga <- sapply(tmodulo_2021$comp_pga, Comportamiento_financiero)
tmodulo_2021$comp_mlp <- sapply(tmodulo_2021$comp_mlp, Comportamiento_financiero)
tmodulo_2021$comp_igc <- sapply(tmodulo_2021$comp_igc, Comportamiento_financiero)
tmodulo_2021$comp_sdm <- sapply(tmodulo_2021$comp_sdm, Comportamiento_financiero)

# Etiquetas para las variables
attr(tmodulo_2021$comp_ant, "label") <- "Piensa si puede pagar antes de comprar"
attr(tmodulo_2021$comp_pat, "label") <- "Paga cuentas a tiempo"
attr(tmodulo_2021$comp_pga, "label") <- "Prefiere gastar antes de ahorrar"
attr(tmodulo_2021$comp_mlp, "label") <- "Se pone metas económicas de largo plazo"
attr(tmodulo_2021$comp_igc, "label") <- "Ingresos y gastos controlan su vida"
attr(tmodulo_2021$comp_sdm, "label") <- "Le sobra dinero al final de mes"

# Etiquetas para las variables
levels(tmodulo_2021$comp_ant) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
levels(tmodulo_2021$comp_pat) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
levels(tmodulo_2021$comp_pga) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
levels(tmodulo_2021$comp_mlp) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
levels(tmodulo_2021$comp_igc) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
levels(tmodulo_2021$comp_sdm) <- c("Nunca / No sabe", "Algunas veces", "Siempre")
#_______________________________________________________________________________

# 16) Riesgos y diversificacion
#_______________________________________________________________________________
# Crear nuevas columnas
tmodulo_2021$cono_inf <- ifelse(tmodulo_2021$P4_7_1 == 1, 1, 0)
tmodulo_2021$cono_rie <- ifelse(tmodulo_2021$P4_7_2 == 1, 1, 0)
tmodulo_2021$cono_div <- ifelse(tmodulo_2021$P4_7_3 == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo_2021$cono_inf, "label") <- "Conoce concepto de inflación"
attr(tmodulo_2021$cono_rie, "label") <- "Conoce concepto de riesgo inversión"
attr(tmodulo_2021$cono_div, "label") <- "Conoce concepto de diversificación de riesgo"

# Definir etiquetas específicas
levels(tmodulo_2021$cono_inf) <- c("No", "Sí")
levels(tmodulo_2021$cono_rie) <- c("No", "Sí")
levels(tmodulo_2021$cono_div) <- c("No", "Sí")

tmodulo_2021$cono_inf
tmodulo_2021$cono_rie
tmodulo_2021$cono_div
#_______________________________________________________________________________

# 17) Otros comportamientos
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P4_8_1"] <- "comp_pre"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_8_2"] <- "comp_dpg"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_8_3"] <- "comp_rdd"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_8_4"] <- "comp_tcd"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_8_5"] <- "comp_cub"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_8_6"] <- "comp_tds"

# Función para recodificar según las reglas de frecuencia
Otros_comportamientos <- function(valor) {
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

# Crear nuevas columnas usando sapply
tmodulo_2021$comp_pre <- sapply(tmodulo_2021$comp_pre, Otros_comportamientos)
tmodulo_2021$comp_dpg <- sapply(tmodulo_2021$comp_dpg, Otros_comportamientos)
tmodulo_2021$comp_rdd <- sapply(tmodulo_2021$comp_rdd, Otros_comportamientos)
tmodulo_2021$comp_tcd <- sapply(tmodulo_2021$comp_tcd, Otros_comportamientos)
tmodulo_2021$comp_cub <- sapply(tmodulo_2021$comp_cub, Otros_comportamientos)
tmodulo_2021$comp_tds <- sapply(tmodulo_2021$comp_tds, Otros_comportamientos)

# Etiquetas para las nuevas variables
attr(tmodulo_2021$comp_pre, "label") <- "Piensa en presente sin preocuparse del futuro"
attr(tmodulo_2021$comp_dpg, "label") <- "Dinero está para gastarse"
attr(tmodulo_2021$comp_rdd, "label") <- "Mantiene revisión detallada de su dinero"
attr(tmodulo_2021$comp_tcd, "label") <- "Siente que tendrá las cosas que desea"
attr(tmodulo_2021$comp_cub, "label") <- "Le alcanza bien para cubrir gastos"
attr(tmodulo_2021$comp_tds, "label") <- "Está tranquilo de que el dinero es suficiente"

# Etiquetas para las variables
levels(tmodulo_2021$comp_pre) <- c("En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo")
levels(tmodulo_2021$comp_dpg) <- c("En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo")
levels(tmodulo_2021$comp_rdd) <- c("En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo")
levels(tmodulo_2021$comp_tcd) <- c("En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo")
levels(tmodulo_2021$comp_cub) <- c("En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo")
levels(tmodulo_2021$comp_tds) <- c("En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo")
#_______________________________________________________________________________

# 18) Aprovechar oportunidades
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P4_9_1"] <- "opor_aho"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_9_2"] <- "opor_cre"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_9_3"] <- "opor_emp"
names(tmodulo_2021)[names(tmodulo_2021) == "P4_9_4"] <- "opor_fam"

#Renombrar variables de la columna
tmodulo_2021$opor_aho <- ifelse(tmodulo_2021$opor_aho == 1, 1, 0)
tmodulo_2021$opor_cre <- ifelse(tmodulo_2021$opor_cre == 1, 1, 0)
tmodulo_2021$opor_emp <- ifelse(tmodulo_2021$opor_emp == 1, 1, 0)
tmodulo_2021$opor_fam <- ifelse(tmodulo_2021$opor_fam == 1, 1, 0)

# Etiquetas 
attr(tmodulo_2021$opor_aho, "label") <- "Podría aprovechar con ahorro"
attr(tmodulo_2021$opor_cre, "label") <- "Podría aprovechar con crédito o tarjeta"
attr(tmodulo_2021$opor_emp, "label") <- "Podría aprovechar empeñando o vendiendo bien"
attr(tmodulo_2021$opor_fam, "label") <- "Podría aprovechar con préstamo de familiares o amistades"


# Crear nueva variable opor_np
tmodulo_2021$opor_np <- (tmodulo_2021$opor_aho == 0 & tmodulo_2021$opor_cre == 0 & tmodulo_2021$opor_emp == 0 & tmodulo_2021$opor_fam == 0)

# Etiqueta 
attr(tmodulo_2021$opor_np, "label") <- "No podría tomar la oportunidad"

#_______________________________________________________________________________

# 19) Vulnerabilidad financiera
#_______________________________________________________________________________
# Renombrar columna
names(tmodulo_2021)[names(tmodulo_2021) == "P4_10"] <- "vuln_fin"

# Recodificar 
tmodulo_2021$vuln_fin[tmodulo_2021$vuln_fin %in% c(8, 9)] <- 0

# Etiqueta 
attr(tmodulo_2021$vuln_fin, "label") <- "Cuánto tiempo podría cubrir gastos con ahorros"

# Etiquetas para la nueva variable
levels(tmodulo_2021$vuln_fin) <- c(
  "Sin ahorros/Menos 1 sem",
  "Mas 1 sem, menos 1 mes",
  "Mas 1 mes, menos 3 meses",
  "Mas 3 meses, menos 6 meses",
  "Seis meses o mas"
)

tmodulo_2021$vuln_fin

#####################################################################################

# Ahorro formal e informal

#####################################################################################

#_______________________________________________________________________________

# 20) Tiene ahorro informal
#_______________________________________________________________________________
# Crear variable aho_inf e inicializarla con 0
tmodulo_2021$aho_inf <- 0

# Actualizar aho_inf a 1 si se cumple alguna de las condiciones
tmodulo_2021$aho_inf[tmodulo_2021$P5_1_1 == 1 | tmodulo_2021$P5_1_2 == 1 | tmodulo_2021$P5_1_3 == 1 |
                       tmodulo_2021$P5_1_4 == 1 | tmodulo_2021$P5_1_5 == 1 | tmodulo_2021$P5_1_6 == 1] <- 1

# Etiqueta
attr(tmodulo_2021$aho_inf, "label") <- "Tiene ahorro informal (tandas, familiares, cajas de ahorro)"

#_______________________________________________________________________________

# 21) Tipo de ahorro informal
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P5_1_1"] <- "aho_inf_pre"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_1_2"] <- "aho_inf_bie"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_1_3"] <- "aho_inf_caj"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_1_4"] <- "aho_inf_fam"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_1_5"] <- "aho_inf_tan"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_1_6"] <- "aho_inf_cas"

#Renombrar variables de la columna
tmodulo_2021$aho_inf_pre <- ifelse(tmodulo_2021$aho_inf_pre == 1, 1, 0)
tmodulo_2021$aho_inf_bie <- ifelse(tmodulo_2021$aho_inf_bie == 1, 1, 0)
tmodulo_2021$aho_inf_caj <- ifelse(tmodulo_2021$aho_inf_caj == 1, 1, 0)
tmodulo_2021$aho_inf_fam <- ifelse(tmodulo_2021$aho_inf_fam == 1, 1, 0)
tmodulo_2021$aho_inf_tan <- ifelse(tmodulo_2021$aho_inf_tan == 1, 1, 0)
tmodulo_2021$aho_inf_cas <- ifelse(tmodulo_2021$aho_inf_cas == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo_2021$aho_inf_pre, "label") <- "Ahorro prestando dinero"
attr(tmodulo_2021$aho_inf_bie, "label") <- "Ahorro comprando animales o bienes"
attr(tmodulo_2021$aho_inf_caj, "label") <- "Ahorro en caja del trabajo o conocidos"
attr(tmodulo_2021$aho_inf_fam, "label") <- "Ahorro guardando con familiares o conocidos"
attr(tmodulo_2021$aho_inf_tan, "label") <- "Ahorro participando en tanda"
attr(tmodulo_2021$aho_inf_cas, "label") <- "Ahorro dinero en su casa"

#_______________________________________________________________________________

# 22) Destino del ahorro informal
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_1"] <- "aho_inf_u_gas"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_2"] <- "aho_inf_u_eme"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_3"] <- "aho_inf_u_cas"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_4"] <- "aho_inf_u_sal"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_5"] <- "aho_inf_u_edu"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_6"] <- "aho_inf_u_vac"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_7"] <- "aho_inf_u_neg"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_8"] <- "aho_inf_u_ret"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_2_9"] <- "aho_inf_u_otr"

# Etiquetas para las variables
attr(tmodulo_2021$aho_inf_u_gas, "label") <- "Uso en gastos personales"
attr(tmodulo_2021$aho_inf_u_eme, "label") <- "Uso en emergencias"
attr(tmodulo_2021$aho_inf_u_cas, "label") <- "Uso en compra casa, terreno, remod"
attr(tmodulo_2021$aho_inf_u_sal, "label") <- "Uso en gastos salud"
attr(tmodulo_2021$aho_inf_u_edu, "label") <- "Uso en gastos educacion"
attr(tmodulo_2021$aho_inf_u_vac, "label") <- "Uso en vacaciones y fiestas"
attr(tmodulo_2021$aho_inf_u_neg, "label") <- "Uso en negocio"
attr(tmodulo_2021$aho_inf_u_ret, "label") <- "Uso en vejez o retiro"
attr(tmodulo_2021$aho_inf_u_otr, "label") <- "Uso en otro"

#_______________________________________________________________________________

# 23) Equivalencia del ahorro informal
#_______________________________________________________________________________
# Renombrar columna
names(tmodulo_2021)[names(tmodulo_2021) == "P5_3"] <- "aho_inf_equiv"

# Reemplazar valores
tmodulo_2021$aho_inf_equiv <- replace(tmodulo_2021$aho_inf_equiv, 
                                      tmodulo_2021$aho_inf_equiv %in% c(9), 
                                      0)

# Etiqueta 
attr(tmodulo_2021$aho_inf_equiv, "label") <- "Equivalencia en ingreso del ahorro informal"

# Etiquetar los valores
labels_equiv <- c("Una semana", "Una quincena", "Un mes", "Mas de un mes")

#_______________________________________________________________________________

# 24) Tiene cuenta bancaria o con institución financiera
#_______________________________________________________________________________

tmodulo_2021$cuenta <- ifelse(tmodulo_2021$P5_4_1 == 1 | tmodulo_2021$P5_4_2 == 1 |
                                tmodulo_2021$P5_4_3 == 1 | tmodulo_2021$P5_4_4 == 1 |
                                tmodulo_2021$P5_4_5 == 1 | tmodulo_2021$P5_4_6 == 1 |
                                tmodulo_2021$P5_4_7 == 1 | tmodulo_2021$P5_4_8 == 1 |
                                tmodulo_2021$P5_4_9 == 1, 1, 0)

# Etiqueta 
attr(tmodulo_2021$cuenta, "label") <- "Tiene alguna cuenta"
#_______________________________________________________________________________

# 24) Tipos de cuentas
#_______________________________________________________________________________
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_1"] <- "cuenta_nom"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_2"] <- "cuenta_pen"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_3"] <- "cuenta_gob"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_4"] <- "cuenta_aho"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_5"] <- "cuenta_che"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_6"] <- "cuenta_pla"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_7"] <- "cuenta_inv"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_8"] <- "cuenta_apl"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_4_9"] <- "cuenta_otr"

# Etiquetas para las variables
attr(tmodulo_2021$cuenta_nom, "label") <- "Tiene cuenta de nómina"
attr(tmodulo_2021$cuenta_pen, "label") <- "Tiene cuenta de pensión"
attr(tmodulo_2021$cuenta_gob, "label") <- "Tiene cuenta de apoyos de gobierno"
attr(tmodulo_2021$cuenta_aho, "label") <- "Tiene cuenta de ahorro"
attr(tmodulo_2021$cuenta_che, "label") <- "Tiene cuenta de cheques"
attr(tmodulo_2021$cuenta_pla, "label") <- "Tiene depósito a plazo fijo"
attr(tmodulo_2021$cuenta_inv, "label") <- "Tiene cuenta de inversión"
attr(tmodulo_2021$cuenta_apl, "label") <- "Tiene cuenta contratada por internet o aplicación"
attr(tmodulo_2021$cuenta_otr, "label") <- "Tiene cuenta de otro tipo"

# Crear variables
tmodulo_2021$cuenta_invpla <- ifelse(tmodulo_2021$cuenta_inv == 1 | tmodulo_2021$cuenta_pla == 1, 1, 0)
tmodulo_2021$cuenta_ahoche <- ifelse(tmodulo_2021$cuenta_aho == 1 | tmodulo_2021$cuenta_che == 1, 1, 0)
tmodulo_2021$cuenta_otrgob <- ifelse(tmodulo_2021$cuenta_pen == 1 | tmodulo_2021$cuenta_gob == 1 | tmodulo_2021$cuenta_otr == 1, 1, 0)

# Etiquetas para las variables
attr(tmodulo_2021$cuenta_invpla, "label") <- "Tiene cuenta de inversión o a plazos"
attr(tmodulo_2021$cuenta_ahoche, "label") <- "Tiene cuenta de ahorro o de cheques"
attr(tmodulo_2021$cuenta_otrgob, "label") <- "Tiene otro tipo de cuenta (pensión, gobierno, otros)"

#_______________________________________________________________________________

# 25) Tarjeta de debito
#_______________________________________________________________________________
# Crear variable tarj_deb e inicializarla con 0
tmodulo_2021$tarj_deb <- 0

# Actualizar tarj_deb a 1 si se cumple alguna de las condiciones
tmodulo_2021$tarj_deb[tmodulo_2021$P5_6_1 == 1 | tmodulo_2021$P5_6_2 == 1 |
                        tmodulo_2021$P5_6_3 == 1 | tmodulo_2021$P5_6_4 == 1 |
                        tmodulo_2021$P5_6_5 == 1 | tmodulo_2021$P5_6_8 == 1 |
                        tmodulo_2021$P5_6_9 == 1] <- 1

# Etiqueta para la variable tarj_deb en tmodulo_2021
attr(tmodulo_2021$tarj_deb, "label") <- "Tiene tarjeta de débito"
#_______________________________________________________________________________

# 26) Ahorro formal efectivo
#_______________________________________________________________________________
# Crear variable aho_for e inicializarla con 0
tmodulo_2021$aho_for <- 0

# Actualizar aho_for a 1 si se cumple alguna de las condiciones
tmodulo_2021$aho_for[tmodulo_2021$P5_7_1 == 1 | tmodulo_2021$P5_7_2 == 1 |
                       tmodulo_2021$P5_7_3 == 1 | tmodulo_2021$P5_7_4 == 1 |
                       tmodulo_2021$P5_7_5 == 1 | tmodulo_2021$P5_7_6 == 1 |
                       tmodulo_2021$P5_7_7 == 1 | tmodulo_2021$P5_7_8 == 1 |
                       tmodulo_2021$P5_7_9 == 1] <- 1

# Etiqueta 
attr(tmodulo_2021$aho_for, "label") <- "Tiene ahorro formal"

#_______________________________________________________________________________

# 27) Tipo de ahorro formal
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_1"] <- "aho_for_nom"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_2"] <- "aho_for_pen"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_3"] <- "aho_for_gob"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_4"] <- "aho_for_aho"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_5"] <- "aho_for_che"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_6"] <- "aho_for_pla"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_7"] <- "aho_for_inv"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_8"] <- "aho_for_apl"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_7_9"] <- "aho_for_otr"

# Etiquetas 
attr(tmodulo_2021$aho_for_nom, "label") <- "Ahorro en cuenta de nómina"
attr(tmodulo_2021$aho_for_pen, "label") <- "Ahorro en cuenta de pensión"
attr(tmodulo_2021$aho_for_gob, "label") <- "Ahorro en cuenta de apoyos de gobierno"
attr(tmodulo_2021$aho_for_aho, "label") <- "Ahorro en cuenta de ahorro"
attr(tmodulo_2021$aho_for_che, "label") <- "Ahorro en cuenta de cheques"
attr(tmodulo_2021$aho_for_pla, "label") <- "Ahorro en depósito a plazo fijo"
attr(tmodulo_2021$aho_for_inv, "label") <- "Ahorro en cuenta de inversión"
attr(tmodulo_2021$aho_for_apl, "label") <- "Ahorro en cuenta contratada por internet o aplicación"
attr(tmodulo_2021$aho_for_otr, "label") <- "Ahorro en cuenta de otro tipo"

# Crear variables adicionales
tmodulo_2021$aho_for_invpla <- ifelse(tmodulo_2021$aho_for_inv == 1 | tmodulo_2021$aho_for_pla == 1, 1, 0)
tmodulo_2021$aho_for_ahoche <- ifelse(tmodulo_2021$aho_for_aho == 1 | tmodulo_2021$aho_for_che == 1, 1, 0)
tmodulo_2021$aho_for_otrgob <- ifelse(tmodulo_2021$aho_for_pen == 1 | tmodulo_2021$aho_for_gob == 1 | tmodulo_2021$aho_for_otr == 1, 1, 0)

# Etiquetas 
attr(tmodulo_2021$aho_for_invpla, "label") <- "Tiene cuenta de inversión o a plazos"
attr(tmodulo_2021$aho_for_ahoche, "label") <- "Tiene cuenta de ahorro o de cheques"
attr(tmodulo_2021$aho_for_otrgob, "label") <- "Tiene otro tipo de cuenta (pensión, gobierno, otros)"

# Crear variable ahorro
tmodulo_2021$ahorro <- ifelse(tmodulo_2021$aho_for == 1 | tmodulo_2021$aho_inf == 1, 1, 0)

# Etiqueta para la variable ahorro en tmodulo_2021
attr(tmodulo_2021$ahorro, "label") <- "Tiene algún tipo de ahorro (formal o informal)"

#_______________________________________________________________________________

# 28) Razones para adquirir cuenta
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P5_8_01"] <- "cuenta_raz_gah"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_8_02"] <- "cuenta_raz_inv"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_8_03"] <- "cuenta_raz_sal"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_8_04"] <- "cuenta_raz_neg"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_8_05"] <- "cuenta_raz_gob"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_8_06"] <- "cuenta_raz_sah"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_8_07"] <- "cuenta_raz_com"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_8_08"] <- "cuenta_raz_otr"

# Etiquetas para las variables
attr(tmodulo_2021$cuenta_raz_gah, "label") <- "Gusta ahorrar"
attr(tmodulo_2021$cuenta_raz_inv, "label") <- "Invertir o generar intereses"
attr(tmodulo_2021$cuenta_raz_sal, "label") <- "Recibe su salario del trabajo"
attr(tmodulo_2021$cuenta_raz_neg, "label") <- "Administra dinero de negocio"
attr(tmodulo_2021$cuenta_raz_gob, "label") <- "Recibe apoyos de gobierno"
attr(tmodulo_2021$cuenta_raz_sah, "label") <- "Es una forma segura de ahorro"
attr(tmodulo_2021$cuenta_raz_com, "label") <- "Forma sencilla de comprar"
attr(tmodulo_2021$cuenta_raz_otr, "label") <- "Otra razón"
#_______________________________________________________________________________

# 29) Destino del ahorro formal
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_01"] <- "aho_for_u_eme"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_02"] <- "aho_for_u_gas"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_03"] <- "aho_for_u_cas"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_04"] <- "aho_for_u_edu"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_05"] <- "aho_for_u_vac"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_06"] <- "aho_for_u_sal"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_07"] <- "aho_for_u_neg"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_08"] <- "aho_for_u_ret"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_9_09"] <- "aho_for_u_otr"

# Etiquetas para las variables
attr(tmodulo_2021$aho_for_u_eme, "label") <- "Uso en emergencias"
attr(tmodulo_2021$aho_for_u_gas, "label") <- "Uso en gastos personales"
attr(tmodulo_2021$aho_for_u_cas, "label") <- "Uso en compra casa, terreno, remodelación"
attr(tmodulo_2021$aho_for_u_edu, "label") <- "Uso en gastos educación"
attr(tmodulo_2021$aho_for_u_vac, "label") <- "Uso en vacaciones y fiestas"
attr(tmodulo_2021$aho_for_u_sal, "label") <- "Uso en gastos salud"
attr(tmodulo_2021$aho_for_u_neg, "label") <- "Uso en negocio"
attr(tmodulo_2021$aho_for_u_ret, "label") <- "Uso en vejez o retiro"
attr(tmodulo_2021$aho_for_u_otr, "label") <- "Uso en otro"
#_______________________________________________________________________________

# 30) Equivalencia del ahorro formal
#_______________________________________________________________________________
# Equivalencia del ahorro formal
names(tmodulo_2021)[names(tmodulo_2021) == "P5_10"] <- "aho_for_equiv"
tmodulo_2021$aho_for_equiv[tmodulo_2021$aho_for_equiv == 9] <- NA

# Etiqueta 
attr(tmodulo_2021$aho_for_equiv, "label") <- "Equivalencia en ingreso del ahorro formal"
#_______________________________________________________________________________

# 31) Sobre cuentas de nomina gratuita
#_______________________________________________________________________________

tmodulo_2021$cuenta_nom_gra <- ifelse(tmodulo_2021$P5_11 %in% c(2), 0, tmodulo_2021$P5_11)

# Etiqueta 
attr(tmodulo_2021$cuenta_nom_gra, "label") <- "Sabe que puede cambiar por cuenta de nómina gratuita"
#_______________________________________________________________________________

# 32) Frecuencia de uso de tarjeta de debito
#_______________________________________________________________________________
# Frecuencia de uso de tarjeta de débito
tmodulo_2021$tarj_deb_frec <- tmodulo_2021$P5_12

# Etiqueta 
attr(tmodulo_2021$tarj_deb_frec, "label") <- "Frecuencia uso mensual de tarjeta de débito"


tmodulo_2021$tarj_deb_frecg <- NA
tmodulo_2021$tarj_deb_frecg[tmodulo_2021$tarj_deb_frec == 0] <- 1
tmodulo_2021$tarj_deb_frecg[tmodulo_2021$tarj_deb_frec >= 1 & tmodulo_2021$tarj_deb_frec < 10 | tmodulo_2021$tarj_deb_frec == 88] <- 2
tmodulo_2021$tarj_deb_frecg[tmodulo_2021$tarj_deb_frec >= 10 & tmodulo_2021$tarj_deb_frec != 88 & !is.na(tmodulo_2021$tarj_deb_frec)] <- 3

# Cambiar nombres de niveles para tarj_deb_frecg
set_label(tmodulo_2021$tarj_deb_frecg, c("1" = "No la utiliza", "2" = "Menos de 10 veces al mes", "3" = "10 veces al mes o más"))

# Etiqueta 
attr(tmodulo_2021$tarj_deb_frecg, "label") <- "Frecuencia uso mensual de tarjeta de débito"
#_______________________________________________________________________________

# 33) Razones de no uso de tarjetas de debito
#_______________________________________________________________________________
tmodulo_2021$tarj_deb_raznu <- tmodulo_2021$P5_13

# Etiqueta de variables de la columna
set_label(tmodulo_2021$tarj_deb_raznu, c(
  "0" = "Blanco",
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
attr(tmodulo_2021$tarj_deb_raznu, "label") <- "Razon principal para no usar tarjeta de débito"
#_______________________________________________________________________________

# 34) Preferencia efectivo
#_______________________________________________________________________________
# Preferencia efectivo
tmodulo_2021$pref_efec <- tmodulo_2021$P5_14

# Etiqueta para la variable 'pref_efec' con set_label y set_names
set_label(tmodulo_2021$pref_efec, c(
  "0" = "Blanco",
  "1" = "Solo aceptan efectivo en establecimientos",
  "2" = "Montos bajos",
  "3" = "Permite llevar control gastos",
  "4" = "Desconfianza en débito",
  "5" = "Por costumbre",
  "6" = "Establecimiento cobra comisiones",
  "7" = "Otro"
))

# Etiquetza
attr(tmodulo_2021$pref_efec, "label") <- "Razon principal preferir efectivo"

#_______________________________________________________________________________

# 35) Solo recibe apoyo gobierno
#_______________________________________________________________________________
# Crear la variable cuenta_gob_unica
tmodulo_2021$cuenta_gob_unica <- ifelse(tmodulo_2021$FILTRO_4_SECC5 == 1, 1, 0)

# Etiqueta 
attr(tmodulo_2021$cuenta_gob_unica, "label") <- "Solo tiene cuenta gobierno"

#_______________________________________________________________________________

# 36) Comparacion de productos ahorro
#_______________________________________________________________________________
tmodulo_2021$compara_cuenta <- ifelse(tmodulo_2021$P5_15 %in% c(2), 0, tmodulo_2021$P5_15)

# Etiqueta 
attr(tmodulo_2021$compara_cuenta, "label") <- "Compara productos de ahorro antes de contratar"

#_______________________________________________________________________________

# 37) Medios e comparacion de cuentas
#_______________________________________________________________________________

# renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P5_16_1"] <- "compara_cuenta_ins"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_16_2"] <- "compara_cuenta_ami"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_16_3"] <- "compara_cuenta_con"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_16_4"] <- "compara_cuenta_anu"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_16_5"] <- "compara_cuenta_esp"

#Renombrar variables de la columna
tmodulo_2021$compara_cuenta_ins <- ifelse(tmodulo_2021$compara_cuenta_ins == 1, 1, 0)
tmodulo_2021$compara_cuenta_ami <- ifelse(tmodulo_2021$compara_cuenta_ami == 1, 1, 0)
tmodulo_2021$compara_cuenta_con <- ifelse(tmodulo_2021$compara_cuenta_con == 1, 1, 0)
tmodulo_2021$compara_cuenta_anu <- ifelse(tmodulo_2021$compara_cuenta_anu == 1, 1, 0)
tmodulo_2021$compara_cuenta_esp <- ifelse(tmodulo_2021$compara_cuenta_esp == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo_2021$compara_cuenta_ins, "label") <- "Compara con info dada por banco"
attr(tmodulo_2021$compara_cuenta_ami, "label") <- "Compara con recomendacion amistades"
attr(tmodulo_2021$compara_cuenta_con, "label") <- "Compara con paginas Condusef o Banxico"
attr(tmodulo_2021$compara_cuenta_anu, "label") <- "Compara con anuncios comerciales"
attr(tmodulo_2021$compara_cuenta_esp, "label") <- "Compara con recomendacion de especialistas"

# Crear la variable compara_cuenta_cfo
tmodulo_2021$compara_cuenta_cfo <- NA
tmodulo_2021$compara_cuenta_cfo[tmodulo_2021$compara_cuenta_con == 1 | tmodulo_2021$compara_cuenta_esp == 1] <- 1
tmodulo_2021$compara_cuenta_cfo[tmodulo_2021$compara_cuenta == 1 & is.na(tmodulo_2021$compara_cuenta_cfo)] <- 0

#_______________________________________________________________________________

# 38) Donde contrato cuenta
#_______________________________________________________________________________
# Crear la variable cuenta_contr
tmodulo_2021$cuenta_contr <- tmodulo_2021$P5_17
tmodulo_2021$cuenta_contr[tmodulo_2021$cuenta_contr == 9] <- 0
#_______________________________________________________________________________

# 39) Medios para hacer movimientos en cuentas o checar saldos
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P5_19_1"] <- "cuenta_uso_int"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_19_2"] <- "cuenta_uso_cel"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_19_3"] <- "cuenta_uso_caj"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_19_4"] <- "cuenta_uso_suc"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_19_5"] <- "cuenta_uso_com"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_19_6"] <- "cuenta_uso_tel"
names(tmodulo_2021)[names(tmodulo_2021) == "P5_19_7"] <- "cuenta_uso_otr"

# Renombrar variables de la columna
tmodulo_2021$cuenta_uso_int <- ifelse(tmodulo_2021$cuenta_uso_int == 1, 1, 0)
tmodulo_2021$cuenta_uso_cel <- ifelse(tmodulo_2021$cuenta_uso_cel == 1, 1, 0)
tmodulo_2021$cuenta_uso_caj <- ifelse(tmodulo_2021$cuenta_uso_caj == 1, 1, 0)
tmodulo_2021$cuenta_uso_suc <- ifelse(tmodulo_2021$cuenta_uso_suc == 1, 1, 0)
tmodulo_2021$cuenta_uso_com <- ifelse(tmodulo_2021$cuenta_uso_com == 1, 1, 0) 
tmodulo_2021$cuenta_uso_tel <- ifelse(tmodulo_2021$cuenta_uso_tel == 1, 1, 0)
tmodulo_2021$cuenta_uso_otr <- ifelse(tmodulo_2021$cuenta_uso_otr == 1, 1, 0)

# Etiquetas 
attr(tmodulo_2021$cuenta_uso_int, "label") <- "Uso por internet"
attr(tmodulo_2021$cuenta_uso_cel, "label") <- "Uso por aplicacion celular"
attr(tmodulo_2021$cuenta_uso_caj, "label") <- "Uso por cajero"
attr(tmodulo_2021$cuenta_uso_suc, "label") <- "Uso por sucursal"
attr(tmodulo_2021$cuenta_uso_com, "label") <- "Uso por comercio"
attr(tmodulo_2021$cuenta_uso_tel, "label") <- "Uso por llamada telefonica"
attr(tmodulo_2021$cuenta_uso_otr, "label") <- "Uso por otro medio"
#_______________________________________________________________________________

# 40) Poblacion ex-usuaria de cuentas
#_______________________________________________________________________________
# Poblacion ex-usuaria de cuentas
tmodulo_2021$cuenta_ex <- ifelse(tmodulo_2021$P5_20 %in% c(2), 0, tmodulo_2021$P5_20)

# Etiqueta para la variable 'cuenta_ex'
attr(tmodulo_2021$cuenta_ex, "label") <- "Habia tenido cuenta, tarjeta de banco o de apoyo de gob"
#_______________________________________________________________________________

# 41) Razon para no tener cuenta
#_______________________________________________________________________________
# Crear la variable cuenta_nt_raz
tmodulo_2021$cuenta_nt_raz <- tmodulo_2021$P5_21

# Reemplazar los valores 99 con missing
tmodulo_2021$cuenta_nt_raz[tmodulo_2021$cuenta_nt_raz == 99] <- 0

# Etiquetas para la nueva variable
levels(tmodulo_2021$cuenta_nt_raz) <- c(
  "Sucursal lejana o no hay",
  "Intereses bajos/Comisiones altas",
  "No confia inst fin",
  "Piden requisitos",
  "Prefiere otras formas de ahorrar",
  "No la necesita",
  "Ingresos insuficientes o variables",
  "No sabe como usarla",
  "No quiere cobren impuestos",
  "Otro"
)

# Etiqueta para la variable 'cuenta_nt_raz'
attr(tmodulo_2021$cuenta_nt_raz, "label") <- "Principal razon para no tener cuenta o tarjeta"

#_______________________________________________________________________________

# 42) Razon para dejar de tener cuenta
#_______________________________________________________________________________
# Razon para dejar de tener cuenta
tmodulo_2021$cuenta_ex_raz <- tmodulo_2021$P5_22
tmodulo_2021$cuenta_ex_raz[tmodulo_2021$cuenta_ex_raz == 99 ] <- 0
# Etiquetas para la nueva variable
levels(tmodulo_2021$cuenta_ex_raz) <- c(
  "Dejo de trabajar y le pagaban ahi",
  "Dejo de recibir apoyo gob",
  "No la utilizaba",
  "Mala experiencia inst fin",
  "No cumplia saldo min / cobro comisiones",
  "Intereses pagados muy bajos",
  "Cerro sucursal o inst fin",
  "Fue victima fraude",
  "No quiere cobren impuestos",
  "Otro"
)

# Etiqueta para la variable 'cuenta_ex_raz'
attr(tmodulo_2021$cuenta_ex_raz, "label") <- "Principal razon para dejar de cuenta o tarjeta"


#_______________________________________________________________________________

# 43) Conoce proteccion al ahorro bancario
#_______________________________________________________________________________
names(tmodulo_2021)[names(tmodulo_2021) == "P5_23"] <- "cono_prot"

# Reemplazar los valores 2 con 0
tmodulo_2021$cono_prot[tmodulo_2021$cono_prot == 2] <- 0

# Etiqueta
attr(tmodulo_2021$cono_prot, "label") <- "Conoce que ahorros están protegidos"

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
tmodulo_2021$cred_inf <- ifelse(tmodulo_2021$P6_1_1 == 1 | tmodulo_2021$P6_1_2 == 1 | 
                                  tmodulo_2021$P6_1_3 == 1 | tmodulo_2021$P6_1_4 == 1 | 
                                  tmodulo_2021$P6_1_5 == 1, 1, 0)

# Etiqueta para la variable 'cred_inf'
attr(tmodulo_2021$cred_inf, "label") <- "Tiene crédito informal"
#_______________________________________________________________________________

# 45) Tipo de credito informal
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P6_1_1"] <- "cred_caj"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_1_2"] <- "cred_emp"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_1_3"] <- "cred_ami"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_1_4"] <- "cred_fam"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_1_5"] <- "cred_oti"

#Renombrar variables de la columna
tmodulo_2021$cred_caj <- ifelse(tmodulo_2021$cred_caj == 1, 1, 0)
tmodulo_2021$cred_emp <- ifelse(tmodulo_2021$cred_emp == 1, 1, 0)
tmodulo_2021$cred_ami <- ifelse(tmodulo_2021$cred_ami == 1, 1, 0)
tmodulo_2021$cred_fam <- ifelse(tmodulo_2021$cred_fam == 1, 1, 0)
tmodulo_2021$cred_oti <- ifelse(tmodulo_2021$cred_oti == 1, 1, 0)

# Etiquetas 
attr(tmodulo_2021$cred_caj, "label") <- "Ahorro prestando dinero"
attr(tmodulo_2021$cred_emp, "label") <- "Ahorro comprando animales o bienes"
attr(tmodulo_2021$cred_ami, "label") <- "Ahorro en caja del trabajo o conocidos"
attr(tmodulo_2021$cred_fam, "label") <- "Ahorro guardando con familiares o conocidos"
attr(tmodulo_2021$cred_oti, "label") <- "Ahorro participando en tanda"
#_______________________________________________________________________________

# 46) Crédito formal
#_______________________________________________________________________________
# Credito formal
tmodulo_2021$cred_for <- ifelse(
  tmodulo_2021$P6_2_1 == 1 | tmodulo_2021$P6_2_2 == 1 | tmodulo_2021$P6_2_3 == 1 | 
    tmodulo_2021$P6_2_4 == 1 | tmodulo_2021$P6_2_5 == 1 | tmodulo_2021$P6_2_6 == 1 | 
    tmodulo_2021$P6_2_7 == 1 | tmodulo_2021$P6_2_8 == 1 | tmodulo_2021$P6_2_9 == 1, 1, 0)

# Credito bancario
tmodulo_2021$cred_ban <- ifelse(
  tmodulo_2021$P6_2_2 == 1 | tmodulo_2021$P6_2_3 == 1 | 
    tmodulo_2021$P6_2_4 == 1 | tmodulo_2021$P6_2_5 == 1 | tmodulo_2021$P6_2_6 == 1 | 
    tmodulo_2021$P6_2_7 == 1 | tmodulo_2021$P6_2_8 == 1 | tmodulo_2021$P6_2_9 == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo_2021$cred_for, "label") <- "Tiene crédito formal"
attr(tmodulo_2021$cred_ban, "label") <- "Tiene crédito bancario"
#_______________________________________________________________________________

# 47) Tipo de credito formal
#_______________________________________________________________________________
# renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_1"] <- "tarj_dep"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_2"] <- "tarj_cred"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_3"] <- "cred_nom"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_4"] <- "cred_per"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_5"] <- "cred_aut"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_6"] <- "cred_viv"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_7"] <- "cred_gru"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_8"] <- "cred_int"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_2_9"] <- "cred_otf"

#Renombrar variables de la columna
tmodulo_2021$tarj_dep <- ifelse(tmodulo_2021$tarj_dep == 1, 1, 0)
tmodulo_2021$tarj_cred <- ifelse(tmodulo_2021$tarj_cred == 1, 1, 0)
tmodulo_2021$cred_nom <- ifelse(tmodulo_2021$cred_nom == 1, 1, 0)
tmodulo_2021$cred_per <- ifelse(tmodulo_2021$cred_per == 1, 1, 0)
tmodulo_2021$cred_aut <- ifelse(tmodulo_2021$cred_aut == 1, 1, 0)
tmodulo_2021$cred_viv <- ifelse(tmodulo_2021$cred_viv == 1, 1, 0)
tmodulo_2021$cred_gru <- ifelse(tmodulo_2021$cred_gru == 1, 1, 0)
tmodulo_2021$cred_int <- ifelse(tmodulo_2021$cred_int == 1, 1, 0)
tmodulo_2021$cred_otf <- ifelse(tmodulo_2021$cred_otf == 1, 1, 0)

# Etiquetas para las nuevas variables
attr(tmodulo_2021$tarj_dep, "label") <- "Tiene tarjeta departamental"
attr(tmodulo_2021$tarj_cred, "label") <- "Tiene tarjeta crédito bancaria"
attr(tmodulo_2021$cred_nom, "label") <- "Tiene crédito nómina"
attr(tmodulo_2021$cred_per, "label") <- "Tiene crédito personal"
attr(tmodulo_2021$cred_aut, "label") <- "Tiene crédito automotriz"
attr(tmodulo_2021$cred_viv, "label") <- "Tiene crédito vivienda"
attr(tmodulo_2021$cred_gru, "label") <- "Tiene crédito grupal"
attr(tmodulo_2021$cred_int, "label") <- "Tiene crédito contratado por internet"
attr(tmodulo_2021$cred_otf, "label") <- "Tiene otro crédito formal"
#_______________________________________________________________________________

# 48) Numero de creditos por tipo
#_______________________________________________________________________________
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_1"] <- "tarj_dep_n"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_2"] <- "tarj_cred_n"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_3"] <- "cred_nom_n"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_4"] <- "cred_per_n"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_5"] <- "cred_aut_n"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_6"] <- "cred_viv_n"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_7"] <- "cred_gru_n"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_8"] <- "cred_int_n"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_3_9"] <- "cred_otf_n"


# Etiquetas para el número de tarjetas y créditos
attr(tmodulo_2021$tarj_dep_n, "label") <- "No. tarjetas departamental"
attr(tmodulo_2021$tarj_cred_n, "label") <- "No. tarjetas crédito bancaria"
attr(tmodulo_2021$cred_nom_n, "label") <- "No. créditos nómina"
attr(tmodulo_2021$cred_per_n, "label") <- "No. créditos personales"
attr(tmodulo_2021$cred_aut_n, "label") <- "No. créditos automotrices"
attr(tmodulo_2021$cred_viv_n, "label") <- "No. créditos vivienda"
attr(tmodulo_2021$cred_gru_n, "label") <- "No. créditos grupales"
attr(tmodulo_2021$cred_int_n, "label") <- "No. créditos contratados por internet"
attr(tmodulo_2021$cred_otf_n, "label") <- "No. otros créditos formales"
#_______________________________________________________________________________

# 49) Atrasos en creditos
#_______________________________________________________________________________
# Atrasos en créditos
tmodulo_2021$cred_atr <- ifelse(
  tmodulo_2021$P6_4_1 == 1 | tmodulo_2021$P6_4_2 == 1 | tmodulo_2021$P6_4_3 == 1 | 
    tmodulo_2021$P6_4_4 == 1 | tmodulo_2021$P6_4_5 == 1 | tmodulo_2021$P6_4_6 == 1 | 
    tmodulo_2021$P6_4_7 == 1 | tmodulo_2021$P6_4_8 == 1 | tmodulo_2021$P6_4_9 == 1, 1, 0)

# Etiqueta 
attr(tmodulo_2021$cred_atr, "label") <- "Se ha atrasado con pagos crédito formal"
#_______________________________________________________________________________

# 50) Atrasos por tipo de credito 
#_______________________________________________________________________________
#Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_1"] <- "tarj_dep_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_2"] <- "tarj_cred_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_3"] <- "cred_nom_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_4"] <- "cred_per_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_5"] <- "cred_aut_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_6"] <- "cred_viv_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_7"] <- "cred_gru_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_8"] <- "cred_int_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4_9"] <- "cred_otf_atr"

#Renombrar variables de la columna
tmodulo_2021$tarj_dep_atr <- ifelse(tmodulo_2021$tarj_dep_atr == 1, 1, 0)
tmodulo_2021$tarj_cred_atr <- ifelse(tmodulo_2021$tarj_cred_atr == 1, 1, 0)
tmodulo_2021$cred_nom_atr <- ifelse(tmodulo_2021$cred_nom_atr == 1, 1, 0)
tmodulo_2021$cred_per_atr <- ifelse(tmodulo_2021$cred_per_atr == 1, 1, 0)
tmodulo_2021$cred_aut_atr <- ifelse(tmodulo_2021$cred_aut_atr == 1, 1, 0)
tmodulo_2021$cred_viv_atr <- ifelse(tmodulo_2021$cred_viv_atr == 1, 1, 0)
tmodulo_2021$cred_gru_atr <- ifelse(tmodulo_2021$cred_gru_atr == 1, 1, 0)
tmodulo_2021$cred_int_atr <- ifelse(tmodulo_2021$cred_int_atr == 1, 1, 0)
tmodulo_2021$cred_otf_atr <- ifelse(tmodulo_2021$cred_otf_atr == 1, 1, 0)

# Etiquetas para atrasos en créditos
attr(tmodulo_2021$tarj_dep_atr, "label") <- "Atraso en tarjetas departamental"
attr(tmodulo_2021$tarj_cred_atr, "label") <- "Atraso en tarjetas crédito bancaria"
attr(tmodulo_2021$cred_nom_atr, "label") <- "Atraso en créditos nómina"
attr(tmodulo_2021$cred_per_atr, "label") <- "Atraso en créditos personales"
attr(tmodulo_2021$cred_aut_atr, "label") <- "Atraso en créditos automotrices"
attr(tmodulo_2021$cred_viv_atr, "label") <- "Atraso en créditos vivienda"
attr(tmodulo_2021$cred_gru_atr, "label") <- "Atraso en créditos grupales"
attr(tmodulo_2021$cred_int_atr, "label") <- "Atraso en créditos contratados por internet"
attr(tmodulo_2021$cred_otf_atr, "label") <- "Atraso en otros créditos formales"

#_______________________________________________________________________________

# 51) Tiempo de atraso
#_______________________________________________________________________________
#Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_1"] <- "tarj_dep_t"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_2"] <- "tarj_cred_t"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_3"] <- "cred_nom_t"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_4"] <- "cred_per_t"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_5"] <- "cred_aut_t"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_6"] <- "cred_viv_t"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_7"] <- "cred_gru_t"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_8"] <- "cred_int_t"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_4A_9"] <- "cred_otf_t"

# Etiquetas para las nuevas variables
attr(tmodulo_2021$tarj_dep_t, "label") <- "Veces atraso en tarjetas departamental"
attr(tmodulo_2021$tarj_cred_t, "label") <- "Veces atraso en tarjetas crédito bancaria"
attr(tmodulo_2021$cred_nom_t, "label") <- "Veces atraso en créditos nómina"
attr(tmodulo_2021$cred_per_t, "label") <- "Veces atraso en créditos personales"
attr(tmodulo_2021$cred_aut_t, "label") <- "Veces atraso en créditos automotrices"
attr(tmodulo_2021$cred_viv_t, "label") <- "Veces atraso en créditos vivienda"
attr(tmodulo_2021$cred_gru_t, "label") <- "Veces atraso en créditos grupales"
attr(tmodulo_2021$cred_int_t, "label") <- "Veces atraso en créditos contratados por internet"
attr(tmodulo_2021$cred_otf_t, "label") <- "Veces atraso en otros créditos formales"
#_______________________________________________________________________________

# 52) Tiene algun tipo de credito
#_______________________________________________________________________________
# Generar la variable credito
tmodulo_2021$credito <- ifelse(tmodulo_2021$cred_for == 1 | tmodulo_2021$cred_inf == 1, 1, 0)

#Etiqueta
attr(tmodulo_2021$credito, "label") <- "Tiene crédito formal o informal"

#_______________________________________________________________________________

# 53) Utilizar el CAT para comparar productos
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo_2021$P6_5 <- ifelse(tmodulo_2021$P6_5 == 1, 1, 0)

# Crear la variable compara_cat
tmodulo_2021$compara_cat <- ifelse(tmodulo_2021$P6_5 == 2, 0, tmodulo_2021$P6_5)

# Etiquetas para la variable
attr(tmodulo_2021$compara_cat, "label") <- "Utiliza CAT para contratar crédito"
#_______________________________________________________________________________

# 54) Sobreendeudamiento
#_______________________________________________________________________________
# Renombrar la variable
names(tmodulo_2021)[names(tmodulo_2021) == "P6_6"] <- "sobreend"

# Etiquetas para la nueva variable
levels(tmodulo_2021$sobreend) <- c(
  "Puede pagar deudas a tiempo",
  "No puede pagar deudas a tiempo",
  "Puede pagar algunas deudas a tiempo",
  "No tiene deudas",
  "No sabe"
)

#Ver variables 
var_sobreend <- table(tmodulo_2021$sobreend)
print(var_sobreend)
#_______________________________________________________________________________

# 55) Donde contrato credito
#_______________________________________________________________________________
tmodulo_2021$P6_7 <- ifelse(tmodulo_2021$P6_7 %in% c(9), 0, tmodulo_2021$P6_7)

# Generar la variable cred_contr
tmodulo_2021$cred_contr <- tmodulo_2021$P6_7

#Etiqueta
attr(tmodulo_2021$cred_contr, "label") <- "Medio de contratación de crédito"
#_______________________________________________________________________________

# 56) Frecuencia de uso de tarjeta departamental o credito
#_______________________________________________________________________________
#Ver variables de P6_9
var_P6_9 <- table(tmodulo_2021$P6_9)
print(var_P6_9)

# Crear la variable 
tmodulo_2021$tarj_cred_frec <- tmodulo_2021$P6_9 

#Etiqueta
attr(tmodulo_2021$tarj_cred_frec, "label") <- "Frecuencia uso mensual de tarjeta departamental o crédito"

#Ver variables de tarj_cred_frec
var_tarj_cred_frec <- table(tmodulo_2021$tarj_cred_frec)
print(var_tarj_cred_frec)

#condiciones para crear la nueva columna
tmodulo_2021$tarj_cred_frecg <- 0
tmodulo_2021$tarj_cred_frecg[tmodulo_2021$tarj_cred_frec == 0] <- 1
tmodulo_2021$tarj_cred_frecg[tmodulo_2021$tarj_cred_frec >= 1 & tmodulo_2021$tarj_cred_frec < 10 | tmodulo_2021$tarj_cred_frec == 88] <- 2
tmodulo_2021$tarj_cred_frecg[tmodulo_2021$tarj_cred_frec >= 10 & tmodulo_2021$tarj_cred_frec != 88 & !is.na(tmodulo_2021$tarj_cred_frec)] <- 3

#Etiqueta
attr(tmodulo_2021$tarj_cred_frecg, "label") <- "Frecuencia uso mensual de tarjeta crédito"
#_______________________________________________________________________________

# 57) Razones de no uso de tarjetas de credito o departamentales
#_______________________________________________________________________________
# Razones de no uso de tarjetas de crédito o departamentales
tmodulo_2021$tarj_cred_raznu <- tmodulo_2021$P6_10

#Etiqueta
attr(tmodulo_2021$tarj_cred_raznu, "label") <- "Razon principal para no usar tarjeta credito"

# Etiquetas para la nueva variable
levels(tmodulo_2021$tarj_cred_raznu) <- c(
  "Solo para emergencias",
  "Intereses o comisiones altas",
  "Prefiere pagos al contado",
  "No la aceptan en establecimientos",
  "No le gusta endeudarse",
  "Comercio cobra comisión",
  "Otro",
  "No tiene necesidad"
)

#Ver variables 
var_tarj_cred_raznu <- table(tmodulo_2021$tarj_cred_raznu)
print(var_tarj_cred_raznu)

#_______________________________________________________________________________

# 58) Comparacion de productos de credito
#_______________________________________________________________________________
# Comparación de productos de crédito
tmodulo_2021$compara_cred <- tmodulo_2021$P6_11

#Etiqueta
attr(tmodulo_2021$compara_cred, "label") <- "Compara productos de crédito antes de contratar"

#_______________________________________________________________________________

# 59) Medios e comparacion de creditos
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo_2021$P6_12_1 <- ifelse(tmodulo_2021$P6_12_1 == 1, 1, 0)
tmodulo_2021$P6_12_2 <- ifelse(tmodulo_2021$P6_12_2 == 1, 1, 0)
tmodulo_2021$P6_12_3 <- ifelse(tmodulo_2021$P6_12_3 == 1, 1, 0)
tmodulo_2021$P6_12_4 <- ifelse(tmodulo_2021$P6_12_4 == 1, 1, 0)
tmodulo_2021$P6_12_5 <- ifelse(tmodulo_2021$P6_12_5 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P6_12_1"] <- "compara_cred_ins"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_12_2"] <- "compara_cred_ami"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_12_3"] <- "compara_cred_con"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_12_4"] <- "compara_cred_anu"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_12_5"] <- "compara_cred_esp"

# Etiquetas 
attr(tmodulo_2021$compara_cred_ins, "label") <- "Compara con info dada por banco"
attr(tmodulo_2021$compara_cred_ami, "label") <- "Compara con recomendación de amistades"
attr(tmodulo_2021$compara_cred_con, "label") <- "Compara con páginas de Condusef o Banxico"
attr(tmodulo_2021$compara_cred_anu, "label") <- "Compara con anuncios comerciales"
attr(tmodulo_2021$compara_cred_esp, "label") <- "Compara con recomendación de especialistas"

# Variable compara_cred_cfo
tmodulo_2021$compara_cred_cfo <- ifelse(tmodulo_2021$compara_cred_con == 1 | tmodulo_2021$compara_cred_esp == 1, 1, NA)
tmodulo_2021$compara_cred_cfo <- ifelse(tmodulo_2021$compara_cred == 1 & is.na(tmodulo_2021$compara_cred_cfo), 0, tmodulo_2021$compara_cred_cfo)

# Etiqueta 
attr(tmodulo_2021$compara_cred_cfo, "label") <- "Compara con info de Condusef o especialistas"

#_______________________________________________________________________________

# 60) Ha traspado su saldo a otra institucion
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo_2021$P6_13 <- ifelse(tmodulo_2021$P6_13 == 1, 1, 0)

# Recodificar la variable
tmodulo_2021$cred_traspaso <- ifelse(tmodulo_2021$P6_13 %in% c(2), 0, tmodulo_2021$P6_13)

# Etiquetas para la nueva variable
attr(tmodulo_2021$cred_traspaso, "label") <- "Ha traspasado su saldo a otra institución"
#_______________________________________________________________________________

# 61) Poblacion ex-usuaria de creditos
#_______________________________________________________________________________
# Recodificar la variable
tmodulo_2021$cred_ex <- ifelse(tmodulo_2021$P6_14 %in% c(2), 0, tmodulo_2021$P6_14)

# Etiquetas para la nueva variable
attr(tmodulo_2021$cred_ex, "label") <- "Población ex-usuaria de créditos"
#_______________________________________________________________________________

# 62) Razon para nunca tener credito formal
#_______________________________________________________________________________
# Crear la variable cred_nt_raz y recodificar
#Razón principal de la persona elegida de no tenencia de un crédito formal
tmodulo_2021$cred_nt_raz <- tmodulo_2021$P6_15

# Etiquetas para la nueva variable
levels(tmodulo_2021$cred_nt_raz) <- c(
  "No cumple requisitos",
  "Sucursal lejana o no hay",
  "Cree que la rechazarán",
  "No confía en la institución financiera",
  "Intereses o comisiones altas",
  "No la necesita o no le interesa",
  "No le gusta endeudarse",
  "No quiere cobro de impuestos",
  "Otro"
)

attr(tmodulo_2021$cred_nt_raz, "label") <- "Principal razón para no tener crédito"
#_______________________________________________________________________________

# 63) Razon para dejar de tener credito
#_______________________________________________________________________________
# Crear la variable cred_nt_raz y recodificar
#Razón principal de la persona elegida por la que dejó de tener tarjeta de crédito o un crédito formal 
tmodulo_2021$cred_nt_raz <- tmodulo_2021$P6_16

# Etiquetas para la nueva variable
levels(tmodulo_2021$cred_nt_raz) <- c(
  "Cobraban intereses altos",
  "No se quiere volver a endeudar",
  "Ya no lo necesite",
  "Mala experiencia inst fin",
  "Ya no cumple con requisitos",
  "Prefiere otros tipos de prestamo",
  "No queria cobro impuestos"
)

attr(tmodulo_2021$cred_nt_raz, "label") <- "Principal razon para dejar de tener credito"

#_______________________________________________________________________________

# 64) Rechazo de creditos
#_______________________________________________________________________________
#Crear columnas
#Alguna vez a la persona elegida, le han rechazado una solicitud de crédito formal (P6_17)
tmodulo_2021$cred_rech <- ifelse(tmodulo_2021$P6_17 == 1, 1, 0)
tmodulo_2021$cred_nunca <- ifelse(tmodulo_2021$P6_17 == 3, 1, 0)

#Etiqueta
attr(tmodulo_2021$cred_rech, "label") <- "Le han rechazado un crédito"
attr(tmodulo_2021$cred_nunca, "label") <- "Nunca ha solicitado crédito"
#_______________________________________________________________________________

# 65)Razones de rechazo de crédito
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo_2021$P6_18_1 <- ifelse(tmodulo_2021$P6_18_1 == 1, 1, 0)
tmodulo_2021$P6_18_2 <- ifelse(tmodulo_2021$P6_18_2 == 1, 1, 0)
tmodulo_2021$P6_18_3 <- ifelse(tmodulo_2021$P6_18_3 == 1, 1, 0)
tmodulo_2021$P6_18_4 <- ifelse(tmodulo_2021$P6_18_4 == 1, 1, 0)
tmodulo_2021$P6_18_5 <- ifelse(tmodulo_2021$P6_18_5 == 1, 1, 0)
tmodulo_2021$P6_18_6 <- ifelse(tmodulo_2021$P6_18_6 == 1, 1, 0)
tmodulo_2021$P6_18_9 <- ifelse(tmodulo_2021$P6_18_9 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P6_18_1"] <- "cred_rech_bur"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_18_2"] <- "cred_rech_ing"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_18_3"] <- "cred_rech_doc"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_18_4"] <- "cred_rech_his"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_18_5"] <- "cred_rech_gar"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_18_6"] <- "cred_rech_otr"
names(tmodulo_2021)[names(tmodulo_2021) == "P6_18_9"] <- "cred_rech_ns"

# Etiquetas 
attr(tmodulo_2021$cred_rech_bur, "label") <- "Rechazaron por problemas con buro"
attr(tmodulo_2021$cred_rech_ing, "label") <- "Rechazaron por ingresos insuficientes o no comprobables"
attr(tmodulo_2021$cred_rech_doc, "label") <- "Rechazaron por falta de documentos"
attr(tmodulo_2021$cred_rech_his, "label") <- "Rechazaron por falta de historial crediticio"
attr(tmodulo_2021$cred_rech_gar, "label") <- "Rechazaron por falta de garantia o fiador"
attr(tmodulo_2021$cred_rech_otr, "label") <- "Otra razon de rechazo"
attr(tmodulo_2021$cred_rech_ns, "label") <- "No sabe razon rechazo"

# Crear la variable cred_rech_burhis
tmodulo_2021$cred_rech_burhis <- ifelse(tmodulo_2021$cred_rech_bur == 1 | tmodulo_2021$cred_rech_his == 1, 1, 0)
tmodulo_2021$cred_rech_burhis[tmodulo_2021$cred_rech == 1 & tmodulo_2021$cred_rech_burhis == 0] <- NA

# Etiquetar la variable
attr(tmodulo_2021$cred_rech_burhis, "label") <- "Rechazaron por problemas con buro o falta de historial"

#####################################################################################

# PAGOS

#####################################################################################
#_______________________________________________________________________________

# 66) Forma de pago mas frecuente
#_______________________________________________________________________________
# Etiquetas para la nueva variable
levels(tmodulo_2021$P7_1_1) <- c(
  "Transferencia electrónica",
  "Cargo automático",
  "Tarjeta débito",
  "Tarjeta crédito",
  "Cheques",
  "Tarjeta prepagada",
  "Efectivo",
  "Otro"
)

levels(tmodulo_2021$P7_1_2) <- c(
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
tmodulo_2021$P7_1_1[tmodulo_2021$P7_1_1 == 9] <- NA
tmodulo_2021$P7_1_2[tmodulo_2021$P7_1_2 == 9] <- NA

# Creación de las nuevas variables
tmodulo_2021$pago_menos500 <- tmodulo_2021$P7_1_1
tmodulo_2021$pago_mas500 <- tmodulo_2021$P7_1_2

# Etiquetas 
attr(tmodulo_2021$pago_menos500, "label") <- "Medio de pago más frecuente, <$500"
attr(tmodulo_2021$pago_mas500, "label") <- "Medio de pago más frecuente, >$500"

#Ver variables de P6_9
var_pago_menos500 <- table(tmodulo_2021$pago_menos500)
print(var_pago_menos500) 

var_pago_mas500 <- table(tmodulo_2021$pago_mas500)
print(var_pago_mas500)
#_______________________________________________________________________________

# 67) Conoce CoDi
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo_2021$P7_2 <- ifelse(tmodulo_2021$P7_2 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P7_2"] <- "cono_codi"
tmodulo_2021$cono_codi

#Etiqueta
attr(tmodulo_2021$cono_codi, "label") <- "Conoce o ha escuchado de CoDi"
#_______________________________________________________________________________

# 68) Utilizacion de CoDi
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo_2021$P7_3 <- ifelse(tmodulo_2021$P7_3 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P7_3"] <- "codi"
tmodulo_2021$codi

#Etiqueta
attr(tmodulo_2021$codi, "label") <- "Ha utilizado CoDi"
#_______________________________________________________________________________

# 69) Preferencia de recibir fondos o efectivo
#_______________________________________________________________________________
# Etiquetas para la nueva variable
levels(tmodulo_2021$P7_4) <- c("En cuenta o tarjeta", "En efectivo")

# Creación de la nueva variable
tmodulo_2021$pago_pref <- tmodulo_2021$P7_4
attr(tmodulo_2021$pago_pref, "label") <- "Medio preferido para recibir pagos"
#_______________________________________________________________________________

# 70) Recepcion de remesas
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo_2021$P7_5 <- ifelse(tmodulo_2021$P7_5 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P7_5"] <- "remesas"

attr(tmodulo_2021$remesas, "label") <- "Ha recibido remesas"
tmodulo_2021$remesas
#_______________________________________________________________________________

# 71) Medio de recepcion de remesas
#_______________________________________________________________________________
var_P7_6 <- table(tmodulo_2021$P7_6)
print(var_P7_6)

# Crear la variable remesas_recep con los mismos valores que P7_6
tmodulo_2021$remesas_recep <- tmodulo_2021$P7_6

# Etiquetas para las variables
levels(tmodulo_2021$remesas_recep) <- c("Cuenta o tarjeta", "Efectivo", "Otro")

#Etiqueta
attr(tmodulo_2021$remesas_recep, "label") <- "Medio preferido para recibir remesas"
#_______________________________________________________________________________

# 72) Realizacion de compras
#_______________________________________________________________________________
#Renombrar variables de la columnas
tmodulo_2021$P7_7_1 <- ifelse(tmodulo_2021$P7_7_1 == 1, 1, 0)
tmodulo_2021$P7_7_2 <- ifelse(tmodulo_2021$P7_7_2 == 1, 1, 0)
tmodulo_2021$P7_7_3 <- ifelse(tmodulo_2021$P7_7_3 == 1, 1, 0)
tmodulo_2021$P7_7_4 <- ifelse(tmodulo_2021$P7_7_4 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P7_7_1"] <- "compras_tie"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_7_2"] <- "compras_sup"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_7_3"] <- "compras_ser"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_7_4"] <- "compras_tra"

# Etiquetas
attr(tmodulo_2021$compras_tie, "label") <- "Realizo compras en tienditas o mercados"
attr(tmodulo_2021$compras_sup, "label") <- "Realizo compras en supermercados"
attr(tmodulo_2021$compras_ser, "label") <- "Realizo pagos de servicios (agua, luz, internet)"
attr(tmodulo_2021$compras_tra, "label") <- "Realizo pago de transporte"
#_______________________________________________________________________________

# 73) Metodo de pago por tipo de compra
#_______________________________________________________________________________
#Renombrar variables de la columnas
tmodulo_2021$P7_8_1_1 <- ifelse(tmodulo_2021$P7_8_1_1 == 1, 1, 0)
tmodulo_2021$P7_8_1_2 <- ifelse(tmodulo_2021$P7_8_1_2 == 1, 1, 0)
tmodulo_2021$P7_8_1_3 <- ifelse(tmodulo_2021$P7_8_1_3 == 1, 1, 0)

tmodulo_2021$P7_8_2_1 <- ifelse(tmodulo_2021$P7_8_2_1 == 1, 1, 0)
tmodulo_2021$P7_8_2_2 <- ifelse(tmodulo_2021$P7_8_2_2 == 1, 1, 0)
tmodulo_2021$P7_8_2_3 <- ifelse(tmodulo_2021$P7_8_2_3 == 1, 1, 0)

tmodulo_2021$P7_8_3_1 <- ifelse(tmodulo_2021$P7_8_3_1 == 1, 1, 0)
tmodulo_2021$P7_8_3_2 <- ifelse(tmodulo_2021$P7_8_3_2 == 1, 1, 0)
tmodulo_2021$P7_8_3_3 <- ifelse(tmodulo_2021$P7_8_3_3 == 1, 1, 0)

tmodulo_2021$P7_8_4_1 <- ifelse(tmodulo_2021$P7_8_4_1 == 1, 1, 0)
tmodulo_2021$P7_8_4_2 <- ifelse(tmodulo_2021$P7_8_4_2 == 1, 1, 0)
tmodulo_2021$P7_8_4_3 <- ifelse(tmodulo_2021$P7_8_4_3 == 1, 1, 0)
#-------------------------------------------------------------------------------
# Crear columnas
#-------------------------------------------------------------------------------
tmodulo_2021$compras_tarj <- ifelse(tmodulo_2021$P7_8_1_2 == 1 | tmodulo_2021$P7_8_2_2 == 1 |
                                      tmodulo_2021$P7_8_3_2 == 1 | tmodulo_2021$P7_8_4_1 == 1, 1, 0)

#-------------------------------------------------------------------------------
tmodulo_2021$compras_cel <- ifelse(tmodulo_2021$P7_8_1_1 == 1 | tmodulo_2021$P7_8_2_1 == 1 |
                                     tmodulo_2021$P7_8_3_1 == 1 | tmodulo_2021$P7_8_4_1 == 1, 1, 0)


#-------------------------------------------------------------------------------
tmodulo_2021$compras_efe <- ifelse(tmodulo_2021$P7_8_1_3 == 1 | tmodulo_2021$P7_8_2_3 == 1 |
                                     tmodulo_2021$P7_8_3_3 == 1 | tmodulo_2021$P7_8_4_3 == 1, 1, 0)


#-------------------------------------------------------------------------------
tmodulo_2021$compras_dig <- ifelse(tmodulo_2021$compras_cel == 1 | tmodulo_2021$compras_tarj == 1, 1, 0)

#Etiquetas
attr(tmodulo_2021$compras_tarj, "label") <- "Realiza compras o pagos con tarjeta"
attr(tmodulo_2021$compras_cel, "label") <- "Realiza compras o pagos por transferencia o celular"
attr(tmodulo_2021$compras_efe, "label") <- "Realiza compras o pagos con efectivo"
attr(tmodulo_2021$compras_dig, "label") <- "Realiza compras o pagos con tarjeta, celular o transferencia"
#-------------------------------------------------------------------------------
# Renombrar las variables
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_1_1"] <- "compras_tie_cel"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_1_2"] <- "compras_tie_tarj"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_1_3"] <- "compras_tie_efe"

names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_2_1"] <- "compras_sup_cel"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_2_2"] <- "compras_sup_tarj"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_2_3"] <- "compras_sup_efe"

names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_3_1"] <- "compras_ser_cel"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_3_2"] <- "compras_ser_tarj"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_3_3"] <- "compras_ser_efe"

names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_4_1"] <- "compras_tra_cel"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_4_2"] <- "compras_tra_tarj"
names(tmodulo_2021)[names(tmodulo_2021) == "P7_8_4_3"] <- "compras_tra_efe"
#-------------------------------------------------------------------------------
# Etiquetas para compras en tienditas
attr(tmodulo_2021$compras_tie_cel, "label") <- "Pago compras tienditas con transferencia o celular"
attr(tmodulo_2021$compras_tie_tarj, "label") <- "Pago compras tienditas con tarjeta"
attr(tmodulo_2021$compras_tie_efe, "label") <- "Pago compras tienditas con efectivo"

# Etiquetas para compras en supermercados
attr(tmodulo_2021$compras_sup_cel, "label") <- "Pago compras supermercado con transferencia o celular"
attr(tmodulo_2021$compras_sup_tarj, "label") <- "Pago compras supermercado con tarjeta"
attr(tmodulo_2021$compras_sup_efe, "label") <- "Pago compras supermercado con efectivo"

# Etiquetas para pago de servicios
attr(tmodulo_2021$compras_ser_cel, "label") <- "Pago servicios con transferencia o celular"
attr(tmodulo_2021$compras_ser_tarj, "label") <- "Pago servicios con tarjeta"
attr(tmodulo_2021$compras_ser_efe, "label") <- "Pago servicios con efectivo"

# Etiquetas para pago de transporte
attr(tmodulo_2021$compras_tra_cel, "label") <- "Pago transporte con transferencia o celular"
attr(tmodulo_2021$compras_tra_tarj, "label") <- "Pago transporte con tarjeta"
attr(tmodulo_2021$compras_tra_efe, "label") <- "Pago transporte con efectivo"
#####################################################################################

# SEGUROS

#####################################################################################
#_______________________________________________________________________________

# 74) Tenencia de seguros
#_______________________________________________________________________________
#Renombrar variables de la columnas
tmodulo_2021$P8_1 <- ifelse(tmodulo_2021$P8_1 == 1, 1, 0)
tmodulo_2021$P8_2 <- ifelse(tmodulo_2021$P8_2 == 1, 1, 0)
#-------------------------------------------------------------------------------
tmodulo_2021$seguro <- ifelse(tmodulo_2021$P8_1 ==1 | tmodulo_2021$P8_2 == 1, 1, 0)
tmodulo_2021$seguro_priv <- ifelse(tmodulo_2021$P8_1 %in% c(2, 9), 0, tmodulo_2021$P8_1)
tmodulo_2021$seguro_madre <- ifelse(tmodulo_2021$P8_2 %in% c(2, 9), 0, tmodulo_2021$P8_2)

attr(tmodulo_2021$seguro, "label") <- "Tiene algun seguro"
attr(tmodulo_2021$seguro_priv, "label") <- "Tiene seguro privado"
attr(tmodulo_2021$seguro_madre, "label") <- "Tiene seguro de madres trabajadoras"
#_______________________________________________________________________________

# 75) Ex-usuarios de seguros
#_______________________________________________________________________________
tmodulo_2021$P8_3 <- ifelse(tmodulo_2021$P8_3 %in% c(2), 0, tmodulo_2021$P8_3)
tmodulo_2021$seguro_ex <- ifelse(tmodulo_2021$P8_3 %in% c(2), 0, tmodulo_2021$P8_3)

#Etiqueta
attr(tmodulo_2021$seguro_ex, "label") <- "Ex-usuario de seguro"
#_______________________________________________________________________________

# 76) Razon para no tener seguro
#_______________________________________________________________________________
tmodulo_2021$seguro_nt_raz <- (tmodulo_2021$P8_4)

# Etiquetas para la nueva variable
levels(tmodulo_2021$seguro_nt_raz) <- c(
  "Tiene ahorro para imprevistos",
  "No confía en las aseguradoras",
  "No se lo han ofrecido",
  "No tiene dinero, no tiene trabajo o sus ingresos son variables",
  "No sabe qué son, cómo funcionan o dónde solicitarlos",
  "Son muy caros",
  "No los necesita o no le interesan",
  "Otro"
)

attr(tmodulo_2021$seguro_nt_raz, "label") <- "Principal razon para no tener seguro"

#Ver varibles de la columna
var_seguro_nt_raz <- table(tmodulo_2021$seguro_nt_raz)
print(var_seguro_nt_raz)
#_______________________________________________________________________________

# 77) Razon para dejar de tener seguro
#_____________________________________________________________________________
tmodulo_2021$seguro_ex_raz <- tmodulo_2021$P8_5

# Etiquetas para la nueva variable
levels(tmodulo_2021$seguro_ex_raz) <- c(
  "Dejo de trabajar o cambio de trabajo",
  "Son muy caros",
  "No los utilizaba",
  "Lo tenía por un crédito y ya lo pagó",
  "Tuvo mala experiencia con la aseguradora",
  "Dejó de recibir un programa de gobierno	",
  "Dejó de tener el bien asegurado (auto, casa, etcétera)",
  "Otro"
)

attr(tmodulo_2021$seguro_ex_raz, "label") <- "Principal razon para dejar de seguro"
#_______________________________________________________________________________

# 78) Tipo de seguros
#_______________________________________________________________________________
tmodulo_2021$P8_6_1 <- ifelse(tmodulo_2021$P8_6_1 == 1, 1, 0)
tmodulo_2021$P8_6_2 <- ifelse(tmodulo_2021$P8_6_2 == 1, 1, 0)
tmodulo_2021$P8_6_3 <- ifelse(tmodulo_2021$P8_6_3 == 1, 1, 0)
tmodulo_2021$P8_6_4 <- ifelse(tmodulo_2021$P8_6_4 == 1, 1, 0)
tmodulo_2021$P8_6_5 <- ifelse(tmodulo_2021$P8_6_5 == 1, 1, 0)
tmodulo_2021$P8_6_6 <- ifelse(tmodulo_2021$P8_6_6 == 1, 1, 0)
tmodulo_2021$P8_6_7 <- ifelse(tmodulo_2021$P8_6_7 == 1, 1, 0)
tmodulo_2021$P8_6_8 <- ifelse(tmodulo_2021$P8_6_8 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P8_6_1"] <- "seguro_vid"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_6_2"] <- "seguro_med"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_6_3"] <- "seguro_aut"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_6_4"] <- "seguro_acc"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_6_5"] <- "seguro_cas"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_6_6"] <- "seguro_edu"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_6_7"] <- "seguro_ret"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_6_8"] <- "seguro_otr"

# Etiquetas
attr(tmodulo_2021$seguro_vid, "label") <- "Tiene seguro de vida"
attr(tmodulo_2021$seguro_med, "label") <- "Tiene seguro de gastos medicos mayores"
attr(tmodulo_2021$seguro_au, "label") <- "Tiene seguro de auto"
attr(tmodulo_2021$seguro_acc, "label") <- "Tiene seguro personal contra accidentes"
attr(tmodulo_2021$seguro_cas, "label") <- "Tiene seguro de casa"
attr(tmodulo_2021$seguro_edu, "label") <- "Tiene seguro de educacion"
attr(tmodulo_2021$seguro_ret, "label") <- "Tiene plan privado de retiro"
attr(tmodulo_2021$seguro_otr, "label") <- "Tiene otro tipo de seguro"


# Crear la variable seguro_gen en base a otras variables
tmodulo_2021$seguro_gen <- ifelse(
  tmodulo_2021$seguro_acc == 1 | tmodulo_2021$seguro_cas == 1 | 
    tmodulo_2021$seguro_edu == 1 | tmodulo_2021$seguro_ret == 1 | 
    tmodulo_2021$seguro_otr == 1, 1, 0
)

#Etiqueta
attr(tmodulo_2021$seguro_gen, "label") <- "Tiene otro tipo de seguro (casa, educación, retiro, accidentes)"

#_______________________________________________________________________________

# 79) Tipo de contratacion de seguro
#_______________________________________________________________________________
tmodulo_2021$P8_7_1 <- ifelse(tmodulo_2021$P8_7_1 == 1, 1, 0)
tmodulo_2021$P8_7_2 <- ifelse(tmodulo_2021$P8_7_2 == 1, 1, 0)
tmodulo_2021$P8_7_3 <- ifelse(tmodulo_2021$P8_7_3 == 1, 1, 0)
tmodulo_2021$P8_7_4 <- ifelse(tmodulo_2021$P8_7_4 == 1, 1, 0)
tmodulo_2021$P8_7_5 <- ifelse(tmodulo_2021$P8_7_5 == 1, 1, 0)
tmodulo_2021$P8_7_8 <- ifelse(tmodulo_2021$P8_7_8 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P8_7_1"] <- "seguro_vid_dir"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_7_2"] <- "seguro_med_dir"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_7_3"] <- "seguro_aut_dir"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_7_4"] <- "seguro_acc_dir"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_7_5"] <- "seguro_cas_dir"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_7_8"] <- "seguro_otr_dir"

# Etiquetas
attr(tmodulo_2021$seguro_vid_dir, "label") <- "Contrato directamente seguro de vida"
attr(tmodulo_2021$seguro_med_dir, "label") <- "Contrato directamente seguro de gastos medicos mayores"
attr(tmodulo_2021$seguro_aut_dir, "label") <- "Contrato directamente seguro de auto"
attr(tmodulo_2021$seguro_acc_dir, "label") <- "Contrato directamente seguro personal contra accidentes"
attr(tmodulo_2021$seguro_cas_dir, "label") <- "Contrato directamente seguro de casa"
attr(tmodulo_2021$seguro_otr_dir, "label") <- "Contrato directamente otro tipo de seguro"


# Crear la variable seguro_dir en base a otras variables
tmodulo_2021$seguro_dir <- ifelse(
  tmodulo_2021$seguro_vid_dir == 1 | tmodulo_2021$seguro_med_dir == 1 |
    tmodulo_2021$seguro_aut_dir == 1 | tmodulo_2021$seguro_acc_dir == 1 |
    tmodulo_2021$seguro_cas_dir == 1 | tmodulo_2021$seguro_otr_dir == 1, 1, 0
)

tmodulo_2021$seguro_dir[tmodulo_2021$seguro == 0] <- NA

#Etiqueta
attr(tmodulo_2021$seguro_dir, "label") <- "Contrato directamente su seguro"
#_______________________________________________________________________________

# 80) Satisfaccion con el seguro
#_______________________________________________________________________________
# Asignar valores en la columnaS según las condiciones dadas

tmodulo_2021$P8_8_2 <- ifelse(tmodulo_2021$P8_8_2 == 1, 1, 0)
tmodulo_2021$P8_8_3 <- ifelse(tmodulo_2021$P8_8_3 == 1, 1, 0)
tmodulo_2021$P8_8_4 <- ifelse(tmodulo_2021$P8_8_4 == 1, 1, 0)
tmodulo_2021$P8_8_5 <- ifelse(tmodulo_2021$P8_8_5 == 1, 1, 0)
tmodulo_2021$P8_8_8 <- ifelse(tmodulo_2021$P8_8_8 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P8_8_2"] <- "seguro_med_sat"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_8_3"] <- "seguro_aut_sat"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_8_4"] <- "seguro_acc_sat"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_8_5"] <- "seguro_cas_sat"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_8_8"] <- "seguro_otr_sat"

# Etiquetas 
attr(tmodulo_2021$seguro_med_sat, "label") <- "Satisfecho con seguro de gastos medicos mayores"
attr(tmodulo_2021$seguro_aut_sat, "label") <- "Satisfecho con seguro de auto"
attr(tmodulo_2021$seguro_acc_sat, "label") <- "Satisfecho con seguro personal contra accidentes"
attr(tmodulo_2021$seguro_cas_sat, "label") <- "Satisfecho con seguro de casa"
attr(tmodulo_2021$seguro_otr_sat, "label") <- "Satisfecho con otro tipo de seguro"
#_______________________________________________________________________________

# 81) Uso de seguro
#_______________________________________________________________________________
tmodulo_2021$P8_10_2 <- ifelse(tmodulo_2021$P8_10_2 == 1, 1, 0)
tmodulo_2021$P8_10_3 <- ifelse(tmodulo_2021$P8_10_3 == 1, 1, 0)
tmodulo_2021$P8_10_4 <- ifelse(tmodulo_2021$P8_10_4 == 1, 1, 0)
tmodulo_2021$P8_10_5 <- ifelse(tmodulo_2021$P8_10_5 == 1, 1, 0)
tmodulo_2021$P8_10_8 <- ifelse(tmodulo_2021$P8_10_8 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P8_10_2"] <- "seguro_med_uso"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_10_3"] <- "seguro_aut_uso"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_10_4"] <- "seguro_acc_uso"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_10_5"] <- "seguro_cas_uso"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_10_8"] <- "seguro_otr_uso"

# Etiqueta para laS columnas
attr(tmodulo_2021$seguro_med_uso, "label") <- "Ha reclamado/cobrado seguro de gastos medicos mayores"
attr(tmodulo_2021$seguro_aut_uso, "label") <- "Ha reclamado/cobrado seguro de auto"
attr(tmodulo_2021$seguro_acc_uso, "label") <- "Ha reclamado/cobrado seguro personal contra accidentes"
attr(tmodulo_2021$seguro_cas_uso, "label") <- "Ha reclamado/cobrado seguro de casa"
attr(tmodulo_2021$seguro_otr_uso, "label") <- "Ha reclamado/cobrado otro tipo de seguro"
#_______________________________________________________________________________

# 82) Comparacion de seguros
#_______________________________________________________________________________
#Renombrar variables de la columna
tmodulo_2021$P8_11 <- ifelse(tmodulo_2021$P8_11 == 1, 1, 0)
tmodulo_2021$compara_seguro <- ifelse(tmodulo_2021$P8_11 == 1, 1, 0)

attr(tmodulo_2021$compara_seguro, "label") <- "Compara seguros antes de contratar"
#_______________________________________________________________________________

# 83) Medios e comparacion de seguros
#_______________________________________________________________________________
#Renombrar variables de las columnas
tmodulo_2021$P8_12_1 <- ifelse(tmodulo_2021$P8_12_1 == 1, 1, 0)
tmodulo_2021$P8_12_2 <- ifelse(tmodulo_2021$P8_12_2 == 1, 1, 0)
tmodulo_2021$P8_12_3 <- ifelse(tmodulo_2021$P8_12_3 == 1, 1, 0)
tmodulo_2021$P8_12_4 <- ifelse(tmodulo_2021$P8_12_4 == 1, 1, 0)
tmodulo_2021$P8_12_5 <- ifelse(tmodulo_2021$P8_12_5 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P8_12_1"] <- "compara_seguro_ins"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_12_2"] <- "compara_seguro_ami"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_12_3"] <- "compara_seguro_con"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_12_4"] <- "compara_seguro_anu"
names(tmodulo_2021)[names(tmodulo_2021) == "P8_12_5"] <- "compara_seguro_esp"

# Etiquetas 
attr(tmodulo_2021$compara_seguro_ins, "label") <- "Compara con info dada por institucion"
attr(tmodulo_2021$compara_seguro_ami, "label") <- "Compara con recomendacion amistades"
attr(tmodulo_2021$compara_seguro_con, "label") <- "Compara con paginas Condusef o CNSF"
attr(tmodulo_2021$compara_seguro_anu, "label") <- "Compara con anuncios comerciales"
attr(tmodulo_2021$compara_seguro_esp, "label") <- "Compara con recomendacion de especialistas"

# Crear la variable compara_seguro_cfo
tmodulo_2021$compara_seguro_cfo <- 0

# Asignar valores a compara_seguro_cfo según las condiciones dadas
tmodulo_2021$compara_seguro_cfo <- ifelse(
  tmodulo_2021$compara_seguro_con == 1 | tmodulo_2021$compara_seguro_esp == 1, 1,
  ifelse(tmodulo_2021$compara_seguro == 1 & is.na(tmodulo_2021$compara_seguro_cfo), 0, tmodulo_2021$compara_seguro_cfo)
)

# Etiqueta
attr(tmodulo_2021$compara_seguro_cfo, "label") <- "Compara con info de Condusef o especialistas"

var_compara_seguro_cfo <- table(tmodulo_2021$compara_seguro_cfo)
print(var_compara_seguro_cfo)
#####################################################################################

# AHORRO PARA EL RETIRO

#####################################################################################
#_______________________________________________________________________________

# 84) Tenencia de cuentas de ahorro para el retiro
#_______________________________________________________________________________
tmodulo_2021$afore <- ifelse(tmodulo_2021$P9_1 == 1, 1, 0)

# Etiquetar la variable 
attr(tmodulo_2021$afore, "label") <- "Tiene afore o cuenta de retiro"
#_______________________________________________________________________________

# 85) Ex-usuarios de afores
#_______________________________________________________________________________
tmodulo_2021$P9_1A <- ifelse(tmodulo_2021$P9_1A == 1, 1, 0)
tmodulo_2021$afore_ex <- ifelse(tmodulo_2021$P9_1A == 1, 1, 0)

# Etiquetar la variable 
attr(tmodulo_2021$afore_ex, "label") <- "Ex-usuario de afore o retiro"
#_______________________________________________________________________________

# 86) Razon para no tener afore
#_______________________________________________________________________________
# Crear la variable afore_nt_raz
tmodulo_2021$afore_nt_raz <- tmodulo_2021$P9_2

# Reemplazar los valores con NA si afore_nt_raz es igual a 99
tmodulo_2021$afore_nt_raz[tmodulo_2021$afore_nt_raz == 99] <- NA

attr(tmodulo_2021$afore_nt_raz, "label") <- "Principal razón para no tener afore"

# Etiquetas para la nueva variable
levels(tmodulo_2021$afore_nt_raz) <- c(
  "Blanco",
  "No trabaja/ Nunca ha trabajado",
  "No sabe que es una cuenta para el retiro",
  "No tiene dinero o es insuficiente para ahorrar",
  "No sabe cómo tramitarla",
  "No le interesa o piensa que no le conviene",
  "La Afores le dan desconfianza",
  "Es jubilada, pensionada o tiene sus necesidades resueltas",
  "Trabaja por su cuenta",
  "Otra"
)

var_afore_nt_raz <- table(tmodulo_2021$afore_nt_raz)
print(var_afore_nt_raz)
#_______________________________________________________________________________

# 87) Como contrato afore
#_______________________________________________________________________________
# Crear la variable afore_contr
tmodulo_2021$afore_contr <- tmodulo_2021$P9_3

# Etiquetar la variable 'afore_contr' y asignar atributo
attr(tmodulo_2021$afore_contr, "label") <- "Medio de contratación de afore"

# Etiquetas para la nueva variable
levels(tmodulo_2021$afore_nt_raz) <- c(
  "Con agente o promotor",
  "En sucursal",
  "Aplicacion movil",
  "Portal e-SAR",
  "A traves de empresa donde trabaja"
)

var_afore_nt_raz <- table(tmodulo_2021$afore_nt_raz)
print(var_afore_nt_raz)
#_______________________________________________________________________________

# 88) Aportaciones voluntarias
#_______________________________________________________________________________
tmodulo_2021$P9_4 <- ifelse(tmodulo_2021$P9_4 == 1, 1, 0)
tmodulo_2021$afore_apor <- ifelse(tmodulo_2021$P9_4 == 1, 1, 0)

# Etiqueta
attr(tmodulo_2021$afore_apor, "label") <- "Realiza aportaciones voluntarias a afore"
#_______________________________________________________________________________

# 89) Razon de no hacer aportaciones voluntarias
#_______________________________________________________________________________
# Crear la variable 
tmodulo_2021$afore_apor_nt_raz <- tmodulo_2021$P9_5

# Etiquetar la variable 
attr(tmodulo_2021$afore_apor_nt_raz, "label") <- "Principal razon para no hacer aportaciones a afore"


# Etiquetas para la nueva variable
levels(tmodulo_2021$afore_apor_nt_raz) <- c(
  "Blanco",
  "No le queda dinero para ahorrar",
  "No sabe que es",
  "Ahorra de otra forma",
  "Desconoce las ventajes",
  "No confia en Afores",
  "Porque ya esta jubilada",
  "Otra"
)

var_afore_apor_nt_raz <- table(tmodulo_2021$afore_apor_nt_raz)
print(var_afore_apor_nt_raz)
#_______________________________________________________________________________

# 90) Satisfaccion con afore
#_______________________________________________________________________________
var_P9_6 <- table(tmodulo_2021$P9_6)
print(var_P9_6)
tmodulo_2021$P9_6 <- ifelse(tmodulo_2021$P9_6 == 1, 1, 0)

# Crear la variable 
tmodulo_2021$afore_sat <- tmodulo_2021$P9_6

# Etiquetar la variable 
attr(tmodulo_2021$afore_sat, "label") <- "Satisfecho con atención de Afore"
#_______________________________________________________________________________

# 91) Razon para no estar satisfecho
#_______________________________________________________________________________
# Etiquetar la variable 
attr(tmodulo_2021$P9_7, "label") <- " Razon para no estar satisfecho Afore"

# Etiquetas para la nueva variable
levels(tmodulo_2021$P9_7) <- c(
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

var_P9_7 <- table(tmodulo_2021$P9_7)
print(var_P9_7)
#_______________________________________________________________________________

# 92) Actitudes financieras en vejez
#_______________________________________________________________________________
tmodulo1_2018$P9_9_1 <- ifelse(tmodulo2_2018$p9_9_1 == 1, 1, 0)
tmodulo1_2018$p9_9_2 <- ifelse(tmodulo2_2018$p9_9_2 == 1, 1, 0)
tmodulo1_2018$p9_9_3 <- ifelse(tmodulo2_2018$p9_9_3 == 1, 1, 0)
tmodulo1_2018$p9_9_4 <- ifelse(tmodulo2_2018$p9_9_4 == 1, 1, 0)
tmodulo1_2018$p9_9_5 <- ifelse(tmodulo2_2018$p9_9_5 == 1, 1, 0)

# Renombrar columnas
names(tmodulo1_2018)[names(tmodulo1_2018) == "p9_9_1"] <- "act_vej_gob"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p9_9_2"] <- "act_vej_afore"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p9_9_3"] <- "act_vej_bie"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p9_9_4"] <- "act_vej_din"
names(tmodulo1_2018)[names(tmodulo1_2018) == "p9_9_5"] <- "act_vej_otr"

# Etiqueta para laS columnas
attr(tmodulo1_2018$act_vej_gob, "label") <- "Piensa cubrir gastos vejez con apoyos gob"
attr(tmodulo1_2018$act_vej_afore, "label") <- "Piensa cubrir gastos vejez con ahorro retiro"
attr(tmodulo1_2018$act_vej_bie, "label") <- "Piensa cubrir gastos vejez con venta de bienes"
attr(tmodulo1_2018$act_vej_din, "label") <- "Piensa cubrir gastos vejez con dinero de pareja o fam"
attr(tmodulo1_2018$act_vej_otr, "label") <- "Piensa cubrir gastos vejez de otra forma"
#####################################################################################

# USO DE CANALES FINANCIEROS

#####################################################################################
#_______________________________________________________________________________

# 93) Uso de sucursales
#_______________________________________________________________________________
tmodulo_2021$sucursal <- ifelse(tmodulo_2021$P10_1 == 1, 1, 0)

# Etiquetar la columna 
attr(tmodulo_2021$sucursal, "label") <- "Ha usado sucursal en el último año"
#_______________________________________________________________________________

# 94) Razon para no utilizar sucursal
#_______________________________________________________________________________
tmodulo_2021$P10_2[tmodulo_2021$P10_2 == 100] <- 0

# Crear la variable 
tmodulo_2021$afore_apor_nt_raz <- tmodulo_2021$P10_2

# Etiqueta 
attr(tmodulo_2021$afore_apor_nt_raz, "label") <- "Principal razon para no usar sucursal"

# Etiquetas para la nueva variable
levels(tmodulo_2021$afore_apor_nt_raz) <- c(
  "No tiene cuenta o tarjeta",
  "Ingresos insuficientes",
  "Prefiere otros medios",
  "Esta muy lejos o no hay",
  "Son inseguras o le dan desconfianza",
  "Otra persona hace sus tramites",
  "Mal servicio",
  "Otra",
  "No ha tenido necesidad por pandemia"
)

var_afore_apor_nt_raz <- table(tmodulo_2021$afore_apor_nt_raz)
print(var_afore_apor_nt_raz)
#_______________________________________________________________________________

# 95) Tiempos de traslado a sucursal
#_______________________________________________________________________________
# Recodificar los valores 99 a NA (missing)
tmodulo_2021$sucursal_tras <- ifelse(tmodulo_2021$P10_3_1 %in% c(99), NA, tmodulo_2021$P10_3_1 * 60) + ifelse(tmodulo_2021$P10_3_2 %in% c(99), NA, tmodulo_2021$P10_3_2 * 60)

# Reemplazar 'sucursal_tras' con 'p10_3_1 * 60' si 'p10_3_2' es NA
tmodulo_2021$sucursal_tras <- ifelse(is.na(tmodulo_2021$sucursal_tras) & !is.na(tmodulo_2021$P10_3_2), tmodulo_2021$P10_3_2 * 60, tmodulo_2021$sucursal_tras)

# Reemplazar 'sucursal_tras' con 'p10_3_2' si 'p10_3_1' es NA
tmodulo_2021$sucursal_tras <- ifelse(is.na(tmodulo_2021$sucursal_tras) & !is.na(tmodulo_2021$P10_3_1), tmodulo_2021$P10_3_1 * 60, tmodulo_2021$sucursal_tras)
#_______________________________________________________________________________

# 96) Uso de cajero
#_______________________________________________________________________________
tmodulo_2021$cajero <- ifelse(tmodulo_2021$P10_4 == 1, 1, 0)

# Etiqueta para la columna cajero en tmodulo_2021
attr(tmodulo_2021$cajero, "label") <- "Ha usado cajero en último año"
#_______________________________________________________________________________

# 97) Razon para no utilizar cajero
#_______________________________________________________________________________
# Crear la variable 
tmodulo_2021$cajero_nt_raz <- tmodulo_2021$P10_5

# Etiquetas para la nueva variable
levels(tmodulo_2021$cajero_nt_raz) <- c(
  "Blanco",
  "No tiene cuenta o tarjeta",
  "Ingresos insuficientes",
  "No conoce o no sabe usarlos",
  "Prefiere otros medios",
  "Otra persona hace sus tramites",
  "Son inseguras o le dan desconfianza",
  "Estan muy lejos o no hay",
  "Otra"
)

var_cajero_nt_raz <- table(tmodulo_2021$cajero_nt_raz)
print(var_cajero_nt_raz)

# Etiqueta para la variable cajero_nt_raz en tmodulo_2021
attr(tmodulo_2021$cajero_nt_raz, "label") <- "Principal razón para no usar cajero"

#_______________________________________________________________________________

# 98) Tiempo de traslado al cajero automático que usa regularmente la persona elegida
#_______________________________________________________________________________
# Cambiar los valores 99 y 100 por NA 
tmodulo_2021$P10_6_1[tmodulo_2021$P10_6_1 %in% c(99)] <- NA
tmodulo_2021$P10_6_2[tmodulo_2021$P10_6_2 %in% c(99)] <- NA

# Crear la variable cajero_tras
tmodulo_2021$cajero_tras <- tmodulo_2021$P10_6_1 * 60 + tmodulo_2021$P10_6_2

# Reemplazar los valores con p10_6_1*60 si p10_6_2 es NA
tmodulo_2021$cajero_tras[is.na(tmodulo_2021$P10_6_2)] <- tmodulo_2021$P10_6_1[is.na(tmodulo_2021$P10_6_2)] * 60

# Reemplazar los valores con p10_6_2 si p10_6_1 es NA
tmodulo_2021$cajero_tras[is.na(tmodulo_2021$P10_6_1)] <- tmodulo_2021$P10_6_2[is.na(tmodulo_2021$P10_6_1)]

# Etiquetar la variable 
attr(tmodulo_2021$cajero_tras, "label") <- "Tiempo de traslado cajero, minutos"

#Ver variables de la columna:
var_cajero_tras <- table(tmodulo_2021$cajero_tras)
print(var_cajero_tras)
#_______________________________________________________________________________

# 99) Uso de corresponsal
#_______________________________________________________________________________
tmodulo_2021$corres <- ifelse(tmodulo_2021$P10_7 == 1, 1, 0)

# Etiquetar la columna 
attr(tmodulo_2021$corres, "label") <- "Ha usado corresponsal en último año"
#_______________________________________________________________________________

# 100) Razón principal de la persona elegida por la que no ha utilizado estas tiendas o comercios para realizar operaciones financieras
#_______________________________________________________________________________
# Crear la variable 
tmodulo_2021$corres_nt_raz <- tmodulo_2021$P10_8

# Etiquetas para la nueva variable
levels(tmodulo_2021$corres_nt_raz) <- c(
  "Prefiere usar cajero o sucursal",
  "Otra persona hace sus tramites",
  "Comisiones altas",
  "No sabia que podia hacerlo",
  "Estan muy lejos o no hay",
  "Son inseguros o le dan desconfianza",
  "Lo obligan a realizar compras en tienda",
  "Otra",
  "No ha tenido necesidad por pandemia"
)

# Etiqueta para la variable corres_nt_raz en tmodulo_2021
attr(tmodulo_2021$corres_nt_raz, "label") <- "Principal razón para no usar corresponsal"

var_corres_nt_raz <- table(tmodulo_2021$corres_nt_raz)
print(var_corres_nt_raz)
#_______________________________________________________________________________

# 101) Tiempos de traslado a cajero
#_______________________________________________________________________________
# Cambiar los valores 99 y 100 por NA
tmodulo_2021$P10_9_1[tmodulo_2021$P10_9_1 %in% c(99)] <- NA
tmodulo_2021$P10_9_2[tmodulo_2021$P10_9_2 %in% c(99)] <- NA

###########
var_P10_9_1 <- table(tmodulo_2021$P10_9_1)
print(var_P10_9_1)

var_P10_9_2 <- table(tmodulo_2021$P10_9_2)
print(var_P10_9_2)

##########

# Crear la variable corres_tras
tmodulo_2021$corres_tras <- tmodulo_2021$P10_9_1 * 60 + tmodulo_2021$P10_9_2

# Reemplazar los valores con p10_9_1*60 si p10_9_2 es NA
tmodulo_2021$corres_tras[is.na(tmodulo_2021$P10_9_2)] <- tmodulo_2021$P10_9_1[is.na(tmodulo_2021$P10_9_2)] * 60

# Reemplazar los valores con p10_9_2 si p10_9_1 es NA
tmodulo_2021$corres_tras[is.na(tmodulo_2021$P10_9_1)] <- tmodulo_2021$P10_9_2[is.na(tmodulo_2021$P10_9_1)]

# Etiquetar la variable corres_tras
attr(tmodulo_2021$corres_tras, "label") <- "Tiempo de traslado cajero, minutos"
#####################################################################################

# CONFIANZA Y PROTECCIÓN DE USUARIOS

#####################################################################################
#_______________________________________________________________________________

# 102) Confianza en las instituciones
#_______________________________________________________________________________
tmodulo_2021$P11_1_1 <- ifelse(tmodulo_2021$P11_1_1 == 1, 1, 0)
tmodulo_2021$P11_1_2 <- ifelse(tmodulo_2021$P11_1_2 == 1, 1, 0)
tmodulo_2021$P11_1_3 <- ifelse(tmodulo_2021$P11_1_3 == 1, 1, 0)
tmodulo_2021$P11_1_4 <- ifelse(tmodulo_2021$P11_1_4 == 1, 1, 0)
tmodulo_2021$P11_1_5 <- ifelse(tmodulo_2021$P11_1_5 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P11_1_1"] <- "conf_inf"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_1_2"] <- "conf_pro"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_1_3"] <- "conf_seg"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_1_4"] <- "conf_que"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_1_5"] <- "conf_dat"

# Etiqueta para laS columnas
attr(tmodulo_2021$conf_inf, "label") <- "Confía en que recibiría información necesaria"
attr(tmodulo_2021$conf_pro, "label") <- "Confía en que resolverían su problema económico"
attr(tmodulo_2021$conf_seg, "label") <- "Confía en que su dinero estaría seguro"
attr(tmodulo_2021$conf_que, "label") <- "Confía en que resolverían sus quejas y reclamaciones"
attr(tmodulo_2021$conf_dat, "label") <- "Confía en que protegerían sus datos personales"
#_______________________________________________________________________________

# 103) Tipos de problemas con productos financieros
#_______________________________________________________________________________
# Crear la variable prob_fin y asignarle el valor 0
tmodulo_2021$prob_fin <- 0

# Crear la variable prob_fin con ifelse
tmodulo_2021$prob_fin <- ifelse(tmodulo_2021$P11_2_1 == 1 | tmodulo_2021$P11_2_2 == 1 | tmodulo_2021$P11_2_3 == 1 | tmodulo_2021$P11_2_4 == 1, 1, 0)

# Etiquetar la variable prob_fin
attr(tmodulo_2021$prob_fin, "label") <- "Tuvo algún problema con algún producto financiero"

#-------------------------------------------------------------------------------
tmodulo_2021$P11_2_1 <- ifelse(tmodulo_2021$P11_2_1 == 1, 1, 0)
tmodulo_2021$P11_2_2 <- ifelse(tmodulo_2021$P11_2_2 == 1, 1, 0)
tmodulo_2021$P11_2_3 <- ifelse(tmodulo_2021$P11_2_3 == 1, 1, 0)
tmodulo_2021$P11_2_4 <- ifelse(tmodulo_2021$P11_2_4 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P11_2_1"] <- "prob_clo"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_2_2"] <- "prob_ide"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_2_3"] <- "prob_fra"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_2_4"] <- "prob_din"

# Etiqueta para laS columnas
attr(tmodulo_2021$prob_clo, "label") <- "Ha sufrido de clonación de tarjetas"
attr(tmodulo_2021$prob_ide, "label") <- "Ha sufrido de robo de identidad"
attr(tmodulo_2021$prob_fra, "label") <- "Ha invertido en productos fraudulentos"
attr(tmodulo_2021$prob_din, "label") <- "Le han defraudado con producto o solicitado dinero para premio"
#_______________________________________________________________________________

# 104) Lugar de reclamo en caso de problema
#_______________________________________________________________________________
tmodulo_2021$P11_3_1 <- ifelse(tmodulo_2021$P11_3_1 == 1, 1, 0)
tmodulo_2021$P11_3_2 <- ifelse(tmodulo_2021$P11_3_2 == 1, 1, 0)
tmodulo_2021$P11_3_3 <- ifelse(tmodulo_2021$P11_3_3 == 1, 1, 0)
tmodulo_2021$P11_3_4 <- ifelse(tmodulo_2021$P11_3_4 == 1, 1, 0)
tmodulo_2021$P11_3_9 <- ifelse(tmodulo_2021$P11_3_9 == 1, 1, 0)
#_______________________________________________________________________________

# 105) No sabría donde reclamar
#_______________________________________________________________________________
# Crear la variable prob_q_ns
tmodulo_2021$prob_q_ns <- ifelse(tmodulo_2021$P11_3_1 == 0 & tmodulo_2021$P11_3_2 == 0 & tmodulo_2021$P11_3_3 == 0 & tmodulo_2021$P11_3_4 == 0, 1, 0)

# Reemplazar con N/A si p11_3_4 es 1
tmodulo_2021$prob_q_ns[tmodulo_2021$P11_3_4 == 1] <- NA

# Etiquetar la variable
attr(tmodulo_2021$prob_q_ns, "label") <- "No sabe dónde levantar queja"
#_______________________________________________________________________________

# 106) Sabria donde reclamar
#_______________________________________________________________________________
# Crear la variable prob_q_sd
tmodulo_2021$prob_q_sd <- ifelse(tmodulo_2021$P11_3_1 == 1 | tmodulo_2021$P11_3_2 == 1 | tmodulo_2021$P11_3_3 == 1, 1, 0)

# Etiquetar la variable
attr(tmodulo_2021$prob_q_sd, "label") <- "Sabe dónde levantar queja"
#_______________________________________________________________________________

# 107) Generamos canales de queja que conoce
#_______________________________________________________________________________
# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P11_3_1"] <- "prob_q_ban"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_3_2"] <- "prob_q_condusef"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_3_3"] <- "prob_q_profeco"
names(tmodulo_2021)[names(tmodulo_2021) == "P11_3_4"] <- "prob_q_otr"

# Etiquetas para las columnas
attr(tmodulo_2021$prob_q_ban, "label") <- "En caso de problema, se quejaría en banco"
attr(tmodulo_2021$prob_q_condusef, "label") <- "En caso de problema, se quejaría en Condusef"
attr(tmodulo_2021$prob_q_profeco, "label") <- "En caso de problema, se quejaría en Profeco"
attr(tmodulo_2021$prob_q_otr, "label") <- "En caso de problema, se quejaría en otro"
#_______________________________________________________________________________

# 108) Condicion de reclamo
#_______________________________________________________________________________
tmodulo_2021$P11_4 <- ifelse(tmodulo_2021$P11_4 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P11_4"] <- "prob_fin_rec"

# Etiqueta para la columna 
attr(tmodulo_2021$prob_fin_rec, "label") <- "Ha presentado reclamo a una institución"
#####################################################################################

# AFECTACIONES ECONOMICAS POR COVID

#####################################################################################
#_______________________________________________________________________________

# 109) Sufrio de afectaciones economicas
#_______________________________________________________________________________
tmodulo_2021$P12_1 <- ifelse(tmodulo_2021$P12_1 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P12_1"] <- "covid"

# Etiquetas 
attr(tmodulo_2021$covid, "label") <- "Sufrió afectación económica por COVID"
#_______________________________________________________________________________

# 110) Tipo de afectacion
#_______________________________________________________________________________
# Cambiar variables de las columnas
tmodulo_2021$P12_2_1 <- ifelse(tmodulo_2021$P12_2_1 == 1, 1, 0)
tmodulo_2021$P12_2_2 <- ifelse(tmodulo_2021$P12_2_2 == 1, 1, 0)
tmodulo_2021$P12_2_3 <- ifelse(tmodulo_2021$P12_2_3 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P12_2_1"] <- "covid_af_ing"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_2_2"] <- "covid_af_fun"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_2_3"] <- "covid_af_emp"

# Etiquetas 
attr(tmodulo_2021$covid_af_ing, "label") <- "Tuvo reducción de ingresos por COVID"
attr(tmodulo_2021$covid_af_fun, "label") <- "Tuvo gastos de salud o funerarios por COVID"
attr(tmodulo_2021$covid_af_emp, "label") <- "Perdió empleo o fuente de ingresos por COVID"
#_______________________________________________________________________________

# 111) Modo en que enfrento la emergencia economica
#_______________________________________________________________________________
# Cambiar variables de las columnas
tmodulo_2021$P12_3_1 <- ifelse(tmodulo_2021$P12_3_1 == 1, 1, 0)
tmodulo_2021$P12_3_2 <- ifelse(tmodulo_2021$P12_3_2 == 1, 1, 0)
tmodulo_2021$P12_3_3 <- ifelse(tmodulo_2021$P12_3_3 == 1, 1, 0)
tmodulo_2021$P12_3_4 <- ifelse(tmodulo_2021$P12_3_4 == 1, 1, 0)
tmodulo_2021$P12_3_5 <- ifelse(tmodulo_2021$P12_3_5 == 1, 1, 0)
tmodulo_2021$P12_3_6 <- ifelse(tmodulo_2021$P12_3_6 == 1, 1, 0)
tmodulo_2021$P12_3_7 <- ifelse(tmodulo_2021$P12_3_7 == 1, 1, 0)
tmodulo_2021$P12_3_8 <- ifelse(tmodulo_2021$P12_3_8 == 1, 1, 0)

# Renombrar columnas
names(tmodulo_2021)[names(tmodulo_2021) == "P12_3_1"] <- "covid_me_fam"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_3_2"] <- "covid_me_aho"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_3_3"] <- "covid_me_red"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_3_4"] <- "covid_me_emp"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_3_5"] <- "covid_me_cred"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_3_6"] <- "covid_me_atr"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_3_7"] <- "covid_me_pro"
names(tmodulo_2021)[names(tmodulo_2021) == "P12_3_8"] <- "covid_me_trab"

# Etiquetas para las columnas
attr(tmodulo_2021$covid_me_fam, "label") <- "Enfrento choque COVID con préstamo familiares"
attr(tmodulo_2021$covid_me_aho, "label") <- "Enfrento choque COVID con ahorros"
attr(tmodulo_2021$covid_me_red, "label") <- "Enfrento choque COVID reduciendo gastos"
attr(tmodulo_2021$covid_me_emp, "label") <- "Enfrento choque COVID vendió o empeñó bien"
attr(tmodulo_2021$covid_me_cred, "label") <- "Enfrento choque COVID con crédito o solicitó uno"
attr(tmodulo_2021$covid_me_atr, "label") <- "Se atrasó en el pago de crédito bancario por COVID"
attr(tmodulo_2021$covid_me_pro, "label") <- "Aceptó prórroga de pago de intereses por COVID"
attr(tmodulo_2021$covid_me_trab, "label") <- "Enfrento choque COVID trabajando extra, o solicitando adelanto"
#_______________________________________________________________________________

# 112) Tiempo de recuperacion de la crisis economica COVID
#_______________________________________________________________________________
var_P12_4 <- table(tmodulo_2021$P12_4)
print(var_P12_4)

# Crear la variable 
tmodulo_2021$covid_t <- tmodulo_2021$P12_4

# Etiqueta para la variable 
attr(tmodulo_2021$covid_t, "label") <- "Tiempo que le tomo reponerse de crisis COVID, meses"
#-------------------------------------------------------------------------------
# Crear la variable covid_tg y asignarle un valor predeterminado.
tmodulo_2021$covid_tg <- 0
tmodulo_2021$covid_tg[tmodulo_2021$covid_t == 88] <- 1
tmodulo_2021$covid_tg[tmodulo_2021$covid_t >= 0 & tmodulo_2021$covid_t <= 6] <- 2
tmodulo_2021$covid_tg[tmodulo_2021$covid_t > 6 & tmodulo_2021$covid_t <= 12] <- 3
tmodulo_2021$covid_tg[tmodulo_2021$covid_t > 12 & !is.na(tmodulo_2021$covid_t) & tmodulo_2021$covid_t != 88] <- 4

var_covid_tg <- table(tmodulo_2021$covid_tg)
print(var_covid_tg)

# Etiquetas para la nueva variable
levels(tmodulo_2021$covid_tg) <- c(
  "Blanco",
  "No se ha recuperado",
  "Seis meses o menos",
  "Entre 6 meses y un año",
  "Más de un año"
)
#####################################################################################

# CAPACIDADES FINANCIERAS

#####################################################################################
#_______________________________________________________________________________

# 113) Interes simple
#_______________________________________________________________________________
# Interes simple
tmodulo_2021$cono_ints <- ifelse(tmodulo_2021$P13_1 == 1, 1, 0)

# Etiquetar variable 
attr(tmodulo_2021$cono_ints, "label") <- "Conoce concepto de interés simple"
#_______________________________________________________________________________

# 114) Interes simple con calculo
#_______________________________________________________________________________
# Interes simple con calculo
tmodulo_2021$cono_ints_c <- ifelse(tmodulo_2021$P13_2 == 2, 1, 0)

# Etiquetar variable cono_ints_c
attr(tmodulo_2021$cono_ints_c, "label") <- "Cálculo correcto de interés simple"

#_______________________________________________________________________________

# 115) Interes compuesto
#_______________________________________________________________________________
# Interes compuesto
tmodulo_2021$cono_intc <- ifelse(tmodulo_2021$P13_3 == 1, 1, 0)

# Etiquetar variable cono_intc
attr(tmodulo_2021$cono_intc, "label") <- "Cálculo correcto de interés compuesto"

#_______________________________________________________________________________

# 116) Calculo correcto de inflacion
#_______________________________________________________________________________
# Cálculo correcto de inflación
tmodulo_2021$cono_infc <- ifelse(tmodulo_2021$P13_4 == 3, 1, 0)

# Etiquetar variable 
attr(tmodulo_2021$cono_infc, "label") <- "Cálculo correcto de impacto inflación"

#####################################################################################

# TOMA DE DESICIONES

#####################################################################################
#_______________________________________________________________________________

# 117) Toma de decision
#_______________________________________________________________________________
# Comparación de productos de crédito
tmodulo_2021$deci <- tmodulo_2021$P14_1

# Etiquetas 
levels(tmodulo_2021$deci) <- c(
  "Solo usted",
  "Usted y otra persona",
  "Otra(s) persona"
)

# Etiquetar variable 
attr(tmodulo_2021$deci, "label") <- "Persona que toma decisiones economicas en hogar"

tmodulo_2021$deci_p <- ifelse(tmodulo_2021$deci == 1 | tmodulo_2021$deci == 2, 1, 0)
attr(tmodulo_2021$deci_p, "label") <- "Tiene poder de decisión económico"

#_______________________________________________________________________________

# 118) Propiedad de activos
#_______________________________________________________________________________
# Cambiar variables de las columnas
tmodulo_2021$P14_2_1 <- ifelse(tmodulo_2021$P14_2_1 == 1, 1, 0)
tmodulo_2021$P14_2_2 <- ifelse(tmodulo_2021$P14_2_2 == 1, 1, 0)
tmodulo_2021$P14_2_3 <- ifelse(tmodulo_2021$P14_2_3 == 1, 1, 0)
tmodulo_2021$P14_2_4 <- ifelse(tmodulo_2021$P14_2_4 == 1, 1, 0)

# Renombrar variables
names(tmodulo_2021)[names(tmodulo_2021) == "P14_2_1"] <- "prop_viv"
names(tmodulo_2021)[names(tmodulo_2021) == "P14_2_2"] <- "prop_auto"
names(tmodulo_2021)[names(tmodulo_2021) == "P14_2_3"] <- "prop_ter"
names(tmodulo_2021)[names(tmodulo_2021) == "P14_2_4"] <- "prop_otr"

# Etiquetar variables
attr(tmodulo_2021$prop_viv, "label") <- "Tiene vivienda o departamento"
attr(tmodulo_2021$prop_auto, "label") <- "Tiene automovil, moto"
attr(tmodulo_2021$prop_ter, "label") <- "Tiene terreno o tierra de cultivo"
attr(tmodulo_2021$prop_otr, "label") <- "Tiene otro tipo de propiedad"

#_______________________________________________________________________________

# 119) Forma de adquisicion de bienes
#_______________________________________________________________________________
# Renombrar variables
names(tmodulo_2021)[names(tmodulo_2021) == "P14_3_1"] <- "prop_viv_adq"
names(tmodulo_2021)[names(tmodulo_2021) == "P14_3_2"] <- "prop_auto_adq"
names(tmodulo_2021)[names(tmodulo_2021) == "P14_3_3"] <- "prop_ter_adq"

# Etiquetar variables
attr(tmodulo_2021$prop_viv_adq, "label") <- "Principal forma de adquisición vivienda"
attr(tmodulo_2021$prop_auto_adq, "label") <- "Principal forma de adquisición automóvil"
attr(tmodulo_2021$prop_ter_adq, "label") <- "Principal forma de adquisición terreno"
#_______________________________________________________________________________

# 120) Libertad para diposicion de bienes
#_______________________________________________________________________________
# Renombrar variables
names(tmodulo_2021)[names(tmodulo_2021) == "P14_4_1"] <- "prop_viv_disp"
names(tmodulo_2021)[names(tmodulo_2021) == "P14_4_2"] <- "prop_auto_disp"
names(tmodulo_2021)[names(tmodulo_2021) == "P14_4_3"] <- "prop_ter_disp"

# Etiquetar variables
attr(tmodulo_2021$prop_viv_disp, "label") <- "Libertad de disposición de vivienda"
attr(tmodulo_2021$prop_auto_disp, "label") <- "Libertad de disposición de automóvil"
attr(tmodulo_2021$prop_ter_disp, "label") <- "Libertad de disposición de terreno"

#####################################################################################

# INDICADORES DE USO

#####################################################################################
#_______________________________________________________________________________

# 120) Poblacion con al menos un producto financiero
#_______________________________________________________________________________
# Crear variable
tmodulo_2021$prod_fin <- ifelse(tmodulo_2021$cuenta == 1 | tmodulo_2021$cred_for == 1 | tmodulo_2021$seguro == 1 | tmodulo_2021$afore == 1, 1, 0)

# Etiquetar variable
attr(tmodulo_2021$prod_fin, "label") <- "Cuenta con al menos un producto financiero"

#_______________________________________________________________________________

# 121) usuarios de cuenta
#_______________________________________________________________________________
# Generar variable
tmodulo_2021$cuenta_usuario <- 0

# Actualizar valores
tmodulo_2021$cuenta_usuario[tmodulo_2021$cuenta == 1] <- 1
tmodulo_2021$cuenta_usuario[tmodulo_2021$cuenta_ex == 1] <- 2

# Etiquetar variable
levels(tmodulo_2021$cuenta_usuario) <- c("Nunca ha tenido", "Tiene producto", "Dejo de tener")
attr(tmodulo_2021$cuenta_usuario, "label") <- "Poblacion por tenencia de cuenta"

#_______________________________________________________________________________

# 122) Poblacion por numero de tipos de productos
#_______________________________________________________________________________
# Generar variables
tmodulo_2021$prod_uno <- 0
tmodulo_2021$prod_dos <- 0
tmodulo_2021$prod_tres <- 0
tmodulo_2021$prod_cuatro <- 0
tmodulo_2021$prod_cero <- 0

# Actualizar valores
tmodulo_2021$prod_uno[tmodulo_2021$cuenta == 1 & tmodulo_2021$cred_for == 0 & tmodulo_2021$seguro == 0 & tmodulo_2021$afore == 0 |
                        tmodulo_2021$cuenta == 0 & tmodulo_2021$cred_for == 1 & tmodulo_2021$seguro == 0 & tmodulo_2021$afore == 0 |
                        tmodulo_2021$cuenta == 0 & tmodulo_2021$cred_for == 0 & tmodulo_2021$seguro == 1 & tmodulo_2021$afore == 0 |
                        tmodulo_2021$cuenta == 0 & tmodulo_2021$cred_for == 0 & tmodulo_2021$seguro == 0 & tmodulo_2021$afore == 1] <- 1

tmodulo_2021$prod_dos[tmodulo_2021$cuenta == 1 & tmodulo_2021$cred_for == 1 & tmodulo_2021$seguro == 0 & tmodulo_2021$afore == 0 |
                        tmodulo_2021$cuenta == 1 & tmodulo_2021$cred_for == 0 & tmodulo_2021$seguro == 1 & tmodulo_2021$afore == 0 |
                        tmodulo_2021$cuenta == 1 & tmodulo_2021$cred_for == 0 & tmodulo_2021$seguro == 0 & tmodulo_2021$afore == 1 |
                        tmodulo_2021$cuenta == 0 & tmodulo_2021$cred_for == 1 & tmodulo_2021$seguro == 1 & tmodulo_2021$afore == 0 |
                        tmodulo_2021$cuenta == 0 & tmodulo_2021$cred_for == 1 & tmodulo_2021$seguro == 0 & tmodulo_2021$afore == 1 |
                        tmodulo_2021$cuenta == 0 & tmodulo_2021$cred_for == 0 & tmodulo_2021$seguro == 1 & tmodulo_2021$afore == 1] <- 1

tmodulo_2021$prod_tres[tmodulo_2021$cuenta == 1 & tmodulo_2021$cred_for == 1 & tmodulo_2021$seguro == 1 & tmodulo_2021$afore == 0 |
                         tmodulo_2021$cuenta == 1 & tmodulo_2021$cred_for == 1 & tmodulo_2021$seguro == 0 & tmodulo_2021$afore == 1 |
                         tmodulo_2021$cuenta == 1 & tmodulo_2021$cred_for == 0 & tmodulo_2021$seguro == 1 & tmodulo_2021$afore == 1 |
                         tmodulo_2021$cuenta == 0 & tmodulo_2021$cred_for == 1 & tmodulo_2021$seguro == 1 & tmodulo_2021$afore == 1] <- 1

tmodulo_2021$prod_cuatro[tmodulo_2021$cuenta == 1 & tmodulo_2021$cred_for == 1 & tmodulo_2021$seguro == 1 & tmodulo_2021$afore == 1] <- 1

tmodulo_2021$prod_cero[tmodulo_2021$cuenta == 0 & tmodulo_2021$cred_for == 0 & tmodulo_2021$seguro == 0 & tmodulo_2021$afore == 0] <- 1

# Etiquetar variables
attr(tmodulo_2021$prod_uno, "label") <- "Cuenta con solo un tipo de producto financiero"
attr(tmodulo_2021$prod_dos, "label") <- "Cuenta con dos tipos de productos financieros"
attr(tmodulo_2021$prod_tres, "label") <- "Cuenta con tres tipos de productos financieros"
attr(tmodulo_2021$prod_cuatro, "label") <- "Cuenta con los cuatro tipos de productos financieros"
attr(tmodulo_2021$prod_cero, "label") <- "Cuenta con ningun producto financiero"

levels(tmodulo_2021$prod_uno) <- c("No", "Sí")
levels(tmodulo_2021$prod_dos) <- c("No", "Sí")
levels(tmodulo_2021$prod_tres) <- c("No", "Sí")
levels(tmodulo_2021$prod_cuatro) <- c("No", "Sí")
levels(tmodulo_2021$prod_cero) <- c("No", "Sí")

#_______________________________________________________________________________

# 123) Variables historicas
#_______________________________________________________________________________
# Variables históricas
tmodulo_2021$cuenta_hist <- ifelse(tmodulo_2021$cuenta == 1 | tmodulo_2021$cuenta_ex == 1, 1, 0)
tmodulo_2021$seguro_hist <- ifelse(tmodulo_2021$seguro == 1 | tmodulo_2021$seguro_ex == 1, 1, 0)
tmodulo_2021$cred_hist <- ifelse(tmodulo_2021$cred_for == 1 | tmodulo_2021$cred_ex == 1, 1, 0)
tmodulo_2021$afore_hist <- ifelse(tmodulo_2021$afore == 1 | tmodulo_2021$afore_ex == 1, 1, 0)
tmodulo_2021$prod_fin_hist <- ifelse(tmodulo_2021$cuenta_hist == 1 | tmodulo_2021$cred_hist == 1 | tmodulo_2021$afore == 1 | tmodulo_2021$seguro_hist == 1, 1, 0)

# Etiquetar variables
attr(tmodulo_2021$cuenta_hist, "label") <- "Tiene o ha tenido cuenta"
attr(tmodulo_2021$seguro_hist, "label") <- "Tiene o ha tenido seguro"
attr(tmodulo_2021$cred_hist, "label") <- "Tiene o ha tenido crédito formal"
attr(tmodulo_2021$afore_hist, "label") <- "Tiene o ha tenido cuenta para retiro o afore"
attr(tmodulo_2021$prod_fin_hist, "label") <- "Tiene o ha tenido un producto financiero"

# Etiquetar variables
levels(tmodulo_2021$cuenta_hist) <- c("No", "Sí")
levels(tmodulo_2021$seguro_hist) <- c("No", "Sí")
levels(tmodulo_2021$cred_hist) <- c("No", "Sí")
levels(tmodulo_2021$afore_hist) <- c("No", "Sí")
levels(tmodulo_2021$prod_fin_hist) <- c("No", "Sí")

#_______________________________________________________________________________

# 124) variable de usuario
#_______________________________________________________________________________
# Generar variable de usuario
tmodulo_2021$usuario <- 0
tmodulo_2021$usuario[tmodulo_2021$prod_fin == 1] <- 1
tmodulo_2021$usuario[tmodulo_2021$prod_fin_hist == 1 & tmodulo_2021$prod_fin == 0] <- 2

# Etiquetar la variable
attr(tmodulo_2021$usuario, "label") <- "Población por tenencia de producto financiero"
levels(tmodulo_2021$usuario) <- c("Nunca ha tenido", "Tiene producto", "Dejó de tener")

#####################################################################################

# INDICE DE ALFABETIZACIÓN FINANCIERA

#####################################################################################
#_______________________________________________________________________________

# 125) Subindice de conocimientos
#_______________________________________________________________________________
tmodulo_2021$cono_intc2 <- tmodulo_2021$cono_intc
tmodulo_2021$cono_intc2[tmodulo_2021$cono_ints_c == 0] <- 0

tmodulo_2021$ind_cono <- tmodulo_2021$cono_infc +
  tmodulo_2021$cono_ints +
  tmodulo_2021$cono_ints_c +
  tmodulo_2021$cono_intc2 +
  tmodulo_2021$cono_rie +
  tmodulo_2021$cono_inf +
  tmodulo_2021$cono_div

# Etiquetar la variable
attr(tmodulo_2021$ind_cono, "label") <- "Subíndice de conocimientos"

#_______________________________________________________________________________

# 126) Subindice de comportamientos
#_______________________________________________________________________________
# Subíndice de comportamientos
tmodulo_2021$deci2 <- tmodulo_2021$deci %in% c(1, 2)
tmodulo_2021$comp2 <- tmodulo_2021$presup + tmodulo_2021$contr_anogas + tmodulo_2021$contr_aplcel +
  tmodulo_2021$contr_cobaut + tmodulo_2021$contr_regdeu + tmodulo_2021$contr_sepdin
tmodulo_2021$presup2 <- tmodulo_2021$deci2 & tmodulo_2021$comp2 >= 2

tmodulo_2021$sobregas2 <- tmodulo_2021$sobregas == 0 | tmodulo_2021$sobregas_aho == 1 | 
  tmodulo_2021$sobregas_rga == 1 | tmodulo_2021$sobregas_bie == 1 | 
  tmodulo_2021$sobregas_ade == 1

tmodulo_2021$compara2 <- tmodulo_2021$compara_cuenta == 1 | tmodulo_2021$compara_cred == 1 | 
  tmodulo_2021$compara_seguro == 1

tmodulo_2021$comparaprod2 <- (tmodulo_2021$compara_cuenta_esp == 1 | tmodulo_2021$compara_cuenta_con == 1) | 
  (tmodulo_2021$compara_cred_esp == 1 | tmodulo_2021$compara_cred_con == 1) | 
  (tmodulo_2021$compara_seguro_esp == 1 | tmodulo_2021$compara_seguro_con == 1)

tmodulo_2021$comp_rdd2 <- tmodulo_2021$comp_rdd == 2
tmodulo_2021$comp_mlp2 <- tmodulo_2021$comp_mlp == 2
tmodulo_2021$comp_ant2 <- tmodulo_2021$comp_ant == 2
tmodulo_2021$comp_pat2 <- tmodulo_2021$comp_pat == 2

tmodulo_2021$ind_comp <- tmodulo_2021$presup2 + tmodulo_2021$ahorro + tmodulo_2021$sobregas2 + 
  tmodulo_2021$compara2 + tmodulo_2021$comparaprod2 + tmodulo_2021$comp_rdd2 + 
  tmodulo_2021$comp_mlp2 + tmodulo_2021$comp_ant2 + tmodulo_2021$comp_pat2

# Etiquetar la variable
attr(tmodulo_2021$ind_comp, "label") <- "Subíndice de comportamientos"


#_______________________________________________________________________________

# 126) Subindice de actitudes
#_______________________________________________________________________________
# SAF_01
tmodulo_2021$comp_pre2 <- 0
tmodulo_2021$comp_pre2[tmodulo_2021$comp_pre == 0] <- 5
tmodulo_2021$comp_pre2[tmodulo_2021$comp_pre == 1] <- 3
tmodulo_2021$comp_pre2[tmodulo_2021$comp_pre == 2] <- 1

# SAF_02
tmodulo_2021$comp_pga2 <- 0
tmodulo_2021$comp_pga2[tmodulo_2021$comp_pga == 0] <- 5
tmodulo_2021$comp_pga2[tmodulo_2021$comp_pga == 1] <- 3
tmodulo_2021$comp_pga2[tmodulo_2021$comp_pga == 2] <- 1

# SFA_03
tmodulo_2021$comp_dpg2 <- 0
tmodulo_2021$comp_dpg2[tmodulo_2021$comp_dpg == 0] <- 5
tmodulo_2021$comp_dpg2[tmodulo_2021$comp_dpg == 1] <- 3
tmodulo_2021$comp_dpg2[tmodulo_2021$comp_dpg == 2] <- 1

# Subíndice de actitudes
tmodulo_2021$ind_act <- (tmodulo_2021$comp_pre2 + tmodulo_2021$comp_pga2 + tmodulo_2021$comp_dpg2) / 3

# Etiquetar la variable
attr(tmodulo_2021$ind_act, "label") <- "Subíndice de actitudes"

#_______________________________________________________________________________

# 127) Indice de alfabetizacion financiera
#_______________________________________________________________________________
# Indice de alfabetizacion financiera
tmodulo_2021$ind_fin <- tmodulo_2021$ind_comp + tmodulo_2021$ind_act + tmodulo_2021$ind_cono

# Etiquetar la variable
attr(tmodulo_2021$ind_fin, "label") <- "Índice de alfabetización financiera"

#_______________________________________________________________________________

# 127) Estandariamos los indices
#_______________________________________________________________________________
# Estandarizar los indices
tmodulo_2021$ind_cono <- (tmodulo_2021$ind_cono * 100) / 7
tmodulo_2021$ind_comp <- (tmodulo_2021$ind_comp * 100) / 9
tmodulo_2021$ind_act <- (tmodulo_2021$ind_act * 100) / 5
tmodulo_2021$ind_fin <- (tmodulo_2021$ind_fin * 100) / 21


#####################################################################################

# PRUEBA DE INDICE CON ANALISIS DE CORRESPONDENCIA

#####################################################################################
# Subindice de conocimientos
tmodulo_2021$ind_cono_mca <- scale(tmodulo_2021$cono_infc + tmodulo_2021$cono_ints + tmodulo_2021$cono_ints_c + tmodulo_2021$cono_intc2 + tmodulo_2021$cono_rie + tmodulo_2021$cono_inf + tmodulo_2021$cono_div)

# Subindice de comportamientos
tmodulo_2021$ind_comp_mca <- scale(tmodulo_2021$presup2 + tmodulo_2021$ahorro + tmodulo_2021$sobregas2 + tmodulo_2021$compara2 + tmodulo_2021$comparaprod2 + tmodulo_2021$comp_rdd2 + tmodulo_2021$comp_mlp2 + tmodulo_2021$comp_ant2 + tmodulo_2021$comp_pat2)

# Subindice de actitudes
tmodulo_2021$ind_act_mca <- scale(tmodulo_2021$comp_pre2 + tmodulo_2021$comp_pga2 + tmodulo_2021$comp_dpg2)

# Indice de alfabetizacion por pca
tmodulo_2021$ind_fin_pca <- scale(tmodulo_2021$ind_cono_mca + tmodulo_2021$ind_comp_mca + tmodulo_2021$ind_act_mca)

#####################################################################################
write.csv(tmodulo_2021, file = "tmodulo_2021_clean.csv", row.names = FALSE)
