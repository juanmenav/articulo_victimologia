library(pdftools)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(here)

setwd(here())

datos <- read_xlsx("datos/procesados/pdls_seguridad_metadata_revisados.xlsx") %>% subset(Revision ==1)

leyes <- datos$num_ley

leyes <- as.character(leyes)

source("scripts/procesamiento de texto/segmentar_intervenciones.R")

procesamiento <- function(txt){
  
  texto_pdf <- pdf_text(paste0("datos/raw/historias de la ley/",txt, ".pdf"))
  
  txt_sin_paginas <- paste(texto_pdf, collapse = "\n")
  
  texto_limpio <- limpiar_texto(txt_sin_paginas)

  unidades_analisis <- segmentar_intervenciones(texto_limpio)
  
  unidades_analisis <- unidades_analisis %>% mutate(discurso = case_when(grepl("(Presidente)", rol) ~ 0,
                                                                         grepl("(Presidente accidental)", rol) ~ 0,
                                                                         grepl("(Vicepresidente)", rol) ~ 0,
                                                                         grepl("(Presidenta)", rol) ~ 0,
                                                                         grepl("(Presidenta accidental)", rol) ~ 0,
                                                                         grepl("(Vicepresidenta)", rol) ~ 0,
                                                                         grepl("(Secretario General)", rol) ~ 0,
                                                                         grepl("(Secretaria General)", rol) ~ 0,
                                                                         grepl("(Secretario General subrogante)", rol) ~ 0,
                                                                         grepl("(Secretaria General subrogante)", rol) ~ 0,
                                                                         grepl("(Proecretario)", rol) ~ 0,
                                                                         grepl("(Secretario)", rol) ~ 0,
                                                                         .default = 1)
  )

  
  unidades_analisis$ley <- txt

  
  unidades_analisis <- unidades_analisis %>%
    mutate(
      texto_sin_header = sub("^[^\n]*\n?", "", texto),
      texto_limpio = str_squish(texto_sin_header),
      n_palabras = str_count(texto_limpio, "\\b\\p{L}+\\b")
    )%>%
    filter(n_palabras >= 25) %>%
    select(-texto_sin_header, -texto_limpio)
  
  discursos <- subset(unidades_analisis, id!=0 & discurso == 1, select = c("ley", "id", "año", "mes", "apellido", "rol", "texto", "discurso"))
  
  write.csv2(discursos, paste0("datos/procesados/discursos/discursos", txt, ".csv"))
  
}

sapply(leyes, procesamiento)

