library(dplyr)
library(tibble)
library(tidyr)
library(gemini.R)
library(purrr)
library(jsonlite)
library(stringr)
library(readxl)
library(writexl)
library(here)

setwd(here())

readRenviron("scripts/analisis/.Renviron")
setAPI(Sys.getenv("GEMINI_API_KEY"))

source("scripts/analisis/parseo.R")

datos <- read_xlsx("datos/procesados/pdls_seguridad_metadata_revisados.xlsx") %>% subset(Revision == 1)

lista_leyes <- as.data.frame(datos$num_ley)

names(lista_leyes) <- "leyes"

# Analisis de contenido #

for (ley in lista_leyes$leyes){
  
  sample <- read.csv2(paste0("datos/procesados/discursos/discursos", ley, ".csv"))
  sample$id <- as.character(sample$id) 
  
  #---- analisis contenido ----
  source("scripts/analisis/system_prompt1.R")
  source("scripts/analisis/prompt_contenido.R")
  
  lista_contenido <- vector("list", nrow(sample))
  
  for (i in 1:nrow(sample)){
    discurso <- sample[i,]
    prompt <- crear_prompt_completo(discurso)
    
    analisis <- NULL
    
    # Lógica de reintentos
    for(intento in 1:5){ 
      analisis <- tryCatch(
        gemini(prompt, model = g_model, temperature = g_temperature, seed = g_seed),
        error = function(e) { message(
          sprintf(
            "Error en fila %d intento %d: %s | clase: %s",
            i, intento, conditionMessage(e), paste(class(e), collapse = ", ")
          )
        )
          Sys.sleep(5)
          return(NULL) }
      )
      if(!is.null(analisis)) break 
      
    }  
    # Si falla después de 5 intentos, guardar un marcador en vez de NA
    if (!is.null(analisis)) {
      lista_contenido[[i]] <- analisis
    } else {
      lista_contenido[[i]] <- toJSON(list(status = "ERROR_API", fila = i, intentos = 5))
      message(paste("Fila", i, "quedó marcada como ERROR_API."))
      Sys.sleep(15)
    }
    
    Sys.sleep(2)
  }
  
  contenido_procesado <- lista_contenido %>%
    map(~ if (!is.null(names(.x)) && "text" %in% names(.x)) .x[["text"]] else NA_character_) %>%
    map(limpiar_fences) %>%
    keep(~ !is.null(.x) && !is.na(.x) && nzchar(.x)) %>%
    map(parsear_json_seguro) %>%
    keep(~ !is.null(.x))
  
  contenido <- map_dfr(contenido_procesado, aplanar_contenido)
  
  #---- analisis discurso ----
  source("scripts/analisis/system_prompt2.R")
  source("scripts/analisis/prompt_discurso.R")
  
  lista_discursos <- vector("list", nrow(sample))
  
  for (i in 1:nrow(sample)){
    discurso <- sample[i,]
    prompt <- crear_prompt_completo(discurso)
    
    analisis <- NULL
    
    # Lógica de reintentos
    for(intento in 1:5){ 
      analisis <- tryCatch(
        gemini(prompt, 
               model = g_model, 
               temperature = g_temperature, 
               seed = g_seed),
        error = function(e) { message(
          sprintf(
            "Error en fila %d intento %d: %s | clase: %s",
            i, intento, conditionMessage(e), paste(class(e), collapse = ", ")
          )
        )
          Sys.sleep(5)
          return(NULL) }
      )
      if(!is.null(analisis)) break 
      
    }  
    # Si falla después de 3 intentos, guardar un marcador en vez de NA
    if (!is.null(analisis)) {
      lista_discursos[[i]] <- analisis
    } else {
      lista_discursos[[i]] <- toJSON(list(status = "ERROR_API", fila = i, intentos = 5))
      message(paste("Fila", i, "quedó marcada como ERROR_API."))
      Sys.sleep(15)
    }
    
    Sys.sleep(2)
  }
  
  discursos_procesados <- lista_discursos %>%
    map(~ if (!is.null(names(.x)) && "text" %in% names(.x)) .x[["text"]] else NA_character_) %>%
    map(limpiar_fences) %>%
    keep(~ !is.null(.x) && !is.na(.x) && nzchar(.x)) %>%
    map(parsear_json_seguro) %>%
    keep(~ !is.null(.x))
  
  discursos <- map_dfr(discursos_procesados, aplanar_discurso)
  
  
  #---- union del contenido analizado ----
  
  discursos_analizados <- inner_join(sample, contenido, by = "id")
  discursos_analizados <- left_join(discursos_analizados, discursos, by = "id")
  
  write_xlsx(discursos_analizados, paste0("datos/procesados/resultados/resultados", ley, ".xlsx"))
}
  
lista_leyes <- as.data.frame(datos$num_ley)

names(lista_leyes) <- "leyes"

resultados_unificado <- discursos_analizados

for (ley in lista_leyes$leyes){
  resultado <- read_xlsx(paste0("datos/procesados/resultados/resultados", ley, ".xlsx"))
  resultados_unificado <- bind_rows(resultados_unificado, resultado)
  }

resultados_unificado$ley_id <- paste(resultados_unificado$ley, resultados_unificado$id, sep = "-")

resultados_genero <- resultados_unificado %>% subset(victima != 0, select = c("ley_id", "texto"))

source("scripts/analisis/system_prompt3.R")
source("scripts/analisis/prompt_genero.R")

lista_contenido <- vector("list", nrow(resultados_genero))

for (i in 1:nrow(resultados_genero)){
  discurso <- resultados_genero[i,]
  prompt <- crear_prompt_completo(discurso)
  
  analisis <- NULL
  

for(intento in 1:5){ 
  analisis <- tryCatch(
    gemini(prompt, model = g_model, temperature = g_temperature, seed = g_seed),
    error = function(e) { message(
      sprintf(
        "Error en fila %d intento %d: %s | clase: %s",
        i, intento, conditionMessage(e), paste(class(e), collapse = ", ")
      )
    )
      Sys.sleep(5)
      return(NULL) }
  )
  if(!is.null(analisis)) break 
  
}  
# Si falla después de 5 intentos, guardar un marcador en vez de NA
if (!is.null(analisis)) {
  lista_contenido[[i]] <- analisis
} else {
  lista_contenido[[i]] <- toJSON(list(status = "ERROR_API", fila = i, intentos = 5))
  message(paste("Fila", i, "quedó marcada como ERROR_API."))
  Sys.sleep(15)
}

Sys.sleep(2)
}

contenido_procesado <- lista_contenido %>%
  map(~ if (!is.null(names(.x)) && "text" %in% names(.x)) .x[["text"]] else NA_character_) %>%
  map(limpiar_fences) %>%
  keep(~ !is.null(.x) && !is.na(.x) && nzchar(.x)) %>%
  map(parsear_json_seguro) %>%
  keep(~ !is.null(.x))

mujer <- map_dfr(contenido_procesado, aplanar_genero)

resultados_genero <- left_join(resultados_unificado, mujer, by = "ley_id") %>% 
  mutate(mujer = ifelse(is.na(mujer), 0, mujer))

write_xlsx(resultados_genero, "datos/output/bbdd_final.xlsx")


