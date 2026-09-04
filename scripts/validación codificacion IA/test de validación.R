library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(here)

setwd(here())

muestras_pablo <- read_xlsx("datos/muestras de validación/raw/muestra_pablo.xlsx") %>% mutate(id = paste0(ley, id)) 
muestras_juan <- read_xlsx("datos/muestras de validación/raw/muestra_juan.xlsx") %>% mutate(id = paste0(ley, id))  
muestras_pablo_sep <- read_xlsx("datos/muestras de validación/raw/muestra_pablo_sep.xlsx") %>% mutate(id = paste0(ley, id)) %>% mutate(coder = "pablo")
muestras_juan_sep <- read_xlsx("datos/muestras de validación/raw/muestra_juan_sep.xlsx") %>% mutate(id = paste0(ley, id))  %>% mutate(coder = "juan")
muestras_ia <- read_xlsx("datos/muestras de validación/raw/muestra_ia.xlsx")  %>% mutate(coder = "ia")

muestras <- rbind(muestras_ia, muestras_juan_sep, muestras_pablo_sep)

contenido_pablo <- muestras_pablo %>% select(2, 8:10)
contenido_juan <- muestras_juan %>% select(2, 8:10)
contenido_ia <- muestras_ia %>% select(2, 8:10)

contenido_pablo_ia <- inner_join(contenido_pablo, contenido_ia, by = "id", suffix = c("_pablo", "_ia"))
contenido_juan_ia <- inner_join(contenido_juan, contenido_ia, by = "id", suffix = c("_juan", "_ia"))
contenido_pablo_juan <- inner_join(contenido_pablo, contenido_juan, by = "id",suffix = c("_pablo", "_juan")) 

analisis_contenido_juan <- contenido_juan_ia %>% 
  mutate(check1 = ifelse(victima_juan == victima_ia, 0, 1),
         check2 = ifelse(enfoques_juan == enfoques_ia, 0, 1),
         check3 = ifelse(tipo_juan == tipo_ia, 0, 1)
  ) 

estadisticas_juan <- analisis_contenido_juan %>% 
  summarise(porcentaje_victimas = mean(check1),
            se1 = sd(check1)/sqrt(n()),
            porcentaje_enfoques = mean(check2),
            se2 = sd(check2)/sqrt(n()),
            porcentaje_tipo = mean(check3),
            se3 = sd(check3)/sqrt(n()),
            n = n()
  )%>% 
  mutate(codificador = "juan-ia")

analisis_contenido_pablo <- contenido_pablo_ia %>% 
  mutate(check1 = ifelse(victima_pablo == victima_ia, 0, 1),
         check2 = ifelse(enfoques_pablo == enfoques_ia, 0, 1),
         check3 = ifelse(tipo_pablo == tipo_ia, 0, 1)
  ) 

estadisticas_pablo <- analisis_contenido_pablo %>% 
  summarise(porcentaje_victimas = mean(check1),
            se1 = sd(check1)/sqrt(n()),
            porcentaje_enfoques = mean(check2),
            se2 = sd(check2)/sqrt(n()),
            porcentaje_tipo = mean(check3),
            se3 = sd(check3)/sqrt(n()),
            n = n()
  )%>% 
  mutate(codificador = "pablo-ia")

analisis_contenido_pablo_juan <- contenido_pablo_juan %>% 
  mutate(check1 = ifelse(victima_pablo == victima_juan, 0, 1),
         check2 = ifelse(enfoques_pablo == enfoques_juan, 0, 1),
         check3 = ifelse(tipo_pablo == tipo_juan, 0, 1)
  ) 

estadisticas_pablo_juan <- analisis_contenido_pablo_juan %>% 
  summarise(porcentaje_victimas = mean(check1),
            se1 = sd(check1)/sqrt(n()),
            porcentaje_enfoques = mean(check2),
            se2 = sd(check2)/sqrt(n()),
            porcentaje_tipo = mean(check3),
            se3 = sd(check3)/sqrt(n()),
            n = n()
  )%>% 
  mutate(codificador = "pablo-juan")

resultados_contenido <- rbind(estadisticas_pablo_juan, estadisticas_pablo, estadisticas_juan)

write_xlsx(resultados_contenido, "datos/muestras de validación/outputs/generales_contenido.xlsx")

#----Cohen's Kappa Victima----

victimas <- muestras_pablo %>% select(id, victima) %>% rename("pablo" = "victima")
victimas<- left_join(victimas, muestras_juan, by = "id") %>% rename("juan" = "victima")
victimas<- left_join(victimas, muestras_ia, by = "id") %>% rename("ia" = "victima") %>% select(id, pablo, juan, ia)

datos_raters <- data.frame(
  IA = victimas$ia,
  Pablo = victimas$pablo
)
victima_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = victimas$ia,
  Juan = victimas$juan
)
victima_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = victimas$juan,
  Pablo = victimas$pablo
)
victima_pablo_juan <- kappa2(datos_raters)

#----Cohen's Kappa enfoques----

enfoques <- muestras_pablo %>% select(id, enfoques) %>% rename("pablo" = "enfoques")
enfoques<- left_join(enfoques, muestras_juan, by = "id") %>% rename("juan" = "enfoques")
enfoques<- left_join(enfoques, muestras_ia, by = "id") %>% rename("ia" = "enfoques") %>% select(id, pablo, juan, ia)

datos_raters <- data.frame(
  IA = enfoques$ia,
  Pablo = enfoques$pablo
)
enfoque_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = enfoques$ia,
  Juan = enfoques$juan
)
enfoque_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = enfoques$juan,
  Pablo = enfoques$pablo
)
enfoque_pablo_juan <- kappa2(datos_raters)

# ---- Cohen's Kappa Tipo ----

tipo <- muestras_pablo %>% select(id, tipo) %>% rename("pablo" = "tipo")
tipo<- left_join(tipo, muestras_juan, by = "id") %>% rename("juan" = "tipo")
tipo<- left_join(tipo, muestras_ia, by = "id") %>% rename("ia" = "tipo") %>% select(id, pablo, juan, ia)

datos_raters <- data.frame(
  IA = tipo$ia,
  Pablo = tipo$pablo
)
tipo_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = tipo$ia,
  Juan = tipo$juan
)

tipo_juan_ia <-kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = tipo$juan,
  Pablo = tipo$pablo
)
tipo_pablo_juan <-kappa2(datos_raters)

#--- Resultados Kappa contenido----


extraer_kappa <- function(resultado, comparacion) {
  
  data.frame(
    Comparacion = comparacion,
    Metodo = resultado$method,
    N = resultado$subjects,
    Raters = resultado$raters,
    Kappa = resultado$value,
    Estadistico = resultado$statistic,
    p_valor = resultado$p.value
  )
}

tabla_resultados <- rbind(
  extraer_kappa(victima_pablo_ia, "Víctima: IA vs. Pablo"),
  extraer_kappa(victima_juan_ia, "Víctima: IA vs. Juan"),
  extraer_kappa(victima_pablo_juan, "Víctima: Pablo vs. Juan"),
  extraer_kappa(enfoque_pablo_ia, "Enfoque: IA vs. Pablo"),
  extraer_kappa(enfoque_juan_ia, "Enfoque: IA vs. Juan"),
  extraer_kappa(enfoque_pablo_juan, "Enfoque: Pablo vs. Juan"),
  extraer_kappa(tipo_pablo_ia, "Tipo: IA vs. Pablo"),
  extraer_kappa(tipo_juan_ia, "Tipo: IA vs. Juan"),
  extraer_kappa(tipo_pablo_juan, "Tipo: Pablo vs. Juan")
)

write_xlsx(tabla_resultados, "datos/muestras de validación/outputs/kappa_contenido.xlsx")


#---- Kappa Conexiones ----

conexiones <- muestras %>% select(2, 22, 11:15) %>% pivot_longer(3:7) %>% select(-name) %>% rename("conexiones"="value") %>% subset(!is.na(conexiones))

hacer_wide_multi <- function(data, var) {
  data %>%
    select(id, coder, all_of(var)) %>%
    rename(respuesta = all_of(var)) %>%
    mutate(respuesta = as.character(respuesta)) %>%
    separate_rows(respuesta, sep = ";") %>%
    mutate(respuesta = str_trim(respuesta)) %>%
    distinct(id, coder, respuesta) %>%
    mutate(
      valor = 1L,
      nombre_columna = paste0(var, "_", respuesta)
    ) %>%
    select(id, coder, nombre_columna, valor) %>%
    pivot_wider(
      names_from = nombre_columna,
      values_from = valor,
      values_fill = 0
    )
}

cols_multi <- "conexiones"

multi_wide <- cols_multi %>%
  map(~ hacer_wide_multi(conexiones, .x)) %>%
  reduce(full_join, by = "id")

categorias_conexion <- paste0("conexiones_", 0:12)

categorias_faltantes <- setdiff(
  categorias_conexion,
  names(multi_wide)
)

if (length(categorias_faltantes) > 0) {
  stop(
    "No se encontraron estas columnas: ",
    paste(categorias_faltantes, collapse = ", ")
  )
}

multi_wide <- multi_wide %>%
  mutate(
    coder = tolower(trimws(coder))
  )

lista_conexiones <- categorias_conexion %>%
  set_names() %>%
  map(
    function(categoria_actual) {
      
      multi_wide %>%
        select(
          id,
          coder,
          valor = all_of(categoria_actual)
        ) %>%
        mutate(
          valor = as.integer(valor)
        ) %>%
        pivot_wider(
          names_from = coder,
          values_from = valor
        ) %>%
        select(
          id,
          any_of(c("ia", "juan", "pablo"))
        ) %>%
        arrange(id)
    }
  )

source("scripts/validación codificacion IA/conexiones.R")

#---- Resultados Conexiones ----

# Función para extraer los elementos relevantes de kappa2
extraer_resultado_kappa <- function(resultado, conexion, comparacion) {
  
  tibble(
    conexion = conexion,
    comparacion = comparacion,
    sujetos = resultado$subjects,
    raters = resultado$raters,
    kappa = resultado$value,
    z = resultado$statistic,
    p_value = resultado$p.value
  )
}

# Nombres de los objetos en el orden deseado
nombres_objetos <- unlist(
  lapply(0:12, function(i) {
    c(
      paste0("conexion", i, "_pablo_ia"),
      paste0("conexion", i, "_juan_ia"),
      paste0("conexion", i, "_pablo_juan")
    )
  })
)

# Información correspondiente a cada objeto
informacion_objetos <- tibble(
  objeto = nombres_objetos,
  conexion = rep(0:12, each = 3),
  comparacion = rep(
    c("Pablo - IA", "Juan - IA", "Pablo - Juan"),
    times = 13
  )
)

# Comprobar que todos los objetos existen
objetos_faltantes <- informacion_objetos$objeto[
  !informacion_objetos$objeto %in% ls(envir = .GlobalEnv)
]

if (length(objetos_faltantes) > 0) {
  stop(
    paste(
      "No existen estos objetos:",
      paste(objetos_faltantes, collapse = ", ")
    )
  )
}

# Construir la tabla
tabla_kappa_conexiones <- map2_dfr(
  informacion_objetos$objeto,
  seq_len(nrow(informacion_objetos)),
  function(nombre_objeto, fila) {
    
    resultado <- get(nombre_objeto, envir = .GlobalEnv)
    
    extraer_resultado_kappa(
      resultado = resultado,
      conexion = informacion_objetos$conexion[fila],
      comparacion = informacion_objetos$comparacion[fila]
    )
  }
)

tabla_kappa_formateada <- tabla_kappa_conexiones %>%
  mutate(
    interpretacion = case_when(
      is.na(kappa) ~ NA_character_,
      kappa < 0    ~ "Inferior al azar",
      kappa < 0.21 ~ "Leve",
      kappa < 0.41 ~ "Aceptable",
      kappa < 0.61 ~ "Moderada",
      kappa < 0.81 ~ "Sustancial",
      kappa <= 1   ~ "Casi perfecta"
    ),
    
    kappa = round(kappa, 2),
    z = round(z, 2),
    
    p = case_when(
      is.na(p_value)  ~ NA_character_,
      p_value < .001  ~ "< .001",
      TRUE            ~ paste0("= ", sprintf("%.3f", p_value))
    )
  ) %>%
  select(
    conexion,
    comparacion,
    sujetos,
    kappa,
    interpretacion,
    z,
    p
  )


porcentajes_conexiones_observados <- rbind(
  conexion0_porcentaje_observado,
  conexion1_porcentaje_observado,
  conexion2_porcentaje_observado,
  conexion3_porcentaje_observado,
  conexion4_porcentaje_observado,
  conexion5_porcentaje_observado,
  conexion6_porcentaje_observado,
  conexion7_porcentaje_observado,
  conexion8_porcentaje_observado,
  conexion9_porcentaje_observado,
  conexion10_porcentaje_observado,
  conexion11_porcentaje_observado,
  conexion12_porcentaje_observado
)


write_xlsx(tabla_kappa_formateada, "datos/muestras de validación/outputs/tabla_kappa_conexiones.xlsx")
write_xlsx(porcentajes_conexiones_observados, "datos/muestras de validación/outputs/porcentajes_conexiones_observados.xlsx")

#---- Kappa Practicas ----

practicas <- muestras %>% select(2, 22, 16:18) %>% pivot_longer(3:5) %>% select(-name) %>% rename("practicas"="value") %>% subset(!is.na(practicas))

hacer_wide_multi <- function(data, var) {
  data %>%
    select(id, coder, all_of(var)) %>%
    rename(respuesta = all_of(var)) %>%
    mutate(respuesta = as.character(respuesta)) %>%
    separate_rows(respuesta, sep = ";") %>%
    mutate(respuesta = str_trim(respuesta)) %>%
    distinct(id, coder, respuesta) %>%
    mutate(
      valor = 1L,
      nombre_columna = paste0(var, "_", respuesta)
    ) %>%
    select(id, coder, nombre_columna, valor) %>%
    pivot_wider(
      names_from = nombre_columna,
      values_from = valor,
      values_fill = 0
    )
}

cols_multi <- "practicas"

multi_wide <- cols_multi %>%
  map(~ hacer_wide_multi(practicas, .x)) %>%
  reduce(full_join, by = "id")

categorias_practicas <- paste0("practicas_", 0:7)

categorias_faltantes <- setdiff(
  categorias_practicas,
  names(multi_wide)
)

if (length(categorias_faltantes) > 0) {
  stop(
    "No se encontraron estas columnas: ",
    paste(categorias_faltantes, collapse = ", ")
  )
}

multi_wide <- multi_wide %>%
  mutate(
    coder = tolower(trimws(coder))
  )

lista_practicas <- categorias_practicas %>%
  set_names() %>%
  map(
    function(categoria_actual) {
      
      multi_wide %>%
        select(
          id,
          coder,
          valor = all_of(categoria_actual)
        ) %>%
        mutate(
          valor = as.integer(valor)
        ) %>%
        pivot_wider(
          names_from = coder,
          values_from = valor
        ) %>%
        select(
          id,
          any_of(c("ia", "juan", "pablo"))
        ) %>%
        arrange(id)
    }
  )

source("scripts/validación codificacion IA/practicas.R")

#---- Resultados Practicas ----

# Función para extraer los elementos relevantes de kappa2
extraer_resultado_kappa <- function(resultado, practicas, comparacion) {
  
  tibble(
    practicas = practicas,
    comparacion = comparacion,
    sujetos = resultado$subjects,
    raters = resultado$raters,
    kappa = resultado$value,
    z = resultado$statistic,
    p_value = resultado$p.value
  )
}

# Nombres de los objetos en el orden deseado
nombres_objetos <- unlist(
  lapply(0:7, function(i) {
    c(
      paste0("practicas", i, "_pablo_ia"),
      paste0("practicas", i, "_juan_ia"),
      paste0("practicas", i, "_pablo_juan")
    )
  })
)

# Información correspondiente a cada objeto
informacion_objetos <- tibble(
  objeto = nombres_objetos,
  practicas = rep(0:7, each = 3),
  comparacion = rep(
    c("Pablo - IA", "Juan - IA", "Pablo - Juan"),
    times = 8
  )
)

# Comprobar que todos los objetos existen
objetos_faltantes <- informacion_objetos$objeto[
  !informacion_objetos$objeto %in% ls(envir = .GlobalEnv)
]

if (length(objetos_faltantes) > 0) {
  stop(
    paste(
      "No existen estos objetos:",
      paste(objetos_faltantes, collapse = ", ")
    )
  )
}

# Construir la tabla
tabla_kappa_practicas <- map2_dfr(
  informacion_objetos$objeto,
  seq_len(nrow(informacion_objetos)),
  function(nombre_objeto, fila) {
    
    resultado <- get(nombre_objeto, envir = .GlobalEnv)
    
    extraer_resultado_kappa(
      resultado = resultado,
      practicas = informacion_objetos$practicas[fila],
      comparacion = informacion_objetos$comparacion[fila]
    )
  }
)

tabla_kappa_formateada <- tabla_kappa_practicas %>%
  mutate(
    interpretacion = case_when(
      is.na(kappa) ~ NA_character_,
      kappa < 0    ~ "Inferior al azar",
      kappa < 0.21 ~ "Leve",
      kappa < 0.41 ~ "Aceptable",
      kappa < 0.61 ~ "Moderada",
      kappa < 0.81 ~ "Sustancial",
      kappa <= 1   ~ "Casi perfecta"
    ),
    
    kappa = round(kappa, 2),
    z = round(z, 2),
    
    p = case_when(
      is.na(p_value)  ~ NA_character_,
      p_value < .001  ~ "< .001",
      TRUE            ~ paste0("= ", sprintf("%.3f", p_value))
    )
  ) %>%
  select(
    practicas,
    comparacion,
    sujetos,
    kappa,
    interpretacion,
    z,
    p
  )


porcentajes_practicas_observados <- rbind(
  practicas0_porcentaje_observado,
  practicas1_porcentaje_observado,
  practicas2_porcentaje_observado,
  practicas3_porcentaje_observado,
  practicas4_porcentaje_observado,
  practicas5_porcentaje_observado,
  practicas6_porcentaje_observado,
  practicas7_porcentaje_observado
)


write_xlsx(tabla_kappa_formateada, "datos/muestras de validación/outputs/tabla_kappa_practicas.xlsx")
write_xlsx(porcentajes_practicas_observados, "datos/muestras de validación/outputs/porcentajes_practicas_observados.xlsx")


#---- Kappa Valoraciones ----

valoraciones <- muestras %>% select(2, 22, 19:21) %>% pivot_longer(3:5) %>% select(-name) %>% rename("valoraciones"="value") %>% subset(!is.na(valoraciones))

hacer_wide_multi <- function(data, var) {
  data %>%
    select(id, coder, all_of(var)) %>%
    rename(respuesta = all_of(var)) %>%
    mutate(respuesta = as.character(respuesta)) %>%
    separate_rows(respuesta, sep = ";") %>%
    mutate(respuesta = str_trim(respuesta)) %>%
    distinct(id, coder, respuesta) %>%
    mutate(
      valor = 1L,
      nombre_columna = paste0(var, "_", respuesta)
    ) %>%
    select(id, coder, nombre_columna, valor) %>%
    pivot_wider(
      names_from = nombre_columna,
      values_from = valor,
      values_fill = 0
    )
}

cols_multi <- "valoraciones"

multi_wide <- cols_multi %>%
  map(~ hacer_wide_multi(valoraciones, .x)) %>%
  reduce(full_join, by = "id")

multi_wide$valoraciones_5 <- 0
multi_wide$valoraciones_7 <- 0

categorias_valoraciones <- paste0("valoraciones_", 0:7)

categorias_faltantes <- setdiff(
  categorias_valoraciones,
  names(multi_wide)
)

if (length(categorias_faltantes) > 0) {
  stop(
    "No se encontraron estas columnas: ",
    paste(categorias_faltantes, collapse = ", ")
  )
}


multi_wide <- multi_wide %>%
  mutate(
    coder = tolower(trimws(coder))
  )

lista_valoraciones <- categorias_valoraciones %>%
  set_names() %>%
  map(
    function(categoria_actual) {
      
      multi_wide %>%
        select(
          id,
          coder,
          valor = all_of(categoria_actual)
        ) %>%
        mutate(
          valor = as.integer(valor)
        ) %>%
        pivot_wider(
          names_from = coder,
          values_from = valor
        ) %>%
        select(
          id,
          any_of(c("ia", "juan", "pablo"))
        ) %>%
        arrange(id)
    }
  )

source("scripts/validación codificacion IA/valoraciones.R")

#---- Resultados valoraciones ----

# Función para extraer los elementos relevantes de kappa2
extraer_resultado_kappa <- function(resultado, valoraciones, comparacion) {
  
  tibble(
    valoraciones = valoraciones,
    comparacion = comparacion,
    sujetos = resultado$subjects,
    raters = resultado$raters,
    kappa = resultado$value,
    z = resultado$statistic,
    p_value = resultado$p.value
  )
}

# Nombres de los objetos en el orden deseado
nombres_objetos <- unlist(
  lapply(0:7, function(i) {
    c(
      paste0("valoraciones", i, "_pablo_ia"),
      paste0("valoraciones", i, "_juan_ia"),
      paste0("valoraciones", i, "_pablo_juan")
    )
  })
)

# Información correspondiente a cada objeto
informacion_objetos <- tibble(
  objeto = nombres_objetos,
  valoraciones = rep(0:7, each = 3),
  comparacion = rep(
    c("Pablo - IA", "Juan - IA", "Pablo - Juan"),
    times = 8
  )
)

# Comprobar que todos los objetos existen
objetos_faltantes <- informacion_objetos$objeto[
  !informacion_objetos$objeto %in% ls(envir = .GlobalEnv)
]

if (length(objetos_faltantes) > 0) {
  stop(
    paste(
      "No existen estos objetos:",
      paste(objetos_faltantes, collapse = ", ")
    )
  )
}

# Construir la tabla
tabla_kappa_valoraciones <- map2_dfr(
  informacion_objetos$objeto,
  seq_len(nrow(informacion_objetos)),
  function(nombre_objeto, fila) {
    
    resultado <- get(nombre_objeto, envir = .GlobalEnv)
    
    extraer_resultado_kappa(
      resultado = resultado,
      valoraciones = informacion_objetos$valoraciones[fila],
      comparacion = informacion_objetos$comparacion[fila]
    )
  }
)

tabla_kappa_formateada <- tabla_kappa_valoraciones %>%
  mutate(
    interpretacion = case_when(
      is.na(kappa) ~ NA_character_,
      kappa < 0    ~ "Inferior al azar",
      kappa < 0.21 ~ "Leve",
      kappa < 0.41 ~ "Aceptable",
      kappa < 0.61 ~ "Moderada",
      kappa < 0.81 ~ "Sustancial",
      kappa <= 1   ~ "Casi perfecta"
    ),
    
    kappa = round(kappa, 2),
    z = round(z, 2),
    
    p = case_when(
      is.na(p_value)  ~ NA_character_,
      p_value < .001  ~ "< .001",
      TRUE            ~ paste0("= ", sprintf("%.3f", p_value))
    )
  ) %>%
  select(
    valoraciones,
    comparacion,
    sujetos,
    kappa,
    interpretacion,
    z,
    p
  )


porcentajes_valoraciones_observados <- rbind(
  valoraciones0_porcentaje_observado,
  valoraciones1_porcentaje_observado,
  valoraciones2_porcentaje_observado,
  valoraciones3_porcentaje_observado,
  valoraciones4_porcentaje_observado,
  valoraciones5_porcentaje_observado,
  valoraciones6_porcentaje_observado,
  valoraciones7_porcentaje_observado
)


write_xlsx(tabla_kappa_formateada, "datos/muestras de validación/outputs/tabla_kappa_valoraciones.xlsx")
write_xlsx(porcentajes_valoraciones_observados, "datos/muestras de validación/outputs/porcentajes_valoraciones_observados.xlsx")

#----McNemar----
#----victima ----
"pablo-ia" <- analisis_contenido_pablo$check1
"pablo-juan" <- analisis_contenido_pablo_juan$check1

mcnemar_victimas_pablo <- table(`pablo-ia`, `pablo-juan`)

test_victima_pablo <- mcnemar.test(mcnemar_victimas_pablo)%>% print()

"juan-ia" <- analisis_contenido_juan$check1

mcnemar_victimas_juan <- table(`juan-ia`, `pablo-juan`)

test_victima_juan <- mcnemar.test(mcnemar_victimas_juan) %>% print()

#----enfoque----

"pablo-ia" <- analisis_contenido_pablo$check2
"pablo-juan" <- analisis_contenido_pablo_juan$check2

mcnemar_enfoques_pablo <- table(`pablo-ia`, `pablo-juan`)

test_victima_pablo <- mcnemar.test(mcnemar_enfoques_pablo)%>% print()

"juan-ia" <- analisis_contenido_juan$check2

mcnemar_enfoques_juan <- table(`juan-ia`, `pablo-juan`)

test_victima_juan <- mcnemar.test(mcnemar_enfoques_juan) %>% print()

#----tipo----

"pablo-ia" <- analisis_contenido_pablo$check3
"pablo-juan" <- analisis_contenido_pablo_juan$check3

mcnemar_tipo_pablo <- table(`pablo-ia`, `pablo-juan`)

test_victima_pablo <- mcnemar.test(mcnemar_tipo_pablo)%>% print()

"juan-ia" <- analisis_contenido_juan$check3

mcnemar_tipo_juan <- table(`juan-ia`, `pablo-juan`)

test_victima_juan <- mcnemar.test(mcnemar_tipo_juan) %>% print()

#---- T-Test Pareados ----
# ---- analisis divergencia discursos tomando humanos (juan) como base ----

#Conexiones 
conexiones_juan <- muestras_juan %>% select(2, 11)

conexiones_pablo <- muestras_pablo %>% select(2, 11)

conexiones_juan_sep <- muestras_juan_sep %>% select(2, 11:15)  %>%
  pivot_longer(cols = 2:6, names_to = "conexion", names_prefix = "conexion", values_to = "respuesta") %>%
  subset(!is.na(respuesta))

conexiones_ia <- muestras_ia %>% select(2, 11:15)  %>%
  pivot_longer(cols = 2:6, names_to = "conexion", names_prefix = "conexion", values_to = "respuesta") %>%
  subset(!is.na(respuesta))

conexiones_pablo_ia <- inner_join(conexiones_pablo, conexiones_ia, by = "id", suffix = c("_pablo", "_ia")
) %>%
  mutate(
    conexion_pablo = str_replace_all(conexion_pablo, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", conexion_pablo, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_conexiones_pablo_ia <- conexiones_pablo_ia %>% 
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "pablo-ia")

conexiones_juan_ia <- inner_join(conexiones_juan, conexiones_ia, by = "id", suffix = c("_juan", "_ia")
) %>%
  mutate(
    conexion_juan = str_replace_all(conexion_juan, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", conexion_juan, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_conexiones_juan_ia <- conexiones_juan_ia %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "juan-ia")

conexiones_pablo_juan <- inner_join(conexiones_pablo, conexiones_juan_sep, by = "id", suffix = c("_pablo", "_juan")
) %>%
  mutate(
    conexion_pablo = str_replace_all(conexion_pablo, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", conexion_pablo, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  ) 

resultados_conexiones_pablo_juan <- conexiones_pablo_juan %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>%  
  mutate(codificador = "pablo-juan")   

# practicas
practicas_ia <- muestras_ia %>% select(2, 16:18)  %>%
  pivot_longer(cols = 2:4, names_to = "practica", names_prefix = "pratica", values_to = "respuesta")%>%
  subset(!is.na(respuesta))

practicas_juan <- muestras_juan %>% select(2, 12)

practicas_pablo <- muestras_pablo %>% select(2, 12)

practicas_juan_sep <- muestras_juan_sep %>% select(2, 16:18)  %>%
  pivot_longer(cols = 2:4, names_to = "practica", names_prefix = "practica", values_to = "respuesta") %>%
  subset(!is.na(respuesta))

practicas_pablo_ia <- inner_join(practicas_pablo, practicas_ia, by = "id", suffix = c("_pablo", "_ia")
) %>%
  mutate(
    practica_pablo = str_replace_all(practica_pablo, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", practica_pablo, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  ) 

resultados_practicas_pablo_ia <- practicas_pablo_ia %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "pablo-ia")

practicas_juan_ia <- inner_join(practicas_juan, practicas_ia, by = "id", suffix = c("_juan", "_ia")
) %>%
  mutate(
    practica_juan = str_replace_all(practica_juan, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", practica_juan, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_practicas_juan_ia <- practicas_juan_ia %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "juan-ia")

practicas_pablo_juan <- inner_join(practicas_pablo, practicas_juan_sep, by = "id", suffix = c("_pablo", "_juan")
) %>%
  mutate(
    practica_pablo = str_replace_all(practica_pablo, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", practica_pablo, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_practicas_pablo_juan <- practicas_pablo_juan %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "pablo-juan")   

#valoracion
valoracion_juan <- muestras_juan %>% select(2, 13)

valoracion_pablo <- muestras_pablo %>% select(2, 13)

valoracion_juan_sep <- muestras_juan_sep %>% select(2, 19:21)  %>%
  pivot_longer(cols = 2:4, names_to = "valoracion", names_prefix = "valoracion", values_to = "respuesta") %>%
  subset(!is.na(respuesta))

valoracion_ia <- muestras_ia %>% select(2, 19:21)  %>%
  pivot_longer(cols = 2:4, names_to = "valoracion", names_prefix = "valoracion", values_to = "respuesta") %>%
  subset(!is.na(respuesta))

valoracion_pablo_ia <- inner_join(valoracion_pablo, valoracion_ia, by = "id", suffix = c("_pablo", "_ia")
) %>%
  mutate(
    valoracion_pablo = str_replace_all(valoracion_pablo, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", valoracion_pablo, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_valoracion_pablo_ia <- valoracion_pablo_ia %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "pablo-ia")

valoracion_juan_ia <- inner_join(valoracion_juan, valoracion_ia, by = "id", suffix = c("_juan", "_ia")
) %>%
  mutate(
    valoracion_juan = str_replace_all(valoracion_juan, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", valoracion_juan, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_valoracion_juan_ia <- valoracion_juan_ia %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "juan-ia")

valoracion_pablo_juan <- inner_join(valoracion_pablo, valoracion_juan_sep, by = "id", suffix = c("_pablo", "_juan")
) %>%
  mutate(
    valoracion_pablo = str_replace_all(valoracion_pablo, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", valoracion_pablo, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  ) 

resultados_valoracion_pablo_juan <- valoracion_pablo_juan %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "pablo-juan")   

# ---- analisis divergencia discursos tomando IA (pablo) como base ----

#Conexiones 

conexiones_juan_sep <- muestras_juan_sep %>% select(2, 11:15) %>%
  pivot_longer(cols = 2:6, names_to = "conexion", names_prefix = "conexion", values_to = "respuesta") %>%
  subset(!is.na(respuesta))

conexiones_pablo_sep <- muestras_pablo_sep %>% select(2, 11:15)  %>%
  pivot_longer(cols = 2:6, names_to = "conexion", names_prefix = "conexion", values_to = "respuesta") %>%
  subset(!is.na(respuesta)) 

conexiones_juan <- muestras_juan %>% select(2, 11) 

conexiones_ia <- muestras_ia %>% select(2, 11:15) %>% mutate(conexion = paste0(conexion1, ";", conexion2, ";",conexion3, ";",conexion4, ";",conexion5)) %>% select(-2,-3, -4, -5, -6)

conexiones_ia_pablo <- inner_join(conexiones_pablo_sep, conexiones_ia, by = "id", suffix = c("_pablo", "_ia")
) %>%
  mutate(
    conexion_ia = str_replace_all(conexion_ia, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", conexion_ia, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_conexiones_ia_pablo <- conexiones_ia_pablo %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "ia-pablo")

conexiones_ia_juan <- inner_join(conexiones_juan_sep, conexiones_ia, by = "id", suffix = c("_juan", "_ia")
) %>%
  mutate(
    conexion_ia = str_replace_all(conexion_ia, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", conexion_ia, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_conexiones_ia_juan <- conexiones_ia_juan %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>%  
  mutate(codificador = "ia-juan")

conexiones_juan_pablo <- inner_join(conexiones_pablo_sep, conexiones_juan, by = "id", suffix = c("_pablo", "_juan")
) %>%
  mutate(
    conexion_juan = str_replace_all(conexion_juan, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", conexion_juan, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  ) 

resultados_conexiones_juan_pablo <- conexiones_juan_pablo %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "juan-pablo")   

# practicas
practica_juan_sep <- muestras_juan_sep %>% select(2, 16:18) %>%
  pivot_longer(cols = 2:4, names_to = "practica", names_prefix = "practica", values_to = "respuesta") %>%
  subset(!is.na(respuesta))

practica_pablo_sep <- muestras_pablo_sep %>% select(2, 16:18)  %>%
  pivot_longer(cols = 2:4, names_to = "practica", names_prefix = "practica", values_to = "respuesta") %>%
  subset(!is.na(respuesta)) 

practica_juan <- muestras_juan %>% select(2, 12) 

practica_ia <- muestras_ia %>% select(2, 16:18) %>% mutate(practica = paste0(practica1, ";", practica2, ";",practica3)) %>% select(-2,-3, -4)

practica_ia_pablo <- inner_join(practica_pablo_sep, practica_ia, by = "id", suffix = c("_pablo", "_ia")
) %>%
  mutate(
    practica_ia = str_replace_all(practica_ia, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", practica_ia, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_practica_ia_pablo <- practica_ia_pablo %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "ia-pablo")

practica_ia_juan <- inner_join(practica_juan_sep, practica_ia, by = "id", suffix = c("_juan", "_ia")
) %>%
  mutate(
    practica_ia = str_replace_all(practica_ia, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", practica_ia, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_practica_ia_juan <- practica_ia_juan %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "ia-juan")

practica_juan_pablo <- inner_join(practica_pablo_sep, practica_juan, by = "id", suffix = c("_pablo", "_juan")
) %>%
  mutate(
    practica_pablo = str_replace_all(practica_pablo, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", practica_pablo, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  ) 

resultados_practica_juan_pablo <- practica_juan_pablo %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "juan-pablo")   


#valoracion
valoracion_juan_sep <- muestras_juan_sep %>% select(2, 19:21) %>%
  pivot_longer(cols = 2:4, names_to = "valoracion", names_prefix = "valoracion", values_to = "respuesta") %>%
  subset(!is.na(respuesta))

valoracion_pablo_sep <- muestras_pablo_sep %>% select(2, 19:21)  %>%
  pivot_longer(cols = 2:4, names_to = "valoracion", names_prefix = "valoracion", values_to = "respuesta") %>%
  subset(!is.na(respuesta)) 

valoracion_juan <- muestras_juan %>% select(2, 13) 

valoracion_ia <- muestras_ia %>% select(2, 19:21) %>% mutate(valoracion = paste0(valoracion1, ";", valoracion2, ";",valoracion3)) %>% select(-2,-3, -4)

valoracion_ia_pablo <- inner_join(valoracion_pablo_sep, valoracion_ia, by = "id", suffix = c("_pablo", "_ia")
) %>%
  mutate(
    valoracion_ia = str_replace_all(valoracion_ia, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", valoracion_ia, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_valoracion_ia_pablo <- valoracion_ia_pablo %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "ia-pablo")

valoracion_ia_juan <- inner_join(valoracion_juan_sep, valoracion_ia, by = "id", suffix = c("_juan", "_ia")
) %>%
  mutate(
    valoracion_ia = str_replace_all(valoracion_ia, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", valoracion_ia, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  )

resultados_valoracion_ia_juan <- valoracion_ia_juan %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "ia-juan")

valoracion_juan_pablo <- inner_join(valoracion_pablo_sep, valoracion_juan, by = "id", suffix = c("_pablo", "_juan")
) %>%
  mutate(
    valoracion_pablo = str_replace_all(valoracion_pablo, "\\s+", ""),
    check = ifelse(str_detect(paste0(";", valoracion_pablo, ";"),
                              paste0(";", respuesta, ";")),0,1
    )
  ) 

resultados_valoracion_juan_pablo <- valoracion_juan_pablo %>%
  group_by(id) %>%
  summarise(porcentaje_divergencia = mean(check)
  )%>% 
  mutate(codificador = "juan-pablo")   


#---- tests pareados humanos base ----

test_conexion_pablo3.1 <- left_join(resultados_conexiones_pablo_ia, resultados_conexiones_pablo_juan, by = "id") 
t.test(test_conexion_pablo3.1$porcentaje_divergencia.x, test_conexion_pablo3.1$porcentaje_divergencia.y, paired = TRUE)

test_conexion_juan3.1 <- left_join(resultados_conexiones_juan_ia, resultados_conexiones_juan_pablo, by = "id") 
t.test(test_conexion_juan3.1$porcentaje_divergencia.x, test_conexion_juan3.1$porcentaje_divergencia.y, paired = TRUE)

test_practica_pablo3.1 <- left_join(resultados_practicas_pablo_ia, resultados_practicas_pablo_juan, by = "id") 
t.test(test_practica_pablo3.1$porcentaje_divergencia.x, test_practica_pablo3.1$porcentaje_divergencia.y, paired = TRUE)

test_practica_juan3.1 <- left_join(resultados_practicas_juan_ia, resultados_practica_juan_pablo, by = "id") 
t.test(test_practica_juan3.1$porcentaje_divergencia.x, test_practica_juan3.1$porcentaje_divergencia.y, paired = TRUE)

test_valoracion_pablo3.1 <- left_join(resultados_valoracion_pablo_ia, resultados_valoracion_pablo_juan, by = "id") 
t.test(test_valoracion_pablo3.1$porcentaje_divergencia.x, test_valoracion_pablo3.1$porcentaje_divergencia.y, paired = TRUE)

test_valoracion_juan3.1 <- left_join(resultados_valoracion_juan_ia, resultados_valoracion_juan_pablo, by = "id") 
t.test(test_valoracion_juan3.1$porcentaje_divergencia.x, test_valoracion_juan3.1$porcentaje_divergencia.y, paired = TRUE)

#---- tests pareados IA base ----

test_conexion_pablo3.1 <- left_join(resultados_conexiones_ia_pablo, resultados_conexiones_pablo_juan, by = "id") 
t.test(test_conexion_pablo3.1$porcentaje_divergencia.x, test_conexion_pablo3.1$porcentaje_divergencia.y, paired = TRUE)

test_conexion_juan3.1 <- left_join(resultados_conexiones_ia_juan, resultados_conexiones_juan_pablo, by = "id") 
t.test(test_conexion_juan3.1$porcentaje_divergencia.x, test_conexion_juan3.1$porcentaje_divergencia.y, paired = TRUE)

test_practica_pablo3.1 <- left_join(resultados_practica_ia_pablo, resultados_practicas_pablo_juan, by = "id") 
t.test(test_practica_pablo3.1$porcentaje_divergencia.x, test_practica_pablo3.1$porcentaje_divergencia.y, paired = TRUE)

test_practica_juan3.1 <- left_join(resultados_practica_ia_juan, resultados_practica_juan_pablo, by = "id") 
t.test(test_practica_juan3.1$porcentaje_divergencia.x, test_practica_juan3.1$porcentaje_divergencia.y, paired = TRUE)

test_valoracion_pablo3.1 <- left_join(resultados_valoracion_ia_pablo, resultados_valoracion_pablo_juan, by = "id") 
t.test(test_valoracion_pablo3.1$porcentaje_divergencia.x, test_valoracion_pablo3.1$porcentaje_divergencia.y, paired = TRUE)

test_valoracion_juan3.1 <- left_join(resultados_valoracion_ia_juan, resultados_valoracion_juan_pablo, by = "id") 
t.test(test_valoracion_juan3.1$porcentaje_divergencia.x, test_valoracion_juan3.1$porcentaje_divergencia.y, paired = TRUE)


