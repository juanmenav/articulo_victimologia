# 1) Limpiar cercas y espacios
limpiar_fences <- function(txt) {
  if (is.null(txt) || length(txt) == 0 || is.na(txt)) return(NA_character_)
  txt_limpio <- txt |>
    # Elimina la cerca inicial ```json (con posibles espacios/saltos)
    str_replace("^\\s*```json\\s*", "") |>
    # Elimina la cerca final ```
    str_replace("\\s*```\\s*$", "") |>
    # Recorta espacios/saltos al inicio y final
    str_trim()
  if (txt_limpio == "") NA_character_ else txt_limpio
}

# 2) Valida y parsea JSON de forma segura
parsear_json_seguro <- function(txt) {
  if (is.null(txt) || is.na(txt) || txt == "") return(NULL)
  if (!jsonlite::validate(txt)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# Aplanar un contenido  a tibble 
aplanar_contenido <- function(e) {
  get1_int <- function(x) {
    if (is.null(x) || length(x) == 0) NA_integer_ else suppressWarnings(as.integer(x)[1])
  }
  get1_chr <- function(x) {
    if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]
  }
  
  tibble(
    id  = get1_chr(e$id),
    victima = get1_int(e$victima),
    enfoques = get1_int(e$enfoque),
    tipo = get1_int(e$tipo)
  )
}

# Aplanar un discurso a tibble
aplanar_discurso <- function(e) {
  get1_int <- function(x) {
    if (is.null(x) || length(x) == 0) NA_integer_ else suppressWarnings(as.integer(x)[1])
  }
  get1_chr <- function(x) {
    if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]
  }
  
  tibble(
    id  = get1_chr(e$id),
    conexion = get1_chr(e$conexion),
    practica = get1_chr(e$practica),
    valoracion = get1_chr(e$valoracion)
  )
}

# Aplanar genero a tibble
aplanar_genero <- function(e) {
  get1_int <- function(x) {
    if (is.null(x) || length(x) == 0) NA_integer_ else suppressWarnings(as.integer(x)[1])
  }
  get1_chr <- function(x) {
    if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]
  }
  
  tibble(
    ley_id  = get1_chr(e$id),
    mujer = get1_chr(e$mujer)
  )
}
