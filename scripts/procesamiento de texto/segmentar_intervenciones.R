limpiar_texto <- function(txt) {
  txt <- gsub("Historia de la Ley N° [0-9]{1,2}.[0-9]{3}", "", txt)
  txt <- gsub("Discusión en Sala", "", txt)
  txt <- gsub("Biblioteca del Congreso Nacional de Chile - www\\.bcn\\.cl/historiadelaley - documento generado el [0-9]{1,2}-[A-Za-z]+-[0-9]{4}", "", txt)
  txt <- gsub("Página\\s+\\d+\\s+de [0-9]{1,2}", "", txt)
  txt <- trimws(txt, which = "both") 
  txt <- gsub("^\\s*\\d+\\s*$", "", txt, perl = TRUE)
  
}

pat_header <- paste0(
  "^\\s*(El|La)\\s+",
  "(señor|señora|señorita)\\s+",
  "((?:DE|DEL|DE LA)?\\s*[A-ZÁÉÍÓÚÑÜ]+(?:\\s+[A-ZÁÉÍÓÚÑÜ]+)*)",
  "\\s*,?\\s*",
  "(?:,?\\s*(?:don|doña)\\s+",
  "[A-ZÁÉÍÓÚÑÜ][a-záéíóúñü]+(?:\\s+[A-ZÁÉÍÓÚÑÜ][a-záéíóúñü]+)*)?",
  "\\s*(?:\\(\\s*([^)]*?)\\s*\\))?",
  "\\s*\\.-\\s*$"
)


pat_doc <- "^\\s*\\d+(?:\\.\\d+)*\\.\\s+(Oficio|Informe|Mensaje|Mocióno|Boletín|Primer|Segundo|Tercer|Ley)"

segmentar_intervenciones <- function(txt) {
  
  lineas <- txt %>%
    str_replace_all("\r\n", "\n") %>%
    str_replace_all("(?<!\\.)\n(?!\n)", " ") %>%
    str_split("\n", simplify = FALSE) %>%
    pluck(1) %>%
    str_trim() 
  
  lineas <- lineas[lineas != ""]
  
  encabezado <- str_detect(lineas, pat_header)
  
  id <- cumsum(encabezado)
  
  df <- tibble(
    linea = lineas,
    encabezado = encabezado,
    id = id
  )
  
  df <- df %>% mutate(fecha = ifelse(grepl("(Diario de Sesión en Sesión)", linea), linea, NA)) %>%
    fill(fecha, .direction = "down")
  
  df$fecha <- str_match(df$fecha, "Fecha\\s+(\\d{1,2})\\s+de\\s+([[:alpha:]]+)\\s*,\\s*(\\d{4})")
  
  df$mes  <- tolower(df$fecha[,3])
  df$año <- as.integer(df$fecha[,4])
  
  df$mes <- c(
    enero=1, febrero=2, marzo=3, abril=4, mayo=5, junio=6,
    julio=7, agosto=8, septiembre=9, setiembre=9, octubre=10, noviembre=11, diciembre=12
  )[df$mes]

  out <- df %>%
    filter(id > 0) %>%
    group_by(id, año, mes) %>%
    summarise(
      encabezado = first(linea[encabezado]),
      texto = {
        idx_doc <- which(str_detect(linea, pat_doc))
        if (length(idx_doc) > 0) {
          paste(linea[seq_len(idx_doc[1] - 1)], collapse = "\n")
        } else {
          paste(linea, collapse = "\n")
        }
      },
      .groups = "drop"
    ) %>%
    mutate(
      m = str_match(encabezado, pat_header),
      apellido = m[,4],
      rol = m[,5]
    ) %>%
    select(-m, -encabezado) %>%
    relocate(id, año, mes, apellido, rol, texto)
  
  out
}




