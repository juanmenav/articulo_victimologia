library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(xml2)
library(rvest)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)

setwd("C:/Users/yeyem/OneDrive - King's College London/paper pablo/anexo digital")

bills <- read_xlsx("datos/raw/bbdd_pdl.xlsx")  

bills <- subset(bills, bills$`N° Boletín` %in% "15866-07")

readRenviron("scripts/selección de muestra/.Renviron")

# Clasificación de PDLs dentro de la agenda de seguridad en base al analisis de sus titulos usando LLM

chatgpt_query <- function(prompt, model = "gpt-4o-mini", temperature = 0, max_tokens = 7, # This function defines the query to GPT-4.1-nano for the coding process. Temperature 0 means there is no randomness.
                          api_key = Sys.getenv("OPENAI_API_KEY")) {
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      `Content-Type` = "application/json",
      `Authorization` = paste("Bearer", api_key)
    ),
    body = jsonlite::toJSON(list(
      model = model,
      temperature = temperature,
      max_tokens = max_tokens,
      messages = list(
        list(role = "system", content = "Eres un codificador experto en política criminal. Te voy a dar el titulo de una ley publicada en Chile y quiero que la clasifiques si pertenece a la “agenda de seguridad”. Esto incluye leyes que abordan la situación de las víctimas y su protección, la delincuencia, tipifican delitos o cuasidelitos, regulan las policías, el control del orden público o en sentido amplio la seguridad nacional. Codifica la ley con el numero 1 si es parte de la Agenda de Seguridad, y con 0 si no. Solo usa números, sin puntos ni palabras"),
        list(role = "user", content = prompt)
      )
    ), auto_unbox = TRUE)
  )
  
  result <- httr::content(response, as = "text", encoding = "UTF-8")
  cat("Raw result:\n", result, "\n")
  
  parsed <- tryCatch({
    jsonlite::fromJSON(result, simplifyVector = FALSE)
  }, error = function(e) {
    message("Error al convertir JSON: ", e$message)
    return(NULL)
  })
  
  if (is.null(parsed) || is.null(parsed$choices)) {
    message("La respuesta no pudo ser procesada correctamente.")
    return(NULL)
  }
  
  return(parsed$choices[[1]]$message$content)
}


bills$security <- sapply(bills$Título, function(t) {                                        
  prompt <- paste0("El título de la ley es:", t)
  chatgpt_query(prompt)
})        

bills$id <- sub("-.*", "", bills$`N° Boletín`)

# Obtención de metadata desde el portal de datos abiertos legislativos del Senado

security_laws <- subset(bills, security == 1)

get_publication <- function(id) { 
  url= paste0("https://tramitacion.senado.cl/wspublico/tramitacion.php?boletin=", id)
  page <- tryCatch(read_xml(url), error = function(e) return(NA))
  if (is.na(page)) return(data.frame(publication = NA))
  date <- page %>% xml_find_first("//proyecto/descripcion/diariooficial") %>% html_text(trim = TRUE)
  return(data.frame(publication = date, stringsAsFactors = FALSE))
}     
publication <- map(security_laws$id, get_publication)
publication <- bind_rows(publication)
security_laws <- bind_cols(security_laws, publication)

get_origen <- function(id) { 
  url= paste0("https://tramitacion.senado.cl/wspublico/tramitacion.php?boletin=", id)
  page <- tryCatch(read_xml(url), error = function(e) return(NA))
  if (is.na(page)) return(data.frame(publication = NA))
  origen <- page %>% xml_find_first("//proyecto/descripcion/camara_origen") %>% html_text(trim = TRUE)
  return(data.frame(origen = origen, stringsAsFactors = FALSE))
}     
origen <- map(security_laws$id, get_origen)
origen <- bind_rows(origen)
security_laws <- bind_cols(security_laws, origen)

get_author <- function(id) { 
  url= paste0("https://tramitacion.senado.cl/wspublico/tramitacion.php?boletin=", id)
  page <- tryCatch(read_xml(url), error = function(e) return(NA))
  if (is.na(page)) return(data.frame(author_1 = "Ejecutivo"))
  authors <- page %>% xml_find_all("//proyecto/autores/autor") %>% html_text(trim = TRUE)
  if (length(authors) == 0) {return(data.frame(author_1 = "Ejecutivo"))}
  author_cols <- setNames(authors, paste0("author_", seq_along(authors)))
  return(as.data.frame(as.list(author_cols), stringsAsFactors = FALSE))
}   
authors <- map(security_laws$id, get_author)
security_laws <- bind_cols(security_laws, authors)

security_laws <- security_laws %>% mutate(publication_date = as.Date(publication, format ="%d/%m/%Y"),
                                          publication_my = format(publication_date, "%m/%Y"),
                                          publication_y = format(publication_date, "%Y"),
                                          introduction_date = as.Date(Fecha, format ="%d/%m/%Y"),
                                          introduction_my = format(introduction_date, "%m/%Y"),
                                          introduction_y = format(introduction_date, "%Y")
                                        )

security_laws$security <- as.numeric(security_laws$security)

security_laws$num_ley <- gsub("\\.", "", gsub(".*Ley N°\\s*", "", security_laws$Estado)) 

security_laws <- security_laws %>% select(-Estado)

security_laws_clean <- security_laws %>% distinct(Estado, .keep_all = TRUE)

write_xlsx(security_laws_clean, "data/procesados/pdls_seguridad_metadata_sin_revision")
