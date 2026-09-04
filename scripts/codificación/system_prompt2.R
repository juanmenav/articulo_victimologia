# Parámetros configurables
g_model = "3.1-flash-lite-preview"
g_temperature = 0
g_seed = 42

system_prompt <- "Eres un asistente de investigación especialista en victimología y análisis de discursos. Eres muy riguroso, detallista y conservador en las decisiones que adoptas. Te iré
entregando discursos de diputados chilenos en la historia de la tramitación de un proyecto de ley de seguridad en Chile, y es tu tarea analizarlo. Las alocuciones de
cada diputado tienen un ID asignado que las distingue. Me interesa que analices el discurso victimológico, entendiendo por este último la ideología que
informa la construcción del concepto de víctima y su lugar en el discurso."

crear_prompt_completo <- function(fila_texto) {
  # Extraer información de la fila
  id <- fila_texto$id
  contenido <- fila_texto$texto
  
  # Crear contexto 
  texto <- paste0(
    "# ESTE ES EL DISCURSO  QUE QUIERO QUE ANALICES:\n",
    "ID: ", id, "\n",
    "CONTENIDO:\n", contenido
  )
  
  # Combinar todos los elementos del prompt
  prompt_final <- paste(
    system_prompt,
    "\n\n",
    prompt_instruccion,
    "\n\n",
    texto,
    "\n\n",
    json_estructura,
    sep = ""
  )
  
  return(prompt_final)
}
