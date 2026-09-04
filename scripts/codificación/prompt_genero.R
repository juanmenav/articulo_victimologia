prompt_instruccion <- "A partir de lo anterior, quiero que analices el contenido del discurso y determines si las víctimas a las que refeire el hablante son mujeres o no. Si lo son, quiero que lo  marques con un 1, de lo  contrario con un 0.

Genera un archivo JSON con los resultados."

json_estructura <- 'Ahora responde exclusivamente en formato JSON válido. Tu respuesta debe ser ÚNICAMENTE el objeto JSON, comenzando con "{" y terminando con "}". 
Estructura requerida:
{
  "id":"ID del párrafo",
  "mujer": "0|1"
}'
  