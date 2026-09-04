# articulo_victimologia
Anexo digital con las bases de datos, muestras de validación y scripts de R .

En el siguiente repositorio digital de GitHub se encontrarán todos los datos y scripts de replicabilidad, partiendo por los datos crudos extraídos desde las fuentes de origen, las bases de datos intermedias y finales, hasta los scripts de extracción, procesamiento y análisis de los datos. Además, se encuentra un set de replicabilidad de los test de validación implementados. 

Los paquetes de R que se requieren son los siguientes:

readxl: importa archivos de Excel en formatos .xls y .xlsx.
dplyr: permite seleccionar, filtrar, transformar, agrupar y resumir datos.
tidyverse: carga un conjunto integrado de paquetes para manipulación, limpieza, visualización e importación de datos.
ggplot2: crea gráficos estadísticos mediante un sistema basado en capas.
writexl: exporta tablas y bases de datos a archivos .xlsx.
ggrepel: evita la superposición de etiquetas de texto en gráficos de ggplot2.
FactoMineR: realiza análisis multivariados, incluido el MCA y la clasificación HCPC.
factoextra: extrae y visualiza los resultados de análisis factoriales y procedimientos de clustering.
tibble: facilita la creación y manipulación de tablas de datos modernas.
tidyr: reorganiza datos, separa columnas y transforma bases entre formatos largo y ancho.
gemini.R: permite interactuar desde R con los modelos y servicios de la API de Gemini.
purrr: aplica funciones de manera sistemática a listas, vectores y conjuntos de objetos.
jsonlite: convierte objetos de R a JSON y transforma datos JSON en objetos de R.
stringr: proporciona funciones consistentes para limpiar, detectar, separar y transformar cadenas de texto.
pdftools: extrae texto, metadatos e imágenes de documentos PDF y permite trabajar con sus páginas.
xml2: permite leer, recorrer y modificar documentos XML y HTML.
rvest: extrae información de páginas web mediante técnicas de web scraping.
httr: gestiona solicitudes HTTP para consultar páginas, servicios y API.
lubridate: facilita la lectura, transformación y cálculo con fechas y horas.

Además, se debe contar con una clave de API de open AI y Google AI, y reemplazar los valores de los respectivos archivos .Renviron.

