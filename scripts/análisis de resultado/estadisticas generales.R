library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(here)

setwd(here())

datos <- read_xlsx("datos/output/bbdd_final.xlsx")

#--- estadisticas descriptivas contenido ---- 

victima <- datos %>% 
  mutate(victima = ifelse(victima != 0, 1, 0)) %>%
  group_by(año) %>% 
  summarise(n = n(),
            "%victima" = mean(victima))

tipo_victima <- datos %>% 
  mutate(concreta = ifelse(victima == 2, 1, 0),
         victima = ifelse(victima != 0, 1, 0)
         ) %>%
  subset(victima == 1) %>%
  group_by(año) %>% 
  summarise(concreta = mean(concreta),
            mujer = mean(as.numeric(mujer))
            )

enfoque1 <- datos %>% 
  group_by(año, enfoques) %>%
  summarise(n = n())

enfoque2 <- datos %>% 
  group_by(año) %>%
  summarise(total = n()) %>% 
  left_join(enfoque1, by = "año", relationship = "one-to-many") %>%
  group_by(año, enfoques) %>%
  summarise(enfoque = n/total) %>%
  pivot_wider(names_from = enfoques, values_from = enfoque, names_prefix = "enfoque") %>%
  mutate(enfoque1 = ifelse(is.na(enfoque1), 0, enfoque1),
         enfoque2 = ifelse(is.na(enfoque2), 0, enfoque2),
         enfoque3 = ifelse(is.na(enfoque3), 0, enfoque3)) 

tipo1 <- datos %>% 
  group_by(año, tipo) %>%
  summarise(n = n())

tipo2 <- datos %>% 
  group_by(año) %>%
  summarise(total = n()) %>% 
  left_join(tipo1, by = "año", relationship = "one-to-many") %>%
  group_by(año, tipo) %>%
  summarise(tipos = n/total) %>%
  pivot_wider(names_from = tipo, values_from = tipos, names_prefix = "tipo") %>%
  mutate(tipo2 = ifelse(is.na(tipo2), 0, tipo2)) 

tabla1 <- left_join(victima, tipo_victima, by = "año") %>%
  left_join(enfoque2, by = "año") %>%
  left_join(tipo2, by = "año") 

write_xlsx(tabla1, "datos/output/anexo3tabla1.xlsx")

#--- estadisticas descriptivas conexiones ---- 

tabla2 <- datos %>% subset(victima != 0) %>%
  mutate(conexion0  = ifelse(grepl("0",  conexion), 1, 0),
    conexion1  = ifelse(grepl("1",  conexion), 1, 0),
    conexion2  = ifelse(grepl("2",  conexion), 1, 0),
    conexion3  = ifelse(grepl("3",  conexion), 1, 0),
    conexion4  = ifelse(grepl("4",  conexion), 1, 0),
    conexion5  = ifelse(grepl("5",  conexion), 1, 0),
    conexion6  = ifelse(grepl("6",  conexion), 1, 0),
    conexion7  = ifelse(grepl("7",  conexion), 1, 0),
    conexion8  = ifelse(grepl("8",  conexion), 1, 0),
    conexion9  = ifelse(grepl("9",  conexion), 1, 0),
    conexion10 = ifelse(grepl("10", conexion), 1, 0),
    conexion11 = ifelse(grepl("11", conexion), 1, 0),
    conexion12 = ifelse(grepl("12", conexion), 1, 0)
  ) %>%
  group_by(año) %>%
  summarise(
    n = n(),
    conexion0  = mean(conexion0, na.rm = TRUE),
    conexion1  = mean(conexion1, na.rm = TRUE),
    conexion2  = mean(conexion2, na.rm = TRUE),
    conexion3  = mean(conexion3, na.rm = TRUE),
    conexion4  = mean(conexion4, na.rm = TRUE),
    conexion5  = mean(conexion5, na.rm = TRUE),
    conexion6  = mean(conexion6, na.rm = TRUE),
    conexion7  = mean(conexion7, na.rm = TRUE),
    conexion8  = mean(conexion8, na.rm = TRUE),
    conexion9  = mean(conexion9, na.rm = TRUE),
    conexion10 = mean(conexion10, na.rm = TRUE),
    conexion11 = mean(conexion11, na.rm = TRUE),
    conexion12 = mean(conexion12, na.rm = TRUE)
  )

totales <- datos %>% subset(victima != 0) %>%
  mutate(conexion0  = ifelse(grepl("0",  conexion), 1, 0),
         conexion1  = ifelse(grepl("1",  conexion), 1, 0),
         conexion2  = ifelse(grepl("2",  conexion), 1, 0),
         conexion3  = ifelse(grepl("3",  conexion), 1, 0),
         conexion4  = ifelse(grepl("4",  conexion), 1, 0),
         conexion5  = ifelse(grepl("5",  conexion), 1, 0),
         conexion6  = ifelse(grepl("6",  conexion), 1, 0),
         conexion7  = ifelse(grepl("7",  conexion), 1, 0),
         conexion8  = ifelse(grepl("8",  conexion), 1, 0),
         conexion9  = ifelse(grepl("9",  conexion), 1, 0),
         conexion10 = ifelse(grepl("10", conexion), 1, 0),
         conexion11 = ifelse(grepl("11", conexion), 1, 0),
         conexion12 = ifelse(grepl("12", conexion), 1, 0)
  ) %>%
  summarise(
    n = n(),
    conexion0  = mean(conexion0,  na.rm = TRUE),
    conexion1  = mean(conexion1,  na.rm = TRUE),
    conexion2  = mean(conexion2,  na.rm = TRUE),
    conexion3  = mean(conexion3,  na.rm = TRUE),
    conexion4  = mean(conexion4,  na.rm = TRUE),
    conexion5  = mean(conexion5,  na.rm = TRUE),
    conexion6  = mean(conexion6,  na.rm = TRUE),
    conexion7  = mean(conexion7,  na.rm = TRUE),
    conexion8  = mean(conexion8,  na.rm = TRUE),
    conexion9  = mean(conexion9,  na.rm = TRUE),
    conexion10 = mean(conexion10, na.rm = TRUE),
    conexion11 = mean(conexion11, na.rm = TRUE),
    conexion12 = mean(conexion12, na.rm = TRUE)
  ) %>%
  mutate(año = "Total") %>%
  relocate(año)

tabla2 <- rbind(tabla2, totales)

write_xlsx(tabla2, "datos/output/anexo3tabla2.xlsx")


#--- estadisticas descriptivas practicas ---- 

tabla3 <- datos %>% subset(victima != 0) %>%
  mutate(practica0  = ifelse(grepl("0",  practica), 1, 0),
         practica1  = ifelse(grepl("1",  practica), 1, 0),
         practica2  = ifelse(grepl("2",  practica), 1, 0),
         practica3  = ifelse(grepl("3",  practica), 1, 0),
         practica4  = ifelse(grepl("4",  practica), 1, 0),
         practica5  = ifelse(grepl("5",  practica), 1, 0),
         practica6  = ifelse(grepl("6",  practica), 1, 0),
         practica7  = ifelse(grepl("7",  practica), 1, 0)
         ) %>%
  group_by(año) %>%
  summarise(
    n = n(),
    practica0  = mean(practica0, na.rm = TRUE),
    practica1  = mean(practica1, na.rm = TRUE),
    practica2  = mean(practica2, na.rm = TRUE),
    practica3  = mean(practica3, na.rm = TRUE),
    practica4  = mean(practica4, na.rm = TRUE),
    practica5  = mean(practica5, na.rm = TRUE),
    practica6  = mean(practica6, na.rm = TRUE),
    practica7  = mean(practica7, na.rm = TRUE)
  )

totales <- datos %>% subset(victima != 0) %>%
  mutate(practica0  = ifelse(grepl("0",  practica), 1, 0),
         practica1  = ifelse(grepl("1",  practica), 1, 0),
         practica2  = ifelse(grepl("2",  practica), 1, 0),
         practica3  = ifelse(grepl("3",  practica), 1, 0),
         practica4  = ifelse(grepl("4",  practica), 1, 0),
         practica5  = ifelse(grepl("5",  practica), 1, 0),
         practica6  = ifelse(grepl("6",  practica), 1, 0),
         practica7  = ifelse(grepl("7",  practica), 1, 0)
         ) %>%
  summarise(
    n = n(),
   practica0  = mean(practica0, na.rm = TRUE),
   practica1  = mean(practica1, na.rm = TRUE),
   practica2  = mean(practica2, na.rm = TRUE),
   practica3  = mean(practica3, na.rm = TRUE),
   practica4  = mean(practica4, na.rm = TRUE),
   practica5  = mean(practica5, na.rm = TRUE),
   practica6  = mean(practica6, na.rm = TRUE),
   practica7  = mean(practica7, na.rm = TRUE)
  ) %>% mutate(año = "Total") %>%
  relocate(año)

tabla3 <- rbind(tabla3, totales)

write_xlsx(tabla3, "datos/output/anexo3tabla3.xlsx")


#--- estadisticas descriptivas valoraciones ---- 

tabla4 <- datos %>% subset(victima != 0) %>%
  mutate(valoracion0  = ifelse(grepl("0",  valoracion), 1, 0),
         valoracion1  = ifelse(grepl("1",  valoracion), 1, 0),
         valoracion2  = ifelse(grepl("2",  valoracion), 1, 0),
         valoracion3  = ifelse(grepl("3",  valoracion), 1, 0),
         valoracion4  = ifelse(grepl("4",  valoracion), 1, 0),
         valoracion5  = ifelse(grepl("5",  valoracion), 1, 0),
         valoracion6  = ifelse(grepl("6",  valoracion), 1, 0),
         valoracion7  = ifelse(grepl("7",  valoracion), 1, 0)
  ) %>%
  group_by(año) %>%
  summarise(
    n = n(),
    valoracion0  = mean(valoracion0, na.rm = TRUE),
    valoracion1  = mean(valoracion1, na.rm = TRUE),
    valoracion2  = mean(valoracion2, na.rm = TRUE),
    valoracion3  = mean(valoracion3, na.rm = TRUE),
    valoracion4  = mean(valoracion4, na.rm = TRUE),
    valoracion5  = mean(valoracion5, na.rm = TRUE),
    valoracion6  = mean(valoracion6, na.rm = TRUE),
    valoracion7  = mean(valoracion7, na.rm = TRUE)
  )

totales <- tabla4  %>%
  summarise(
    n = n(),
    valoracion0  = mean(valoracion0, na.rm = TRUE),
    valoracion1  = mean(valoracion1, na.rm = TRUE),
    valoracion2  = mean(valoracion2, na.rm = TRUE),
    valoracion3  = mean(valoracion3, na.rm = TRUE),
    valoracion4  = mean(valoracion4, na.rm = TRUE),
    valoracion5  = mean(valoracion5, na.rm = TRUE),
    valoracion6  = mean(valoracion6, na.rm = TRUE),
    valoracion7  = mean(valoracion7, na.rm = TRUE)
  ) %>% mutate(año = "Total") %>%
  relocate(año)

tabla4 <- rbind(tabla4, totales)

write_xlsx(tabla4, "datos/output/anexo3tabla4.xlsx")


