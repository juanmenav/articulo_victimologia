library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(writexl)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(here)

setwd(here())

datos <- read_xlsx("datos/output/bbdd_final.xlsx") %>% mutate(mujer = as.numeric(mujer)) %>% subset(!is.na(conexion))

#---- MCA clsuterizado ----

# Tratamiento de los datos para MCA

cols_multi <- c("victima", "enfoques", "tipo", "conexion", "practica", "valoracion")

hacer_wide_multi <- function(data, var) {
  data %>%
    select(ley_id, all_of(var)) %>%
    rename(respuesta = all_of(var)) %>%
    mutate(respuesta = as.character(respuesta)) %>%
    separate_rows(respuesta, sep = ";") %>%
    mutate(respuesta = str_trim(respuesta)) %>%
    distinct(ley_id, respuesta) %>%
    mutate(
      valor = 1L,
      nombre_columna = paste0(var, "_", respuesta)
    ) %>%
    select(ley_id, nombre_columna, valor) %>%
    pivot_wider(
      names_from = nombre_columna,
      values_from = valor,
      values_fill = 0
    )
}

multi_wide <- cols_multi %>%
  map(~ hacer_wide_multi(datos, .x)) %>%
  reduce(full_join, by = "ley_id")

datos_wide <- datos %>%
  select(-all_of(cols_multi)) %>%
  left_join(multi_wide, by = "ley_id") %>%
  mutate(
    across(
      matches("^(victima|enfoques|tipo|conexion|practica|valoracion)_"),
      ~ replace_na(.x, 0L)
    )
  )

cols_dummy <- names(datos_wide) %>%
  str_subset("^(victima|enfoques|tipo|conexion|practica|valoracion)_")

orden_dummy <- tibble(col = cols_dummy) %>%
  extract(
    col,
    into = c("variable", "categoria"),
    regex = "^(victima|enfoques|tipo|conexion|practica|valoracion)_(\\d+)$",
    remove = FALSE,
    convert = TRUE
  ) %>%
  arrange(factor(variable, levels = cols_multi), categoria) %>%
  pull(col)

datos_wide <- datos_wide %>%
  select(
    -all_of(cols_dummy),
    all_of(orden_dummy)
  )

varianza <- datos_wide %>% select(3, 9:48) %>%
  subset(victima_0 != 1) %>%
  select(where(~ n_distinct(.x) > 1))

write_xlsx(varianza, "datos/output/matriz_mca.xlsx")

varianza_mca <- varianza %>% select(-1) %>%
  filter(rowSums(across(everything()), na.rm = TRUE) > 0) %>%
  select(where(~ n_distinct(.x, na.rm = TRUE) > 1)) %>%
  mutate(across(everything(), as.factor)) %>%
  droplevels()

row.names(varianza_mca) <- varianza$ley_id

res_mca <- MCA(
  as.data.frame(varianza_mca),
  graph = FALSE
)

contribuciones <- as.data.frame(res_mca$var$contrib)
contribuciones$variable <- row.names(contribuciones)

write_xlsx(contribuciones , "datos/output/contribuciones_mca.xlsx")

coordenadas <- as.data.frame(res_mca$ind$coord)
coordenadas$ley_id <- row.names(varianza_mca)

write_xlsx(coordenadas , "datos/output/coordenadas_mca.xlsx")

res_hcpc <- HCPC(
  res_mca,
  nb.clust = 5,     
  consol = TRUE,    
  graph = FALSE
)

varianza_mca$cluster_hcpc <- as.factor(res_hcpc$data.clust$clust) 

varianza$cluster_hcpc <- as.factor(res_hcpc$data.clust$clust) 

clusters <- varianza %>% select(1, 4, 41)

textos <- datos %>% subset(victima != 0, select = c(3, 8))

cluster_textos <- left_join(clusters, textos, by = "ley_id")

write_xlsx(cluster_textos, "datos/output/cluster_textos.xlsx")

varianza_mca <- varianza_mca %>% mutate(cluster = case_when(cluster_hcpc == 1 ~ "Retributivismo concreto de la víctima inocente",
                                                            cluster_hcpc == 2 ~ "Restaurativismo vulnerable e innovador",
                                                            cluster_hcpc == 3 ~ "Punitivismo abstracto de la víctima vulnerable",
                                                            cluster_hcpc == 4 ~ "Legalismo securitario con baja valoración victimológica",
                                                            cluster_hcpc == 5 ~ "Securitización difusa sin enfoque victimológico claro")
)

descripcion_clusters <- varianza_mca %>%
  mutate(across(1:39, ~ as.numeric(as.character(.x)))) %>%
  group_by(cluster_hcpc) %>%
  summarise(n = n(),
            across(1:39, mean))

write_xlsx(descripcion_clusters, "datos/output/descripcion_clusters.xlsx")

fviz_screeplot(res_mca, 
               addlabels = TRUE,
               main = "",
               xlab = "Dimensiones",
               ylab = "Porcentaje de la variación explicada"
               ) +
  theme_classic(base_family = "serif")+
      theme(axis.title = element_text(face = "bold"))

ggsave(
  "mca_inercia.png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)


p <- fviz_mca_ind(
  res_mca,
  geom = "point",
  habillage = varianza_mca$cluster,
  addEllipses = TRUE,
  alpha.ind = 0.3,
  repel = FALSE,  
  title = "",
) +
  labs(colour = "Cluster") +
  guides(fill = "none")+
  theme_classic(
    base_family = "serif",
    base_size = 14
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.line = element_line(colour = "black")
  )
      
ggsave(
  "mca_hcpc.png",
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)
