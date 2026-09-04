#---- Conexion0----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_0$ia,
  Pablo = lista_conexiones$conexiones_0$pablo
)
conexion0_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_0$ia,
  Juan = lista_conexiones$conexiones_0$juan
)
conexion0_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_0$juan,
  Pablo = lista_conexiones$conexiones_0$pablo
)
conexion0_pablo_juan <- kappa2(datos_raters)

conexion0_porcentaje_observado <- lista_conexiones$conexiones_0 %>% summarise(conexion = 0,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))


#---- Conexion1----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_1$ia,
  Pablo = lista_conexiones$conexiones_1$pablo
)
conexion1_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_1$ia,
  Juan = lista_conexiones$conexiones_1$juan
)
conexion1_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_1$juan,
  Pablo = lista_conexiones$conexiones_1$pablo
)
conexion1_pablo_juan <- kappa2(datos_raters)

conexion1_porcentaje_observado <- lista_conexiones$conexiones_1 %>% summarise(conexion = 1,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))



#---- Conexion2----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_2$ia,
  Pablo = lista_conexiones$conexiones_2$pablo
)
conexion2_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_2$ia,
  Juan = lista_conexiones$conexiones_2$juan
)
conexion2_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_2$juan,
  Pablo = lista_conexiones$conexiones_2$pablo
)
conexion2_pablo_juan <- kappa2(datos_raters)

conexion2_porcentaje_observado <- lista_conexiones$conexiones_2 %>% summarise(conexion = 2,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))

#---- Conexion3----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_3$ia,
  Pablo = lista_conexiones$conexiones_3$pablo
)
conexion3_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_3$ia,
  Juan = lista_conexiones$conexiones_3$juan
)
conexion3_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_3$juan,
  Pablo = lista_conexiones$conexiones_3$pablo
)
conexion3_pablo_juan <- kappa2(datos_raters)

conexion3_porcentaje_observado <- lista_conexiones$conexiones_3 %>% summarise(conexion = 3,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))


#---- Conexion4----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_4$ia,
  Pablo = lista_conexiones$conexiones_4$pablo
)
conexion4_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_4$ia,
  Juan = lista_conexiones$conexiones_4$juan
)
conexion4_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_4$juan,
  Pablo = lista_conexiones$conexiones_4$pablo
)
conexion4_pablo_juan <- kappa2(datos_raters)


conexion4_porcentaje_observado <- lista_conexiones$conexiones_4 %>% summarise(conexion = 4,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))

#---- Conexion5----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_5$ia,
  Pablo = lista_conexiones$conexiones_5$pablo
)
conexion5_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_5$ia,
  Juan = lista_conexiones$conexiones_5$juan
)
conexion5_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_5$juan,
  Pablo = lista_conexiones$conexiones_5$pablo
)
conexion5_pablo_juan <- kappa2(datos_raters)

conexion5_porcentaje_observado <- lista_conexiones$conexiones_5 %>% summarise(conexion = 5,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))


#---- Conexion6----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_6$ia,
  Pablo = lista_conexiones$conexiones_6$pablo
)
conexion6_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_6$ia,
  Juan = lista_conexiones$conexiones_6$juan
)
conexion6_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_6$juan,
  Pablo = lista_conexiones$conexiones_6$pablo
)
conexion6_pablo_juan <- kappa2(datos_raters)

conexion6_porcentaje_observado <- lista_conexiones$conexiones_6 %>% summarise(conexion = 6,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))


#---- Conexion7----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_7$ia,
  Pablo = lista_conexiones$conexiones_7$pablo
)
conexion7_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_7$ia,
  Juan = lista_conexiones$conexiones_7$juan
)
conexion7_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_7$juan,
  Pablo = lista_conexiones$conexiones_7$pablo
)
conexion7_pablo_juan <- kappa2(datos_raters)

conexion7_porcentaje_observado <- lista_conexiones$conexiones_7 %>% summarise(conexion = 7,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))


#---- Conexion8----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_8$ia,
  Pablo = lista_conexiones$conexiones_8$pablo
)
conexion8_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_8$ia,
  Juan = lista_conexiones$conexiones_8$juan
)
conexion8_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_8$juan,
  Pablo = lista_conexiones$conexiones_8$pablo
)
conexion8_pablo_juan <- kappa2(datos_raters)

conexion8_porcentaje_observado <- lista_conexiones$conexiones_8 %>% summarise(conexion = 8,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))


#---- Conexion9----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_9$ia,
  Pablo = lista_conexiones$conexiones_9$pablo
)
conexion9_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_9$ia,
  Juan = lista_conexiones$conexiones_9$juan
)
conexion9_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_9$juan,
  Pablo = lista_conexiones$conexiones_9$pablo
)
conexion9_pablo_juan <- kappa2(datos_raters)

conexion9_porcentaje_observado <- lista_conexiones$conexiones_9 %>% summarise(conexion = 9,
                                                                              mean_ia = mean(ia),
                                                                              mean_juan = mean(juan),
                                                                              mean_pablo = mean(pablo))


#---- Conexion10----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_10$ia,
  Pablo = lista_conexiones$conexiones_10$pablo
)
conexion10_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_10$ia,
  Juan = lista_conexiones$conexiones_10$juan
)
conexion10_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_10$juan,
  Pablo = lista_conexiones$conexiones_10$pablo
)
conexion10_pablo_juan <- kappa2(datos_raters)


conexion10_porcentaje_observado <- lista_conexiones$conexiones_10 %>% summarise(conexion = 10,
                                                                                mean_ia = mean(ia),
                                                                                mean_juan = mean(juan),
                                                                                mean_pablo = mean(pablo))

#---- Conexion11----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_11$ia,
  Pablo = lista_conexiones$conexiones_11$pablo
)
conexion11_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_11$ia,
  Juan = lista_conexiones$conexiones_11$juan
)
conexion11_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_11$juan,
  Pablo = lista_conexiones$conexiones_11$pablo
)
conexion11_pablo_juan <- kappa2(datos_raters)


conexion11_porcentaje_observado <- lista_conexiones$conexiones_11 %>% summarise(conexion = 11,
                                                                                mean_ia = mean(ia),
                                                                                mean_juan = mean(juan),
                                                                                mean_pablo = mean(pablo))

#---- Conexion12----

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_12$ia,
  Pablo = lista_conexiones$conexiones_12$pablo
)
conexion12_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_conexiones$conexiones_12$ia,
  Juan = lista_conexiones$conexiones_12$juan
)
conexion12_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_conexiones$conexiones_12$juan,
  Pablo = lista_conexiones$conexiones_12$pablo
)
conexion12_pablo_juan <- kappa2(datos_raters)

conexion12_porcentaje_observado <- lista_conexiones$conexiones_12 %>% summarise(conexion = 12,
                                                                                mean_ia = mean(ia),
                                                                                mean_juan = mean(juan),
                                                                                mean_pablo = mean(pablo))
