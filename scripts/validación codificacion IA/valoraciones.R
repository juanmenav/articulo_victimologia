#---- valoraciones0----

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_0$ia,
  Pablo = lista_valoraciones$valoraciones_0$pablo
)
valoraciones0_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_0$ia,
  Juan = lista_valoraciones$valoraciones_0$juan
)
valoraciones0_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_valoraciones$valoraciones_0$juan,
  Pablo = lista_valoraciones$valoraciones_0$pablo
)
valoraciones0_pablo_juan <- kappa2(datos_raters)

valoraciones0_porcentaje_observado <- lista_valoraciones$valoraciones_0 %>% summarise(valoraciones = 0,
                                                                                      mean_ia = mean(ia),
                                                                                      mean_juan = mean(juan),
                                                                                      mean_pablo = mean(pablo))


#---- valoraciones1----

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_1$ia,
  Pablo = lista_valoraciones$valoraciones_1$pablo
)
valoraciones1_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_1$ia,
  Juan = lista_valoraciones$valoraciones_1$juan
)
valoraciones1_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_valoraciones$valoraciones_1$juan,
  Pablo = lista_valoraciones$valoraciones_1$pablo
)
valoraciones1_pablo_juan <- kappa2(datos_raters)

valoraciones1_porcentaje_observado <- lista_valoraciones$valoraciones_1 %>% summarise(valoraciones = 1,
                                                                                      mean_ia = mean(ia),
                                                                                      mean_juan = mean(juan),
                                                                                      mean_pablo = mean(pablo))



#---- valoraciones2----

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_2$ia,
  Pablo = lista_valoraciones$valoraciones_2$pablo
)
valoraciones2_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_2$ia,
  Juan = lista_valoraciones$valoraciones_2$juan
)
valoraciones2_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_valoraciones$valoraciones_2$juan,
  Pablo = lista_valoraciones$valoraciones_2$pablo
)
valoraciones2_pablo_juan <- kappa2(datos_raters)

valoraciones2_porcentaje_observado <- lista_valoraciones$valoraciones_2 %>% summarise(valoraciones = 2,
                                                                                      mean_ia = mean(ia),
                                                                                      mean_juan = mean(juan),
                                                                                      mean_pablo = mean(pablo))

#---- valoraciones3----

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_3$ia,
  Pablo = lista_valoraciones$valoraciones_3$pablo
)
valoraciones3_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_3$ia,
  Juan = lista_valoraciones$valoraciones_3$juan
)
valoraciones3_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_valoraciones$valoraciones_3$juan,
  Pablo = lista_valoraciones$valoraciones_3$pablo
)
valoraciones3_pablo_juan <- kappa2(datos_raters)

valoraciones3_porcentaje_observado <- lista_valoraciones$valoraciones_3 %>% summarise(valoraciones = 3,
                                                                                      mean_ia = mean(ia),
                                                                                      mean_juan = mean(juan),
                                                                                      mean_pablo = mean(pablo))


#---- valoraciones4----

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_4$ia,
  Pablo = lista_valoraciones$valoraciones_4$pablo
)
valoraciones4_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_4$ia,
  Juan = lista_valoraciones$valoraciones_4$juan
)
valoraciones4_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_valoraciones$valoraciones_4$juan,
  Pablo = lista_valoraciones$valoraciones_4$pablo
)
valoraciones4_pablo_juan <- kappa2(datos_raters)


valoraciones4_porcentaje_observado <- lista_valoraciones$valoraciones_4 %>% summarise(valoraciones = 4,
                                                                                      mean_ia = mean(ia),
                                                                                      mean_juan = mean(juan),
                                                                                      mean_pablo = mean(pablo))

#---- valoraciones5----

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_5$ia,
  Pablo = lista_valoraciones$valoraciones_5$pablo
)
valoraciones5_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_5$ia,
  Juan = lista_valoraciones$valoraciones_5$juan
)
valoraciones5_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_valoraciones$valoraciones_5$juan,
  Pablo = lista_valoraciones$valoraciones_5$pablo
)
valoraciones5_pablo_juan <- kappa2(datos_raters)

valoraciones5_porcentaje_observado <- lista_valoraciones$valoraciones_5 %>% summarise(valoraciones = 5,
                                                                                      mean_ia = mean(ia),
                                                                                      mean_juan = mean(juan),
                                                                                      mean_pablo = mean(pablo))


#---- valoraciones6----

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_6$ia,
  Pablo = lista_valoraciones$valoraciones_6$pablo
)
valoraciones6_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_6$ia,
  Juan = lista_valoraciones$valoraciones_6$juan
)
valoraciones6_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_valoraciones$valoraciones_6$juan,
  Pablo = lista_valoraciones$valoraciones_6$pablo
)
valoraciones6_pablo_juan <- kappa2(datos_raters)

valoraciones6_porcentaje_observado <- lista_valoraciones$valoraciones_6 %>% summarise(valoraciones = 6,
                                                                                      mean_ia = mean(ia),
                                                                                      mean_juan = mean(juan),
                                                                                      mean_pablo = mean(pablo))


#---- valoraciones7----

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_7$ia,
  Pablo = lista_valoraciones$valoraciones_7$pablo
)
valoraciones7_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_valoraciones$valoraciones_7$ia,
  Juan = lista_valoraciones$valoraciones_7$juan
)
valoraciones7_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_valoraciones$valoraciones_7$juan,
  Pablo = lista_valoraciones$valoraciones_7$pablo
)
valoraciones7_pablo_juan <- kappa2(datos_raters)

valoraciones7_porcentaje_observado <- lista_valoraciones$valoraciones_7 %>% summarise(valoraciones = 7,
                                                                                      mean_ia = mean(ia),
                                                                                      mean_juan = mean(juan),
                                                                                      mean_pablo = mean(pablo))




