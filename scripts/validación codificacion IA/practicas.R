#---- practicas0----

datos_raters <- data.frame(
  IA = lista_practicas$practicas_0$ia,
  Pablo = lista_practicas$practicas_0$pablo
)
practicas0_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_practicas$practicas_0$ia,
  Juan = lista_practicas$practicas_0$juan
)
practicas0_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_practicas$practicas_0$juan,
  Pablo = lista_practicas$practicas_0$pablo
)
practicas0_pablo_juan <- kappa2(datos_raters)

practicas0_porcentaje_observado <- lista_practicas$practicas_0 %>% summarise(practicas = 0,
                                                                             mean_ia = mean(ia),
                                                                             mean_juan = mean(juan),
                                                                             mean_pablo = mean(pablo))


#---- practicas1----

datos_raters <- data.frame(
  IA = lista_practicas$practicas_1$ia,
  Pablo = lista_practicas$practicas_1$pablo
)
practicas1_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_practicas$practicas_1$ia,
  Juan = lista_practicas$practicas_1$juan
)
practicas1_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_practicas$practicas_1$juan,
  Pablo = lista_practicas$practicas_1$pablo
)
practicas1_pablo_juan <- kappa2(datos_raters)

practicas1_porcentaje_observado <- lista_practicas$practicas_1 %>% summarise(practicas = 1,
                                                                             mean_ia = mean(ia),
                                                                             mean_juan = mean(juan),
                                                                             mean_pablo = mean(pablo))



#---- practicas2----

datos_raters <- data.frame(
  IA = lista_practicas$practicas_2$ia,
  Pablo = lista_practicas$practicas_2$pablo
)
practicas2_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_practicas$practicas_2$ia,
  Juan = lista_practicas$practicas_2$juan
)
practicas2_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_practicas$practicas_2$juan,
  Pablo = lista_practicas$practicas_2$pablo
)
practicas2_pablo_juan <- kappa2(datos_raters)

practicas2_porcentaje_observado <- lista_practicas$practicas_2 %>% summarise(practicas = 2,
                                                                             mean_ia = mean(ia),
                                                                             mean_juan = mean(juan),
                                                                             mean_pablo = mean(pablo))

#---- practicas3----

datos_raters <- data.frame(
  IA = lista_practicas$practicas_3$ia,
  Pablo = lista_practicas$practicas_3$pablo
)
practicas3_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_practicas$practicas_3$ia,
  Juan = lista_practicas$practicas_3$juan
)
practicas3_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_practicas$practicas_3$juan,
  Pablo = lista_practicas$practicas_3$pablo
)
practicas3_pablo_juan <- kappa2(datos_raters)

practicas3_porcentaje_observado <- lista_practicas$practicas_3 %>% summarise(practicas = 3,
                                                                             mean_ia = mean(ia),
                                                                             mean_juan = mean(juan),
                                                                             mean_pablo = mean(pablo))


#---- practicas4----

datos_raters <- data.frame(
  IA = lista_practicas$practicas_4$ia,
  Pablo = lista_practicas$practicas_4$pablo
)
practicas4_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_practicas$practicas_4$ia,
  Juan = lista_practicas$practicas_4$juan
)
practicas4_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_practicas$practicas_4$juan,
  Pablo = lista_practicas$practicas_4$pablo
)
practicas4_pablo_juan <- kappa2(datos_raters)


practicas4_porcentaje_observado <- lista_practicas$practicas_4 %>% summarise(practicas = 4,
                                                                             mean_ia = mean(ia),
                                                                             mean_juan = mean(juan),
                                                                             mean_pablo = mean(pablo))

#---- practicas5----

datos_raters <- data.frame(
  IA = lista_practicas$practicas_5$ia,
  Pablo = lista_practicas$practicas_5$pablo
)
practicas5_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_practicas$practicas_5$ia,
  Juan = lista_practicas$practicas_5$juan
)
practicas5_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_practicas$practicas_5$juan,
  Pablo = lista_practicas$practicas_5$pablo
)
practicas5_pablo_juan <- kappa2(datos_raters)

practicas5_porcentaje_observado <- lista_practicas$practicas_5 %>% summarise(practicas = 5,
                                                                             mean_ia = mean(ia),
                                                                             mean_juan = mean(juan),
                                                                             mean_pablo = mean(pablo))


#---- practicas6----

datos_raters <- data.frame(
  IA = lista_practicas$practicas_6$ia,
  Pablo = lista_practicas$practicas_6$pablo
)
practicas6_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_practicas$practicas_6$ia,
  Juan = lista_practicas$practicas_6$juan
)
practicas6_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_practicas$practicas_6$juan,
  Pablo = lista_practicas$practicas_6$pablo
)
practicas6_pablo_juan <- kappa2(datos_raters)

practicas6_porcentaje_observado <- lista_practicas$practicas_6 %>% summarise(practicas = 6,
                                                                             mean_ia = mean(ia),
                                                                             mean_juan = mean(juan),
                                                                             mean_pablo = mean(pablo))


#---- practicas7----

datos_raters <- data.frame(
  IA = lista_practicas$practicas_7$ia,
  Pablo = lista_practicas$practicas_7$pablo
)
practicas7_pablo_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  IA = lista_practicas$practicas_7$ia,
  Juan = lista_practicas$practicas_7$juan
)
practicas7_juan_ia <- kappa2(datos_raters)

datos_raters <- data.frame(
  Juan = lista_practicas$practicas_7$juan,
  Pablo = lista_practicas$practicas_7$pablo
)
practicas7_pablo_juan <- kappa2(datos_raters)

practicas7_porcentaje_observado <- lista_practicas$practicas_7 %>% summarise(practicas = 7,
                                                                             mean_ia = mean(ia),
                                                                             mean_juan = mean(juan),
                                                                             mean_pablo = mean(pablo))


