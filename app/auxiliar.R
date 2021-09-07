library(dplyr)

res_finales <- read.csv("ResultsReadyToPlot.csv", encoding = "UTF-8")

# Arreglos a la base de datos,
# servirá como insumo en App Shiny

# Arreglos iniciales

res_finales <- res_finales %>%
                mutate(n = case_when(grupo == "Noveno" ~ 26,
                                     grupo == "Décimo" ~ 30,
                                     grupo == "Undécimo" ~ 25,
                                     grupo == "Global" ~ 26+30+25,
                                     TRUE ~ 0),
                       metodo = case_when(grupo %in% c("Noveno", "Décimo", "Undécimo") ~ "t-student",
                                          grupo == "Global" ~ "normal"),
                       se = case_when(metodo == "t-student" ~ B / qt(0.975, n - 1),
                                      metodo == "normal" ~ B / qnorm(0.975),
                                      TRUE ~ 0)) %>%
                select(grupo, parametro, est, n, metodo, se)

# Labels para los parámetros

# Arreglo
level1 <- "Proporción de estudiantes satisfechos con el modelo de alternacia"
level2 <- "Promedio de horas semanales que invierte estudiando"
level3 <- "Proporción de estudiantes que realizan actividades extracurriculares"
level4 <- "Promedio de horas semanales dedicadas a leer material diferente al académico"
level6 <- "Proporción de estudiantes que consideró útil la virtualidad en su proceso de aprendizaje"

level1_inp <- "Satisfacción con el modelo de alternancia"
level2_inp <- "Horas semanales invertidas estudiando"
level3_inp <- "Actividades extracurriculares"
level4_inp <- "Horas de lectura de material diferente al académico"
level6_inp <- "Utilidad de la virtualidad"

res_finales <- res_finales %>%
                mutate(label_parametro = case_when(parametro == "P_1" ~ level1,
                                                   parametro == "mu_2" ~ level2,
                                                   parametro == "P_3" ~ level3,
                                                   parametro == "mu_4" ~  level4,
                                                   parametro == "P_6" ~ level6,
                                                   TRUE ~  parametro),
                       label_parametro_input = case_when(parametro == "P_1" ~ level1_inp,
                                                         parametro == "mu_2" ~ level2_inp,
                                                         parametro == "P_3" ~ level3_inp,
                                                         parametro == "mu_4" ~  level4_inp,
                                                         parametro == "P_6" ~ level6_inp,
                                                         TRUE ~  parametro))
  


#write.csv(res_finales, "BD_app.csv", row.names = F)

