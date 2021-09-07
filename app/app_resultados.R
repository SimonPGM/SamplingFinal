#### PRELIMINARES ####

library(dplyr)
library(ggplot2)
datos_app <- read.csv("BD_app.csv")

ui <- fluidPage(
  
  titlePanel("Presentación interactiva de resultados"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "grado", 
                  label = "Grado", 
                  choices = c("Noveno", "Décimo", "Undécimo", "Global")),
      
      selectInput(inputId = "parametro", 
                  label = "Aspecto de interés", 
                  choices = unique(datos_app$label_parametro_input)),
      
      sliderInput(
        inputId = "confianza",
        label = "Nivel de confianza",
        value = 0.95,
        min = 0.85,
        max = 0.99,
        step = 0.01
      )
    ),
    
    mainPanel(
      
        tabPanel("Gráfico", plotOutput("distPlot"))
      
    )
  )
)

server <- function(input, output) {
  
  
  #### FILTRADO DE DATOS ####
  
  funcion_filtrado <- function(grado_sel, label_parametro_input_sel, confianza){
    datos_app_filtered <- datos_app %>% filter(grupo == grado_sel,
                                               label_parametro_input == label_parametro_input_sel)
    
    datos_app_filtered <- datos_app_filtered %>% 
      mutate(quantil = case_when(metodo == "t-student" ~ qt(confianza + (1 - confianza)/2, n-1),
                                 metodo == "normal" ~ qnorm(confianza + (1 - confianza)/2))) %>%
      mutate(B = quantil * se)
    return(datos_app_filtered)
  }
  
  tabla_filtrada <- reactive({
     funcion_filtrado(input$grado, input$parametro, input$confianza)
  })
  
  #### RENDERIZACIÓN DEL PLOT ####
  
  output$distPlot <- renderPlot({
    
    ggplot(tabla_filtrada(), aes(x = 1, y = est)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size=14),
            strip.text.y = element_text(size = 14, angle = 90)) +
      geom_errorbar(aes(ymin = est - B, ymax = est + B), width = 0.4, colour = "black", size = 1.5) +
      geom_point(size = 5, colour = "red") +
      guides(x = "none") +
      scale_x_continuous(limits = c(0, 2), breaks = NULL) +
      scale_y_continuous(limits = c(as.numeric(tabla_filtrada()["LI"]), as.numeric(tabla_filtrada()["LS"]))) +
      labs(title = tabla_filtrada()["label_parametro"], 
           y = "Estimación e intervalo de confianza", 
           x = input$grado)
    
    
  })
  
}

shinyApp(ui = ui, server = server)

