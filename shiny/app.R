library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

datos_cargados <- read.csv("C:/Users/lopez/Documents/Prueba/Taller de programacion/Practicaprogramada2_shiny/datos/datos_libertad.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Libertades Mundiales", titleWidth = 230),
  
  dashboardSidebar(
    selectInput("pais", "Selecciona un País", choices = unique(datos_cargados$pais)),
    sliderInput("ano", "Selecciona el año:", min = 2008, max = 2016, value = c(2008, 2016)),
    radioButtons("metrica", "Elige visualización:", choices = c("Puntaje", "Ranking")),
    downloadButton("descargarPDF", "Descargar en PDF")
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("Libertad Humana", plotOutput("plotHumana")),
      tabPanel("Libertad Personal", plotOutput("plotPersonal")),
      tabPanel("Libertad Económica", plotOutput("plotEconomica"))
    )
  ),
  skin = "purple"
)


server <- function(input, output) {
  
  
  datos_filtrados <- reactive({
    filter(datos_cargados, pais == input$pais, anio >= input$ano[1] & anio <= input$ano[2])
  })
  
  
  render_graph <- function(metrica, titulo) {
    ggplot(datos_filtrados(), aes(x = anio, y = get(metrica), group = pais, color = pais)) +
      geom_line() +
      ggtitle(paste("Evolución de", titulo, "-", input$metrica)) +
      labs(color = "País") +
      theme_minimal()
  }
  
  
  output$plotHumana <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_humana_puntaje", "libertad_humana_ranking")
    render_graph(metrica, "Libertad Humana")
  })
  
  
  output$plotPersonal <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_personal_puntaje", "libertad_personal_ranking")
    render_graph(metrica, "Libertad Personal")
  })
  
  
  output$plotEconomica <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_economica_puntaje", "libertad_economica_ranking")
    render_graph(metrica, "Libertad Económica")
  })
  
  
  output$descargarPDF <- downloadHandler(
    filename = function() {paste("datos_", input$pais, ".pdf", sep = "")},
    content = function(file) {
      metrica <- ifelse(input$metrica == "Puntaje", paste("libertad", input$metrica, sep = "_"), paste("libertad", input$metrica, sep = "_"))
      pdf(file)
      print(render_graph(metrica, "Libertad"))
      dev.off()
    }
  )
}


shinyApp(ui, server)

