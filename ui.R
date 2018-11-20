library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)

fluidPage(
  tags$head(tags$style(HTML("
    .hovertext text{
        font-size:20px !important;
}
             ")),
  tags$link(rel = "stylesheet", type = "text/css", 
            href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
            ),
  # Set theme
  theme = shinytheme("spacelab"),
  
  # Some help text
  div(class="container",h2("Gráficos de CEP"),
  h4("Controle de Variância Multivariado")),
  
  # Vertical space
  tags$hr(),
  
  # Window length selector
  fluidRow(
  column(6,fileInput("dados",label="Entre com seu Conjunto de Daods",buttonLabel="Procurar",placeholder = "Nenhum Dado Selecionado")),
  column(6,selectInput("metodo", label = "Selecionar Método de Controle de Variância",
  choices = c("Método 1"=1,"Método 2"=2), selected = NULL))),
  
  # Plotly Chart Area
  fluidRow(
    column(6, plotlyOutput(outputId = "metodo1", height = "600px")),
    column(6, plotlyOutput(outputId = "metodo2",height = "600px")))
)