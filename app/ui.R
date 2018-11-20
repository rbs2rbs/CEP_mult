library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(ggplot2)
library('shinyBS')



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
    column(4,fileInput("dados",label="Entre com seu Conjunto de Dados",buttonLabel="Procurar",placeholder = "Nenhum Dado Selecionado")),
    column(4,numericInput("n","Selecione Tamanho de Cada Amostra",8,min=1,max=100)),
    column(4,selectInput("metodo", label = "Selecionar Método de Controle de Variância",
                         choices = c("Método 1"=1,"Método 2"=2), selected = NULL))
  ),
  
  # Plotly Chart Area
  fluidRow(
    column(6, plotlyOutput(outputId = "multiS", height = "600px")),
    column(6, plotlyOutput(outputId = "univarS",height = "600px"))
  ),
  tags$hr(),
  fluidRow(
    column(6, plotlyOutput(outputId = "multiX", height = "600px")),
    column(6, plotlyOutput(outputId = "univarX",height = "600px"))
  ),
  textOutput("tex"),
  actionButton("drop","teste"),
  bsModal("modalExample", "Your plot","drop",plotOutput(outputId = "s")),
  includeScript("www/teste.js")
)
