library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library('MSQC')


ui = source(file.path("ui.R"),local = TRUE)$value
server = source(file.path("server.R"),local = TRUE)$value

shinyApp(ui, server)
