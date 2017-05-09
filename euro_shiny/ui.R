library(shiny)

data <- read.csv('bias_data.csv')

countries <- sort(as.character(unique(data$to.country)))

shinyUI(fluidPage(

  fluidRow(selectInput('country',
                       label = 'Select a country',
                       choices = countries,
                       selected = 'United Kingdom')),

  fluidRow(htmlOutput("googlePlot"))
  
))
