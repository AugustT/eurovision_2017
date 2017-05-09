
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(googleVis)

data <- read.csv('bias_data.csv')

confidence <- function(x){
  if(x <= 1) ret <- 'low'
  if(x < 0.2) ret <- 'medium'
  if(x < 0.05) ret <- 'high'
  return(ret)
}

shinyServer(function(input, output) {

  output$googlePlot <- renderGvis({

    selected.data <- data[data$to.country == input$country,]
    
    selected.data$confidence <- ifelse(selected.data$estimate > 0, 'low', 'high')
    
    names(selected.data)[4] <- 'Bias'
    
    selected.data$hover_text <- paste('Confidence:', sapply(selected.data$p_value, FUN = confidence))
    
    gvisGeoChart(selected.data,
                locationvar = "voter",
                hovervar = 'hover_text',
                colorvar = 'Bias',
                options = list(projection = "kavrayskiy-vii",
                               region = 150,
                               colorAxis="{values:[-5, 0, 5],
                               colors:[\'red', \'white', \'green\']}",
                               legend = 'none',
                               # height = 800,
                               width = 700))
    
  })

})
