library(shiny)

# sourcing in the data
source('IllinoisMap.R')
#source("data-prep.R")

# using shiny server to build the app with the output of the map and input of the mapping values
shinyServer(function(input, output) {
  output$BuildMap <- renderLeaflet({
    return(BuildMap(input$year1, input$year2))
  })
  output$BuildNationalMap <- renderLeaflet({
    return(national.map(input$year, input$choice))
  })
  output$BuildNationalChart <- renderPlotly({
    return(national.plot())
  })
})