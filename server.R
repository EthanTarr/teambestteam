library(shiny)

# sourcing in the data
source('visualizations.R')

# using shiny server to build the app with the output of the map and input of the mapping values
shinyServer(function(input, output) {
  output$BuildMap <- renderLeaflet({
    return(BuildMap(input$year1, input$year2))
  })
  output$BuildNationalMap <- renderLeaflet({
    return(national.map(input$Nationalyear1, input$Nationalyear2, input$Choice))
  })
  output$BuildNationalChart <- renderPlotly({
    return(national.plot())
  })
  output$BuildNationalOutcomes <- renderPlotly({
    return(national.total())
  })
})
