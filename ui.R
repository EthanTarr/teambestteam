library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("Title"),
  # Create sidebar layout
  sidebarLayout(
    navbarPage('Policy effects on abortion rates',
               tabPanel('Home'
                       
               ),
               tabPanel('Info'
                        
               ),
               tabPanel('Map',
                        # creating a title for the tab
                        headerPanel("Change in abortion rates in Illinois"),
                        
                        # Side panel for controls
                        sidebarPanel(
                          
                          # Input to select variable to map
                          sliderInput("year1",
                                      "year1",
                                      min = 1995,
                                      max = 2012,
                                      value = 1995),
                          sliderInput("year2",
                                      "year2",
                                      min = 1995,
                                      max = 2012,
                                      value = 2012)
                        ),
                        mainPanel(
                          leafletOutput('BuildMap')
                        )
               )
    ),
    
    # Main panel: display leaflet map
    mainPanel(
      
    )
  )
))