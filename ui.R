library(shiny)
library(leaflet)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Policy effects on abortion rates"),
  # Create sidebar layout
  sidebarLayout(
    navbarPage('',
               tabPanel('Home'
                       
               ),
               tabPanel('National Map',
                        # creating a title for the tab
                        headerPanel("Change in abortion rates nationally"),
                        
                        # Side panel for controls
                        sidebarPanel(
                          helpText("Select whether the abortions are calcuated by resident or by service."),
                          # creating a divider
                          hr(),
                          # creating a drop down to choose the county to display on the plot
                          selectInput("Choice", label = 'choice', choices = c("Residence", "Service"), selected = 'Resident'),
                          
                          # Input to select variable to map
                          sliderInput("Nationalyear1",
                                      "Nationalyear1",
                                      min = 2009,
                                      max = 2013,
                                      value = 2009),
                          sliderInput("Nationalyear2",
                                      "Nationalyear2",
                                      min = 2009,
                                      max = 2013,
                                      value = 2013)
                        ),
                        mainPanel(
                          leafletOutput('BuildNationalMap')
                        )
                        
               ),
               tabPanel('Illinois Map',
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
               ),
               tabPanel('National aggregate',
                        # creating a title for the tab
                        headerPanel("Change in abortion rates in Illinois"),
                        
                        mainPanel(
                          plotlyOutput("BuildNationalChart")
                        )
               )
    ),
    
    # Main panel: display leaflet map
    mainPanel(
      
    )
  )
))