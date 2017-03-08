library(shiny)
library(leaflet)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Abortion Services and Policy"),
  # Create sidebar layout
  sidebarLayout(
    navbarPage('',
               tabPanel('Home'
                       
               ),
               tabPanel('National Map',
                        # creating a title for the tab
                        headerPanel("Change in Abortion Rates Nationally"),
                        
                        # Side panel for controls
                        sidebarPanel(
                          helpText("Select to see abortion rates by state of maternal residence, or by state of service"),
                          # creating a divider
                          hr(),
                          # creating a drop down to choose the county to display on the plot
                          selectInput("Choice", label = 'Qualifier', choices = c("Residence", "Service"), selected = 'Resident'),
                          
                          # Input to select variable to map
                          sliderInput("Nationalyear1",
                                      "Nationalyear1",
                                      min = 2009,
                                      max = 2013,
                                      value = 2009,
                                      sep = ""),
                          sliderInput("Nationalyear2",
                                      "Nationalyear2",
                                      min = 2009,
                                      max = 2013,
                                      value = 2013,
                                      sep = "")
                        ),
                        mainPanel(
                          leafletOutput('BuildNationalMap'),
                          hr(),
                          plotlyOutput("BuildNationalOutcomes")
                        )
                        
               ),
               tabPanel('Illinois Map',
                        # creating a title for the tab
                        headerPanel("Change in Abortion Rates in Illinois"),
                        
                        # Side panel for controls
                        sidebarPanel(
                          
                          # Input to select variable to map
                          sliderInput("year1",
                                      "year1",
                                      min = 1995,
                                      max = 2012,
                                      value = 1995,
                                      sep = ""),
                          sliderInput("year2",
                                      "year2",
                                      min = 1995,
                                      max = 2012,
                                      value = 2012,
                                      sep = "")
                        ),
                        mainPanel(
                          leafletOutput('BuildMap')
                        )
               ),
               tabPanel('National Outcome Measures',
                        # creating a title for the tab
                        headerPanel("National Outcome Measures "),
                        
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
