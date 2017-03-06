library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("Title"),
  # Create sidebar layout
  sidebarLayout(
    navbarPage('Policy effects on abortion rates',
               tabPanel('Home'
                       
               ),
               tabPanel('National Map',
                        # creating a title for the tab
                        headerPanel("Change in abortion rates nationally"),
                        
                        # Side panel for controls
                        sidebarPanel(
                          helpText("Select a County in New York to display the number of Obese and Overweight Students
                 in the county selected. The colors of the scatter points represent the grade category 
                                   (including the district total). Hover over a scatter point to discover what school 
                                   district or area the students are from and the exact number of students overweight and obese 
                                   in that specific school."),
                          # creating a divider
                          hr(),
                          # creating a drop down to choose the county to display on the plot
                          selectInput("choice", label = 'choice', choices = c("Resident", "Service"), selected = 'Resident'),
                          
                          # Input to select variable to map
                          sliderInput("year",
                                      "year",
                                      min = 2009,
                                      max = 2013,
                                      value = 2009)
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
               )
    ),
    
    # Main panel: display leaflet map
    mainPanel(
      
    )
  )
))