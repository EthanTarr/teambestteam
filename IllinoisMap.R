library(dplyr)
library(leaflet)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(tidyr)

#Read in data from csv in Data folder
abortions.data <- read.csv("~/GitHub/teambestteam/Data/Abortions_By_County__1995-2012.csv", stringsAsFactors = FALSE)
live.births.data <- read.csv("~/GitHub/teambestteam/Data/All_Live_Births_In_Illinois__1989-2009.csv", stringsAsFactors = FALSE)
teen.mothers.data <- read.csv("~/GitHub/teambestteam/Data/IDPH_Births_to_Teen_Mothers__by_County__2009.csv", stringsAsFactors = FALSE)


#modify data to fit for a map
#-----------------------------
abortions.data[abortions.data==abortions.data[2,3]] <- "50"

abortions.data <- transform(abortions.data, FREQUENCY = as.numeric(FREQUENCY))

abortions.data <- spread(abortions.data, YEAR, FREQUENCY)

abortions.data$COUNTY <- toupper(abortions.data$COUNTY)


live.births.data <- live.births.data[-c(1,18,19), ]

teen.mothers.data <- teen.mothers.data[-c(17,18), ]

#----------------------------

# read in map data as a Spatial Polygons Data Frame
map.data <- readShapeSpatial("~/GitHub/teambestteam/Data/IL_BNDY_County/IL_BNDY_County_Py.shp")

#merge map data with county data
combined.map.data <- merge(map.data, abortions.data, by.x = "COUNTY_NAM", by.y = "COUNTY", duplicateGeoms = TRUE)

x.1995 <- combined.map.data$`1995`
x.1996 <- combined.map.data$`1996`
x.1997 <- combined.map.data$`1997`
x.1998 <- combined.map.data$`1998`
x.1999 <- combined.map.data$`1999`
x.2000 <- combined.map.data$`2000`
x.2001 <- combined.map.data$`2001`
x.2002 <- combined.map.data$`2002`
x.2003 <- combined.map.data$`2003`
x.2004 <- combined.map.data$`2004`
x.2005 <- combined.map.data$`2005`
x.2006 <- combined.map.data$`2006`
x.2007 <- combined.map.data$`2007`
x.2008 <- combined.map.data$`2008`
x.2009 <- combined.map.data$`2009`
x.2010 <- combined.map.data$`2010`
x.2011 <- combined.map.data$`2011`
x.2012 <- combined.map.data$`2012`

BuildMap <- function(year1, year2) {
  
  value <- eval(parse(text = paste0("x.", year2))) - eval(parse(text = paste0("x.", year1)))
  
  #create map popups
  names <- paste0(substr(combined.map.data$COUNTY_NAM, 0, 1), tolower(substr(combined.map.data$COUNTY_NAM, 2, 10000)))
  county_popup <- paste0(names, " county", " with a change",
                         "<br>in abortion rate of ", 
                         value, " from ", year1, " to ", year2)
  
  #create color palette
  pal <- colorNumeric(
    palette = "OrRd",
    domain = value
  )
  
  #create map with legend
  map <- leaflet(combined.map.data) %>%
    addPolygons(
      fillOpacity = 0.8, 
      color = "#BDBDC3", 
      weight = 1, fillColor = ~pal(value), popup = county_popup
    ) %>% 
    addLegend("bottomright", pal = pal, values = ~value,
              title = paste0("Change between ", year1, " and ", year2),
              labFormat = labelFormat(suffix = "abortions"),
              opacity = 1
    )
  return(map)
}
