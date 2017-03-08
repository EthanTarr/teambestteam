library(dplyr)
library(leaflet)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(tidyr)
library(xlsx)
library(rgdal)
library(plotly)

#Read in data from csv in Data folder
abortions.data <- read.csv("Data/Abortions_By_County__1995-2012.csv", stringsAsFactors = FALSE)


#modify data to fit for a map
#-----------------------------
#change all 50 or less values to 50
abortions.data[abortions.data==abortions.data[2,3]] <- "50"

#transform data to numberic
abortions.data <- transform(abortions.data, FREQUENCY = as.numeric(FREQUENCY))

#spread data by year and frequency
abortions.data <- spread(abortions.data, YEAR, FREQUENCY)

#uppercase county names for merging
abortions.data$COUNTY <- toupper(abortions.data$COUNTY)

#----------------------------

# read in map data as a Spatial Polygons Data Frame
map.data <- readShapeSpatial("Data/IL_BNDY_County/IL_BNDY_County_Py.shp")

#merge map data with county data
combined.map.data <- merge(map.data, abortions.data, by.x = "COUNTY_NAM", by.y = "COUNTY", duplicateGeoms = TRUE)

#data values to be read when needed by map
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
  
  #specific value to be mapped to
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
              labFormat = labelFormat(suffix = " reported cases"),
              opacity = 1
    )
  return(map)
}


# ------------------ National Map ---------------------
#load in data
abortion_by_residence <- read.csv("Data/abortion.by.residence.csv", stringsAsFactors = FALSE)
abortion_by_service <- read.csv("Data/abortion.by.service.csv", stringsAsFactors = FALSE)

# read in map data as a Spatial Polygons Data Frame
national.map.data <- readOGR("Data/cb_2015_us_state_20m/cb_2015_us_state_20m.shp")

#merge map data with resident county data
national.combined.map.resident.data <- merge(national.map.data, abortion_by_residence, by.x = "NAME", by.y = "State")

#merge map data with service county data
national.combined.map.service.data <- merge(national.map.data, abortion_by_service, by.x = "NAME", by.y = "State")

#load in data to be used for map
Residence.2009 <- national.combined.map.resident.data$X2009
Residence.2010 <- national.combined.map.resident.data$X2010
Residence.2011 <- national.combined.map.resident.data$X2011
Residence.2012 <- national.combined.map.resident.data$X2012
Residence.2013 <- national.combined.map.resident.data$X2013
Service.2009 <- national.combined.map.service.data$X2009
Service.2010 <- national.combined.map.service.data$X2010
Service.2011 <- national.combined.map.service.data$X2011
Service.2012 <- national.combined.map.service.data$X2012
Service.2013 <- national.combined.map.service.data$X2013

national.map <- function(year1, year2, choice) {
 
  #value to mapped to
  value <- eval(parse(text = paste0(choice, ".", year2))) - eval(parse(text = paste0(choice, ".", year1)))
  
  #choice of data frame to use
  if(choice == "Residence") {
    data <- national.combined.map.resident.data
    #create map popups
    county_popup <- paste0(data$NAME, " with a change in abortion rate of",
                           "<br> ", round(value, 1), " per 100k from ", year1, " to ", year2)
    suf <- " per 100k"
  } else {
    data <- national.combined.map.service.data
    #create map popups
    county_popup <- paste0(data$NAME, " with a ", round(value, 3),"% change",
                           "<br> in proportional abortion rate from ", year1, " to ", year2)
    suf <- "% change"
  }
  
  
  
  #create color palette
  pal <- colorNumeric(
    palette = "OrRd",
    domain = value
  )
  
  #create map with legend
  map <- leaflet(data) %>%
    addPolygons(
      fillOpacity = 0.8, 
      color = "#BDBDC3", 
      weight = 1, fillColor = ~pal(value), popup = county_popup
    ) %>% 
    addLegend("bottomright", pal = pal, values = ~value,
              title = paste0("Rate from ", year1, " to ", year2),
              labFormat = labelFormat(suffix = suf),
              opacity = 1
    )
  return(map)
}

#load in data
total_abortion_by_residence <- read.csv("Data/total.abortion.by.residence.csv")
total_abortion_by_service <- read.csv("Data/total.abortion.by.service.csv")

#group state residence data for national values
total_abortion_by_residence[is.na(total_abortion_by_residence)] <- 0
total.abortion.by.residence <- summarise(total_abortion_by_residence, "residence" = "residence", "2009" = sum(X2009), 
                                         "2010" = sum(X2010), "2011" = sum(X2011), "2012" = sum(X2012), 
                                         "2013" = sum(X2013))
total.abortion.by.residence <- gather(total.abortion.by.residence, "Year", "Residence.Values", 2:6)

#group state service data for national values
total_abortion_by_service[is.na(total_abortion_by_service)] <- 0
total.abortion.by.service <- summarise(total_abortion_by_service, "service" = "service", "2009" = sum(X2009), 
                                       "2010" = sum(X2010), "2011" = sum(X2011), "2012" = sum(X2012), 
                                       "2013" = sum(X2013))
total.abortion.by.service <- gather(total.abortion.by.service, "Year", "Service.Values", 2:6)

#combine residence and service values to be plotted
total.abortions <- merge(total.abortion.by.residence, total.abortion.by.service, by = "Year")

national.total <- function() {
  
  #plot of national outcomes data
  plots <- plot_ly(total.abortions, x = ~Year, y = ~Residence.Values, name = 'Residence Total', type = 'scatter', mode = 'lines',
                           line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
    add_trace(y = ~Service.Values, name = 'Service Total', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
    layout(title = "National Rates of Abortion",
           xaxis = list(title = "Years"),
           yaxis = list (title = "Number of Reported Abortions"))
  return(plots)
}

# ---------------------------------------------------------------

#load in data
natl.outcome <- read.csv("Data/natl.outcome.measures.csv")

national.plot <- function() {

#plot national data values
national.plot <- plot_ly(natl.outcome, x = ~year, y = ~Perinatal.Mortality, name = 'Perinatal Mortality', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
  add_trace(y = ~Infant.Mortality, name = 'Infant Mortality', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) %>%
  add_trace(y = ~Maternal.Morbidity, name = 'Maternal Morbidity', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
  add_trace(y = ~Maternal.Mortality, name = 'Maternal Mortality', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash')) %>%
  layout(
         xaxis = list(title = "Years"),
         yaxis = list (title = "People (per 10,000)"))
return(national.plot)
}