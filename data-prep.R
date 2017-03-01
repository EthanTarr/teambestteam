# setup
library(dplyr)
library(xlsx)
library(tidyr)

# Total abortions by state of service 2009 - 2013
service09 <- abortionByService('2009')
service10 <- abortionByService('2010')
service11 <- abortionByService('2011')
service12 <- abortionByService('2012')
service13 <- abortionByService('2013')

abortion_by_service <- service09 %>%
                       full_join(service10, by='State') %>%
                       full_join(service11, by='State') %>%
                       full_join(service12, by='State') %>%
                       full_join(service13, by='State')

# Total abortions by state of residence 2009 - 2013
residence09 <- abortionByResidence('2009')
residence10 <- abortionByResidence('2010')
residence11 <- abortionByResidence('2011')
residence12 <- abortionByResidence('2012')
residence13 <- abortionByResidence('2013')

abortion_by_residence <- residence09 %>%
                         merge(residence10, by=0) %>%
                         merge(residence11) %>%
                         merge(residence12) %>%
                         merge(residence13)

# function to organize dataset by state of service
abortionByService <- function(year){
                       df <- read.xlsx('data/abortions-by-state.xls', sheetName = year)
                       df <- df %>%
                             select(NA., NA..57) %>%
                             slice(2:53) %>%
                             `colnames<-`(c("State", year)) 
                       return(df)
                     }
# function to organize dataset by state of residence
abortionByResidence <- function(year){
                         df <- read.xlsx('data/abortions-by-state.xls', sheetName = year) %>%
                               select(State.of.Maternal.Residence:NA..56) 
                          
                         colnames(df) <- as.character(unlist(df[1,]))
                         df <- slice(df, 54) 
                          
                         df <- as.data.frame(t(df)) %>%
                               `colnames<-`(c(year))
                         return(df)
                       }
