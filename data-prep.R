# setup
library(dplyr)
library(xlsx)
library(tidyr)
library(plotly)

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
  df <- read.xlsx('Data/abortions-by-state.xls', sheetName = year) %>%
    select(State.of.Maternal.Residence:NA..56) 
  
  # colnames(df) <- as.character(unlist(df[1,]))
  df <- slice(df, c(1,54)) 
  
  df <- as.data.frame(t(df)) %>%
    `colnames<-`(c('State', year))
  return(df)
}

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

abortion_by_service[33, 6] = abortion_by_service[54, 6]
abortion_by_service <- slice(abortion_by_service, 1:52)
abortion_by_service[abortion_by_service == '--'] <- NA

abortion_by_service$`2009` <- as.numeric(as.character(abortion_by_service$`2009`))
abortion_by_service$`2010` <- as.numeric(as.character(abortion_by_service$`2010`))
abortion_by_service$`2011` <- as.numeric(as.character(abortion_by_service$`2011`))
abortion_by_service$`2012` <- as.numeric(as.character(abortion_by_service$`2012`))
abortion_by_service$`2013` <- as.numeric(as.character(abortion_by_service$`2013`))

temp <- filter(abortion_by_service, State == c("New York State", "New York City"))
temp <- summarise(temp, State = "New York", `2009` = sum(`2009`), `2010` = sum(`2010`), `2011` = sum(`2011`)
                  , `2012` = sum(`2012`), `2013` = sum(`2013`))
abortion_by_service[33, ] <- temp
abortion_by_service <- abortion_by_service[-c(34), ]

abortion_by_service[5,1] <- "California"
abortion_by_service[10,1] <- "Florida"
abortion_by_service[21,1] <- "Maryland"
abortion_by_service[30,1] <- "New Hampshire"

write.csv(abortion_by_service, "Data/abortion.by.service.csv")

# Total abortions by state of residence 2009 - 2013
residence09 <- abortionByResidence('2009')
residence10 <- abortionByResidence('2010')
residence11 <- abortionByResidence('2011')
residence12 <- abortionByResidence('2012')
residence13 <- abortionByResidence('2013')

abortion_by_residence <- residence09 %>%
                         full_join(residence10, by='State') %>%
                         full_join(residence11, by='State') %>%
                         full_join(residence12, by='State') %>%
                         full_join(residence13, by='State')

abortion_by_residence[33, 6] = abortion_by_residence[58, 6]
abortion_by_residence <- slice(abortion_by_residence, 1:57)
abortion_by_residence[abortion_by_residence == '--'] <- NA

abortion_by_residence <- abortion_by_residence[-c(52,53,54,55,56,57), ]
abortion_by_residence[5,1] <- "California"
abortion_by_residence[10,1] <- "Florida"
abortion_by_residence[21,1] <- "Maryland"
abortion_by_residence[30,1] <- "New Hampshire"
abortion_by_residence[33,1] <- "New York"

write.csv(abortion_by_residence, "Data/abortion.by.residence.csv")

p.residence <- plot_ly(abortion_by_residence,
                       x = ~State,
                       y = ~abortion_by_residence[[2]],
                       type = 'scatter',
                       mode = 'markers',
                       name = '2009')
#p.residence


# World contraceptive use: survey data from 1970 to 2013 
contraceptive_prevalence <- read.xlsx('Data/UNPD_WCU2016_Country_Data_Survey-Based.xlsx', sheetName = 'DATA')

colnames(contraceptive_prevalence) <- c(as.character(unlist(contraceptive_prevalence[3,]))[1:6], 
                                    as.character(unlist(contraceptive_prevalence[4,]))[7:24])
# narrow by United States and contraceptive prevalence (by percentage)
# any method increasing over years until 2006-08, decreasing by ~3% 2011-13, 
# increased significantly ~10% between 1973 and 1975
# use of pill increasing until 1975, decrease after 
contraceptive_prevalence <- contraceptive_prevalence[, 0:24] %>%
                            slice(1054:1067)


