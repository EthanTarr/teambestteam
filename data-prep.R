# setup
library(dplyr)
library(xlsx)
library(tidyr)
library(plotly)

# function to organize dataset by state of service
abortionByService <- function(year){
  df <- read.xlsx('Data/abortions-by-state.xls', sheetName = year)
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

write.csv(abortion_by_service, "Data/abortion.by.service.csv", row.names = FALSE)

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

write.csv(abortion_by_residence, "Data/abortion.by.residence.csv", row.names = FALSE)

p.residence <- plot_ly(abortion_by_residence,
                       x = ~State,
                       y = ~abortion_by_residence[[2]],
                       type = 'scatter',
                       mode = 'markers',
                       name = '2009')
#p.residence


# National Outcome Measures from HRSA: Maternal & Child Health
#                                      https://mchb.tvisdata.hrsa.gov/PrioritiesAndMeasures/NationalOutcomeMeasures
#                                      https://mchb.tvisdata.hrsa.gov/uploadedfiles/Documents/FADResourceDocument.pdf

# (# deaths related to or aggravated by pregnancy within 42 days of end of pregnancy)/(# live births) - rate per 100,000
maternal.mortality <- read.xlsx('Data/HRSA_natl_outcome/maternal_mortality.xlsx', sheetName = 'Sheet1', stringsAsFactors = FALSE)

# (# deliveries hospitalizations with an indication of severe morbidity)/(# delivery hospitalizations) - rate per 10,000
maternal.morbidity <- read.xlsx('Data/HRSA_natl_outcome/maternal_morbidity.xlsx', sheetName = 'Sheet1', stringsAsFactors = FALSE)

# (# deaths to infants from birth through 364 days)/(# live births) rate per 1,000
infant.mortality <- read.xlsx('Data/HRSA_natl_outcome/infant_mortality.xlsx', sheetName = 'Sheet1', stringsAsFactors = FALSE)

# (# fetal deaths 28 weeks+ gestation + early neonatal deaths occuring <7 days) / (# live births + fetal deaths) - rate per 1,000
perinatal.mortality <- read.xlsx('Data/HRSA_natl_outcome/perinatal_mortality.xlsx', sheetName = 'Sheet1', stringsAsFactors = FALSE)

maternal_mortality <- as.data.frame(t(maternal.mortality[1:6])) %>%
                      slice(2:6) %>%
                      `row.names<-`(c('2009', '2010', '2011', '2012', '2013')) %>%
                      `colnames<-`(c('Maternal Mortality'))
# change from rate per 100,000 to rate per 10,000 - divide by 10
maternal_mortality <- transform(maternal_mortality, `Maternal Mortality` = as.numeric(as.character(`Maternal Mortality`))/10)

maternal_morbidity <- as.data.frame(t(maternal.morbidity[c(1, 3:7)])) %>%
                      slice(2:6) %>%
                      `row.names<-`(c('2009', '2010', '2011', '2012', '2013')) %>%
                      `colnames<-`(c('Maternal Morbidity'))

infant_mortality <- as.data.frame(t(infant.mortality)) %>%
                    slice(2:6) %>%
                    `row.names<-`(c('2009', '2010', '2011', '2012', '2013'))  %>%
                    `colnames<-`(c('Infant Mortality'))
# change from rate per 1,000 to rate per 10,000 - multiply by 10
infant_mortality <- transform(infant_mortality, `Infant Mortality` = 10 * as.numeric(as.character(`Infant Mortality`)))

perinatal_mortality <- as.data.frame(t(perinatal.mortality)) %>%
                      slice(2:6) %>%
                      `row.names<-`(c('2009', '2010', '2011', '2012', '2013')) %>%
                      `colnames<-`(c('Perinatal Mortality'))
# change from rate per 1,000 to rate per 10,000 - multiply by 10
perinatal_mortality <- transform(perinatal_mortality, `Perinatal Mortality` = 10 * as.numeric(as.character(`Perinatal Mortality`)))

# All in units rate per 10,000
natl_outcome_measures <- bind_cols(list(maternal_mortality, maternal_morbidity, infant_mortality, perinatal_mortality)) %>%
                         mutate(year = c(2009:2013))

write.csv(natl_outcome_measures, file = 'Data/natl.outcome.measures.csv', row.names = FALSE)

