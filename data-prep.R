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


# National Outcome Measures from HRSA: Maternal & Child Health
#                                      https://mchb.tvisdata.hrsa.gov/PrioritiesAndMeasures/NationalOutcomeMeasures
#                                      https://mchb.tvisdata.hrsa.gov/uploadedfiles/Documents/FADResourceDocument.pdf

# Maternal mortality: (# deaths related to or aggravated by pregnancy within 42 days of end of pregnancy)/(# live births)
#                     rate per 100,000                      
# source: National - National Vital Statistics System
maternal.mortality <- read.xlsx('Data/HRSA_natl_outcome/maternal_mortality.xlsx', sheetName = 'Sheet1')

# Severe maternal morbidity: (# deliveries hospitalizations with an indication of severe morbidity)/(# delivery hospitalizations)
#                            rate per   
# source: National - HCUP State Inpatient Database
maternal.morbidity <- read.xlsx('Data/HRSA_natl_outcome/maternal_morbidity.xlsx', sheetName = 'Sheet1')

# Infant mortality: (# deaths to infants from birth through 364 days)/(# live births)
#                   rate per 1,000
# source: National - National Vital Statistics System
infant.mortality <- read.xlsx('Data/HRSA_natl_outcome/infant_mortality.xlsx', sheetName = 'Sheet1')

# Neonatal mortality: (# deaths to infants under 28 days)/(# live births)
#                     rate per 1,000
# source: National - National Vital Statistics System
neonatal.mortality <- read.xlsx('Data/HRSA_natl_outcome/neonatal_mortality.xlsx', sheetName = 'Sheet1')

# Drinking during pregnancy: (# women report drinking alcohol in last 3 mo pregnancy)/(# live biths)
#                            percent
# source: National - Pregnancy Risk Assessment Monitoring System
drinking.during.pregnancy <- read.xlsx('Data/HRSA_natl_outcome/drinking_during_pregnancy.xlsx', sheetName = 'Sheet1')

maternal_mortality <- maternal.mortality[1:6] %>%
                      `colnames<-`(c('Outcome', 'X2009', 'X2010', 'X2011', 'X2012', 'X2013')) 
maternal_mortality$Outcome <- 'Maternal Mortality'

maternal_morbidity <- maternal.morbidity[c(1, 3:7)] %>%
                      `colnames<-`(c('Outcome', 'X2009', 'X2010', 'X2011', 'X2012', 'X2013')) 
maternal_morbidity$Outcome <- 'Maternal Morbidity'
                      
infant_mortality <- infant.mortality %>%
                    `colnames<-`(c('Outcome', 'X2009', 'X2010', 'X2011', 'X2012', 'X2013')) 
infant_mortality$Outcome <- 'Infant Mortality'

neonatal_mortality <- neonatal.mortality %>%
                      `colnames<-`(c('Outcome', 'X2009', 'X2010', 'X2011', 'X2012', 'X2013')) 
neonatal_mortality$Outcome <- 'Neonatal Mortality'

drinking_during_pregnancy <- drinking.during.pregnancy[c(1, 4:8)] %>%
                             `colnames<-`(c('Outcome', 'X2009', 'X2010', 'X2011', 'X2012', 'X2013'))
drinking_during_pregnancy$Outcome <- 'Drinking During Pregnancy'

natl_outcome_measures <- bind_rows(list(maternal_mortality, maternal_morbidity, infant_mortality, neonatal_mortality, drinking_during_pregnancy))
                        
