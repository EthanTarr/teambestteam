## Analysis file
#

# Needed libraries
library(readr)
library(tidyr)
library(lubridate)
library(tibble)
library(dplyr)


# Read in data
abortions.by.residence <- read_csv('Data/abortion.by.residence.csv')
abortions.by.service <- read_csv('Data/abortion.by.service.csv')
natl.outcome.measures <- read_csv('Data/natl.outcome.measures.csv') %>%
    select(year,
           (1:4)) %>%
    rename(Year = year) %>%
    mutate(Year = make_date(Year))


################
### Analysis ###
################

## Sum abortion data ##

residence.totals <- colSums(abortions.by.residence[, -1], na.rm = T) %>%
    as_tibble() %>%
    rownames_to_column(var = 'Year') %>%
    mutate(Year = make_date(Year))

service.totals <- colSums(abortions.by.service[, -1], na.rm = T) %>%
    as_tibble() %>%
    rownames_to_column(var = 'Year') %>%
    mutate(Year = make_date(Year))
