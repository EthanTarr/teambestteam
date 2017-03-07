## Analysis file
#

# Needed libraries
library(dplyr)
library(readr)
library(tidyr)
library(tibble)


# Read in data
abortions.by.residence <- read_csv('Data/abortion.by.residence.csv')
abortions.by.service <- read_csv('Data/abortion.by.service.csv')
natl.outcome.measures <- read_csv('Data/natl.outcome.measures.csv')


################
### Analysis ###
################

## Sum abortion data ##

residence.totals <- colSums(abortions.by.residence[, -1], na.rm = T) %>%
    as_tibble() %>%
    rownames_to_column(var = 'Year') %>%
    mutate(Year = as.numeric(Year))

service.totals <- colSums(abortions.by.service[, -1], na.rm = T) %>%
    as_tibble() %>%
    rownames_to_column(var = 'Year') %>%
    mutate(Year = as.numeric(Year))

