## Analysis file
#

# Needed libraries
library(readr)
library(tidyr)
library(lubridate)
library(tibble)
library(ggplot2)
library(dplyr)


# Read in data
abortions.by.residence <- read_csv('Data/total.abortion.by.residence.csv')
abortions.by.service <- read_csv('Data/total.abortion.by.service.csv')
natl.outcome.measures <- read_csv('Data/natl.outcome.measures.csv') %>%
    select(year,
           (1:4)) %>%
    rename(Year = year) %>%
    mutate(Year = make_date(Year))


###################
### Format data ###
###################

## Sum abortion data ##

residence.totals <- colSums(abortions.by.residence[, -1], na.rm = T) %>%
    as_tibble() %>%
    rownames_to_column(var = 'Year') %>%
    mutate(Year = make_date(Year),
           summation = 'State of Residence')

service.totals <- colSums(abortions.by.service[, -1], na.rm = T) %>%
    as_tibble() %>%
    rownames_to_column(var = 'Year') %>%
    mutate(Year = make_date(Year),
           summation = 'State of Service')

all.totals <- bind_rows(residence.totals, service.totals) %>%
    arrange(Year)


## Create long data ##

# NATL data over time
natl.long <- natl.outcome.measures %>%
    gather('Metric', 'Rate', 2:5)

# Abortions by state of residence over time
ab.res.long <- abortions.by.residence %>%
    gather('Year', 'value', -1)

# Abortions by state of service over time
ab.srv.long <- abortions.by.service %>%
    gather('Year', 'value', -1)


## Group all data by time ##

facet.data <- all.totals %>%
    spread(summation, value) %>%
    left_join(natl.outcome.measures, by = 'Year') %>%
    gather('Metric', 'Rate', -(1:3)) %>%
    gather('Qualifier', "Number", 2:3)


#####################
### Create graphs ###
#####################

## Summary plots ##

# NATL data over time
g.natl.summary <- ggplot(data = natl.long,
                         aes(x = Year,
                             y = Rate,
                             color = Metric,
                             group = Metric)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = 'NATL Outcome Metrics over Time',
         x = 'Year',
         y = 'Rate (per 10,000 people)')


# Abortion totals over time
g.ab.tot.summary <- ggplot(data = all.totals,
                           aes(x = Year,
                               y = value,
                               color = summation,
                               group = summation)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = 'Number of Abortions over Time',
         subtitle = 'Total abortions; grouped by summation method',
         x = 'Year',
         y = 'Number')


# Abortions by state of residence over time
g.ab.res.summary <- ggplot(data = ab.res.long,
                           aes(x = Year,
                               y = value,
                               color = State,
                               group = State)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = 'Number of Abortions over Time',
         subtitle = 'For state of residence; grouped by state',
         x = 'Year',
         y = 'Number')


# Abortions by state of service over time
g.ab.srv.summary <- ggplot(data = ab.srv.long,
                           aes(x = Year,
                               y = value,
                               color = State,
                               group = State)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = 'Number of Abortions over Time',
         subtitle = 'For state of service; grouped by state',
         x = 'Year',
         y = 'Number')


## Covariate graphs ##

# Facet graph of all natl outcomes with abortion rates
g.facet <- ggplot(data = facet.data,
                  aes(x = Number,
                      y = Rate,
                      color = Qualifier,
                      group = Qualifier)) +
    facet_wrap(~ Metric,
               scales = 'free',
               ncol = 2) +
    geom_point(size = 2) +
    geom_smooth(method = 'lm') +
    labs(title = 'HRSA Outcome Rates vs Number of Abortions',
         subtitle = 'Seperated by summation method and HRSA metric',
         x = 'Number of Abortions',
         y = 'Rate (per 10,000 people)')


############################
### Statistical analysis ###
############################

## Basic correlation tests ##


