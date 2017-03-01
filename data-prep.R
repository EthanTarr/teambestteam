# Team Best Team
# Nico Malig
# Winter 2017 | INFO 498c
# 
# Prep: Demographic information

library(dplyr)
library(xlsx)
library(tidyr)

# Uncomment when you need this run, the xlsx package takes FOREVER to read stuff...
# data.raw.illinois <- read.csv("data/IL-ab-demo.csv")
# data.raw.un <- read.xlsx("data/UNPD2016.xlsx", sheetName = 'DATA')

data.raw.un.colnames <- data.raw.un[3,]
data.raw.un.colnames2 <- data.raw.un[4,]
