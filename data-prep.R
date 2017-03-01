# setup
library(dplyr)
library(xlsx)
library(tidyr)
# how should we represent clinical service versus maternal residence
# we can use this data overall amounts in state that got abortions, subset showing by state who went to different state
# show traces, one indicating amount in state receivign clinical service in state, other trace showing receiving
# service out of state

# For each sheet, add a new column to data frame that is the sum of row (state of clinical service)
# col 60 is total by location of service, row 54 total by residence
natl_abortion09 <-  read.xlsx('data/abortions-by-state.xls', sheetName = '2009')
View(natl_abortion09)

abortion_by_service <- natl_abortion09 %>%
                       select(NA., NA..57) %>%
                       slice(2:53) %>%
                       `colnames<-`(c("State", 2009)) 

# for each year, go through and add abortion_by_service$2010
  
abortion_by_residence <- natl_abortion09 %>%
                         select(State.of.Maternal.Residence:NA..56) %>%
                         `colnames<-`(as.character(unlist(abortion_by_residence[1,]))) %>%
                         slice(54) 
rownames(abortion_by_residence) <- c(2009)

