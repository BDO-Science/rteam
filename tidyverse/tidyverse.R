library(tidyverse)
library(janitor)

#######reading in files, changing data type, cleaning column names, and renaming columns

#with base R
s1 <- read.csv('steelhead.csv') #read in file
s1$Sample.Time <- as.Date(s1$Sample.Time) #convert sample time from date/time to just date
names(s1)[1] <- "date" #rename column to date

#with tidyverse
s2 <- read_csv('steelhead.csv') %>%
  clean_names() %>%
  mutate(sample_time = as.Date(sample_time)) %>%
  rename('date' = 'sample_time')
