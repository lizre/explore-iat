# This code was used to take certain variables from, and a random sample of, data from the Gender-Science Implicit Association Test. This data had already been cleaned as part of the same Documenting Bias project.

library(here)
library(dplyr)

gendersciiatdat <- read.csv("genderscience_cleandata.csv", header = TRUE) %>% select(implicit, explicit, raceomb, sex, politics, year, age, education) # import data, selecting only needed variables
#  N = 387 974

gendersciiatdat <- gendersciiatdat[sample(1:nrow(gendersciiatdat), 8000, replace = FALSE),]
# Take random sample of 8000 rows; about 2%

### Write data to csvs to export ##### ================
library(readr)
write_csv(gendersciiatdat, "gendersciiatdat.csv") # Export data to use in Shiny
