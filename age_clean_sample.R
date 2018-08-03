# This code was used to take certain variables from, and a random sample of, data from the Gender-Science Implicit Association Test. This data had already been cleaned as part of the same Documenting Bias project.

library(here)
library(dplyr)
library(haven)

ageiatdat <- read_sav("ageiat_cleaned.sav") %>% select(Implicit, Explicit, racenum, sexnum, politics, age, year, education, datecode, numiats, Age_group) # import data, selecting only needed variables
#  N = 795 376

ageiatdat <- ageiatdat[sample(1:nrow(ageiatdat), 8000, replace = FALSE),] # Take random sample of 8000 rows; about 1%

### Write data to csv to export ##### ================
library(readr)
write_csv(ageiatdat, "ageiatdat.csv") # Export data to use in Shiny
