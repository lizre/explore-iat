# This code was used to take certain variables from, and a random sample of, data from the Gender-Science Implicit Association Test. This data had already been cleaned as part of the same Documenting Bias project.

library(here)
library(dplyr)

sexualityiatdat <- read.csv("sexuality_cleaned.csv", header = TRUE) %>% select(Implicit, Explicit, racenum, sexnum, politics, datecode, age, year, education, numiats, religiosity) # import data, selecting only needed variables
#  N = 993 953

sexualityiatdat <- sexualityiatdat[sample(1:nrow(sexualityiatdat), 8000, replace = FALSE),] # Take random sample of 8000 rows; about .8%

### Write data to csv to export ##### ================
library(readr)
write_csv(sexualityiatdat, "sexualityiatdat.csv") # Export data to use in Shiny
