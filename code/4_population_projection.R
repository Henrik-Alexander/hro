#####
# Project: Open data for city of Rostock
# Purpose: Population projection
# Author: Henrik-Alexander Schubert
# Date: 31.03.2026
###

library(data.table)
library(tidyverse)
library(haven)

## Structure:
# 1. Population structure in the jump off year
# 2. Scenarios for mortality, fertility and migration
# 3. Estimate the cohort-component method

# Meta-parameter =====================================================

# Load the population projection
pop_proj_hro <- read_xlsx("projection/HRO_Ergebnisse_2023-2040_AG.xlsx")

# Make the data into data.table
pop_proj_hro <- as.data.table(pop_proj_hro)

# Plot the aggregate
pop_proj_hro[, .(N = sum(Anzahl)), by = Jahr] %>% 
  ggplot(aes(x=Jahr, y = N)) + 
  geom_line()



# Change in the age structure
pop_proj_hro[Jahr %in% c(2024, 20240)]

### END ###############################################################