#####
# Project: Open data for city of Rostock
# Purpose: Population projection
# Author: Henrik-Alexander Schubert
# Date: 31.03.2026
###

library(data.table)
library(tidyverse)

## Structure:
# 1. Population structure in the jump off year
# 2. Scenarios for mortality, fertility and migration
# 3. Estimate the cohort-component method

# Meta-parameter =====================================================

# 
jump_off_year <- 2024
forecasting_horizon <- 30
time_speps <- 5
t <- seq(jump_off_year, jump_off_year+forecasting_horizon, by = time_speps)

# Age groups
max_age <- 110

# Population structure ===============================================

# Population structure comes from Regionaldatenbank of the German Statistical Office

# 
header_pop <- read.csv("raw/pop_structure_gesis.csv", skip = 5, nrows = 1, sep = ";", header = F)
header_pop <- header_pop[1, ]
header_pop[is.na(header_pop)] <- "Estimate"
pop_2024 <- read.csv("raw/pop_structure_gesis.csv", skip = 7, sep = ";", header = T, col.names = header_pop)
names(pop_2024)[1:2] <- c("code", "municipality") 

# Clean the column names
names(pop_2024) <- str_to_lower(str_replace_all(names(pop_2024), "\\.", "_"))
pop_2024 <- pop_2024[, !str_detect(names(pop_2024), "^estimate_")]
names(pop_2024) <- paste0(names(pop_2024), c("", "", rep(c("_men", "_women"), each = 17)))

# Reshape to long format
pop_2024 <- pivot_longer(pop_2024,
                         cols = ends_with("men"),
                         names_to = "group",
                         values_to = "pop")


# Create the sex column
pop_2024 <- pop_2024 %>% 
  mutate(sex = ifelse(str_detect(group, "_women"), "women", "men"),
         age_lower = ifelse(str_detect(group, "^unter"), 0, str_extract(group, "\\d+")),
          age_upper = ifelse(str_detect(group, "und_mehr"), max_age, str_remove(str_extract(group, "\\d+_jahre"), "_jahre")))


# Make age numeric
pop_2024 <- pop_2024 %>%
  mutate(age_lower = as.numeric(age_lower),
         age_upper = as.numeric(age_upper),
         pop = as.numeric(pop))

# Extract Rostock
pop_2024_hro <- pop_2024 %>%
  filter(municipality == "Rostock, kreisfreie Stadt")


# Plot the population pyramide
ggplot(data = pop_2024_hro, aes(x = age_lower, y = ifelse(sex == "men", -pop, pop), group = sex, colour=sex)) +
  geom_step(linewidth = 1.5) + 
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_continuous(labels = abs, n.breaks = 10) +
  scale_x_continuous("Age", expand = c(0, 0))
  
# 2. Estimate the demographic processes =====================================

# 
hro_processes <- read.csv("raw/bewegung_natuerlich/bewegung_natuerlich_2019.csv")



### END ###############################################################