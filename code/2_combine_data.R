#####
# Project: Open data for city of Rostock
# Purpose: Load the data from the platform
# Author: Henrik-Alexander Schubert
# Date: 06.05.2025
###

rm(list = ls()); gc(TRUE)

library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(sf)
library(lwgeom)

## STRUCTURE
# 1. Combine the data
# 2. Create maps

## Combine the data -------------------------------

# Load the meta data
meta <- read.csv("meta_data.csv")

# Extract the dataset names from the meta data
dataset_names <- unique(meta$dataset)

# Function that loads the data, assigns the year and returns it
combine_data <- function(dataset_name) {
  
  # Create the data set names
  filenames <- list.files(file.path("raw", dataset_name), full.names = T)
  
  # Extract the years
  years <- str_extract(filenames, "[0-9]{4}")
  
  # Load the data
  tmp <- lapply(years, FUN = function(year) {
    # Load the data
    df <- read.csv(filenames[years==year])
    
    # Assign a year column
    df$year <- as.numeric(year)
    
    return(df)
  })
  
  # Bind the data
  tmp <- bind_rows(tmp)
  
  # Save the file
  write.csv(tmp, file.path("data", paste0(dataset_name, ".csv")))
  
  # Write to the memory
  assign(dataset_name, tmp, envir = .GlobalEnv)
  
  return(tmp)
  
}

# Combine the data
datasets <- lapply(dataset_names, combine_data)


## Create the maps -------------------------------

# Load the city map data
map <- read_sf(list.files("map", pattern = "stadtbereiche.shp", full.names=T))

## Load the water and boundary maps
waterflows <- read_sf(list.files("map", pattern = "fliessgewaesser.shp", full.names=T))
waterflows <- waterflows[waterflows$art=="offen", ]
uferlinie <- read_sf(list.files("map", pattern = "^uferlinie_mecklenburg-vorpommern.shp", full.names=T))
kreise <- read_sf(list.files("map", pattern = "^kreise_mecklenburg-vorpommern.shp", full.names=T))

# Constrain the shapefiles
waterflows <- waterflows[map, ]
uferlinie <- uferlinie[map, ]
sf_use_s2(FALSE)

# Select the states around Rostock
my_bbox <- st_bbox(map)
kreise <- st_crop(kreise, my_bbox)

# Select the data
# water <- st_intersection(st_as_sfc(my_bbox), uferlinie)
# water <- gBuffer(uferlinie, width=0.01, quadsegs=100)

# Create the district labels
map_labels <- st_centroid(map)

# Plot the data
base_map <- ggplot() +
  geom_sf(data=kreise, fill="white", colour="white") +
  geom_sf(data=map, colour="black", alpha=0.8, linewidth=1.3) +
  geom_sf(data=waterflows, colour="lightblue2", fill="darkblue") +
  #geom_sf(data=water, colour="darkblue", fill="darkblue") +
   theme(
    panel.background = element_rect(fill="lightblue2"),
    legend.position = c(0.9, 0.2),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(), 
    panel.border = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0))


# Save the base map as R-file
save(base_map, map_labels, file="base_map.Rda")

### END #############################################################