#####
# Project: Open data for city of Rostock
# Purpose: Load the data from the platform
# Author: Henrik-Alexander Schubert
# Date: 06.05.2025
###

library(httr)
library(jsonlite)
library(rvest)
library(stringr)

# Website: https://www.opendata-hro.de

# STRUCTURE:
# 1. Set the paths
# 2. Load the data


# 1. Paths ---------------------------------

# Set the root path to the website
path_root <- "https://www.opendata-hro.de"

## Obtain the observation years

# Get the years
side_bevoelkerung <- read_html(file.path(path_root, "group/bevoelkerung"))
minimal_html(side_bevoelkerung)

# Extract the observation years from the links
extract_years_from_links <- function(website) {
  links <- html_elements(website, "li")
  links <- html_text2(links)
  years <- unique(as.numeric(str_extract(links, "[0-9]{4}")))
  years <- years[!is.na(years)]
  return(years)
}
 
# Get the observation years
years <- extract_years_from_links(side_bevoelkerung)

## Collect the data sets for each year

# Function to get the data sets for a year
get_data_links_year <- function(year, format="csv") {

  # Obtain the different data sets for each year
  path_bevoelkerung_year <- file.path(path_root, paste0("dataset/bevoelkerungsstruktur_", year))
  
  # Load the website
  side_bevoelkerung <- minimal_html(read_html(path_bevoelkerung_year))
  
  # Load the content for a year
  links <- side_bevoelkerung |> 
    html_elements("li") |> 
    html_element("a") |> 
    html_attr("href")
  
  # Filter those with a .csv ending
  links <- links[str_detect(links, paste0(".", tolower(format)))]

  return(links)
}


# Extract the name of the data from the hyperlink
extract_dataset_name <- function(hyperlink) {
  # Example: "https://geo.sv.rostock.de/download/opendata/bevoelkerungsstruktur_2005/bevoelkerungsstruktur_2005_alter.csv"               
  len <- nchar(hyperlink)
  str_sub(hyperlink, start=99, end=len-4)
}


## Create the meta data ---------------------------------------------

# Load all the hyperlinks
links <- lapply(years, get_data_links_year)
repetitions <- lapply(links, length)
links <- do.call(c, links)

# Create the dataset categories
meta_data <- data.frame(year = rep(years, times=repetitions),
                        dataset = extract_dataset_name(links),
                        link = links)

# Save the meta-data
write.csv(meta_data, file = "meta_data.csv")


## Create the folder structure -----------------------------------

create_folder <- function(foldername) {
  if(!dir.exists(foldername)) {
    dir.create(foldername)
  }
}

# create the Base folders
lapply(c("raw", "data"), create_folder)

# Create the topic folder
lapply(file.path("raw", unique(meta_data$dataset)), create_folder)

# Create the save place



## Load the data -------------------------------------------------

# Load the data
datasets <- lapply(meta_data$link, read.csv)

# Save the datasets
lapply(seq_along(datasets), FUN = function(i){
  write.csv(datasets[i], file=paste0(file.path("raw", meta_data$dataset[i], paste(meta_data$dataset[i], meta_data$year[i], sep = "_")), ".csv"))
})



### END #########################################################