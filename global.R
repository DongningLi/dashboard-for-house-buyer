# Loads required libraries and data, and initializes variables
library(shiny)
library(leaflet)
library(sf)
library(shinyalert)
library(tidyverse)
library(dplyr)
library(DT)
library(shinyjs)

# Function
# Define the filter_facilities function
filter_facilities <- function(data, target_suburb, target_theme) {
  # Filter the data for the specified suburb
  facilities_in_suburb <- data[data$suburb == target_suburb, ]
  
  # Filter the data for the specified sub_theme
  facilities_filtered <- facilities_in_suburb[facilities_in_suburb$theme == target_theme, ]
  
  return(facilities_filtered)
}

# Make icon

health_icon <- makeIcon(
  iconUrl = "images/health_icon.png",
  iconWidth = 20,
  iconHeight = 20
)

school_icon <- makeIcon(
  iconUrl = "images/school_icon.png",
  iconWidth = 20,
  iconHeight = 20
)
church_icon <- makeIcon(
  iconUrl = "images/church_icon.png",
  iconWidth = 20,
  iconHeight = 20
)
transport_icon<- makeIcon(
  iconUrl = "images/transport_icon.png",
  iconWidth = 20,
  iconHeight = 20
)









