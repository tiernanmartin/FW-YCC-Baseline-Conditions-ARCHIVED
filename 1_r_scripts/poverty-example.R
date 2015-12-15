## LEAFLET MAP EXPERIMENT

# PROJECT SETTINGS --------------------------------------------------------------------------------

options(scipen=999)

# LOAD PACKAGES -----------------------------------------------------------------------------------

library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(purrr)
library(magrittr)
library(downloader)
library(tmap)
library(rgeos)

# GET SPATIAL DATA ------------------------------------------------------------------------------

tracts <- tracts(state = "Washington",county = "King") # download spatial data for all King County census tracts

# Check if the Seattle neighborhood boundaries data has already been downloaded,
# and if it hasn't then download it.

if(!file.exists("./2_inputs/Neighborhoods/StatePlane/Neighborhoods.dbf.")){  
        
        url <- "https://data.seattle.gov/download/2mbt-aqqx/application/zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
}

# download spatial data for all Seattle neighborhood boundaries


# GET DEMOGRAPHIC DATA ----------------------------------------------------------------------------

# MERGE -------------------------------------------------------------------------------------------

# CREATE LEAFLET MAP ------------------------------------------------------------------------------

# SAVE MAP ----------------------------------------------------------------------------------------

