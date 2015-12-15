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

# GET NEIGHBORHOOD SPATIAL DATA ------------------------------------------------------------------------------

# Check if the Seattle neighborhood boundaries data has already been downloaded,
# and if it hasn't then download it.

if(!file.exists("./2_inputs/Neighborhoods/WGS84/Neighborhoods.dbf")){  
        
        url <- "https://data.seattle.gov/download/2mbt-aqqx/application/zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
}

nhoods <- readOGR(dsn = "./2_inputs/Neighborhoods/WGS84/",  # select YCC and adjacent neigborhood boundaries
                  layer = "Neighborhoods")

# select the Centreal Area Crescent neighborhoods from the small list
CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 

nhoods_CAC <- nhoods[nhoods$S_HOOD %in% CAC_sm,]

red <- "#ff0000" # leaflet uses HEX code colors
white <- "#ffffff"

leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = nhoods_CAC,
                    fillColor = red, 
                    fillOpacity = .5,
                    color = white,
                    opacity = .5)

nhoods_CAC_union <- gUnaryUnion(spgeom = nhoods_CAC) # merge CAC neighborhoods together into one polygon

# GET BLOCK.GROUP SPATIAL DATA --------------------------------------------------------------------



# GET DEMOGRAPHIC DATA ----------------------------------------------------------------------------

# MERGE -------------------------------------------------------------------------------------------

# CREATE LEAFLET MAP ------------------------------------------------------------------------------

# SAVE MAP ----------------------------------------------------------------------------------------

