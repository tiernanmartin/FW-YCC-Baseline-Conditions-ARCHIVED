## LEAFLET MAP EXPERIMENT

# PROJECT SETTINGS --------------------------------------------------------------------------------

options(scipen=999)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS


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
                  layer = "Neighborhoods") %>% 
        spTransform(.,CRSobj = crs_proj)

# select the Centreal Area Crescent neighborhoods from the small list
CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 

nhoods_CAC <- nhoods[nhoods$S_HOOD %in% CAC_sm,]

# create a centroid object for neighborhood labels 
nhoods_CAC_cntr <- gCentroid(spgeom = nhoods_CAC,byid = TRUE) %>%     
        SpatialPointsDataFrame(.,data = as.data.frame(nhoods_CAC@data))

red <- "#ff0000" # leaflet uses HEX code colors
white <- "#ffffff"
black <- "#000000"
blue <- "#0000FF"

leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = nhoods_CAC,
                    fillColor = red, 
                    fillOpacity = .5,
                    color = white,
                    opacity = .5,
                    popup = ~S_HOOD) 


nhoods_CAC_union <- gUnaryUnion(spgeom = nhoods_CAC) %>%  # merge CAC neighborhoods together into one polygon
        spTransform(., CRSobj = crs_geog) %>%
        gBuffer(width = -1000) %>% 
        spTransform(., CRSobj = crs_proj)

# GET SCHOOL ATTENDANCE AREA BOUNDARY SPATIAL DATA ------------------------------------------------

# Check if the Seattle public school attendance area boundaries shapefiles
# have already been downloaded, and if they haven't then download the zip file.

if(!file.exists("./2_inputs/sps_attendance_area_ES_2015_2016.sbx")){  
        
        url <- "https://www.seattleschools.org/UserFiles/Servers/Server_543/File/District/Departments/Enrollment%20Planning/Maps/gisdata/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016.zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
}

bgatz <- readOGR(dsn = "./2_inputs/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016_v2/",
                 layer = "sps_attendance_area_ES_2015_2016") %>% 
        .[bgatz@data$ES_ZONE == "Gatzert",] %>% 
        gBuffer(width = -500) %>%    # decrease the size of the polygon to avoid including sliver overlap block groups
        spTransform(., CRSobj = crs_proj)



# GET BLOCK.GROUP SPATIAL DATA --------------------------------------------------------------------

# Note: to expedite the processing time, the following script is run once and the output
# is saved for future uses

bg_seattle <- tigris::block_groups(state = "WA",county = "King") %>%  # Download the census block groups for King County
        spTransform(., CRSobj = crs_proj)

overlap <- gIntersects(spgeom1 = bg_seattle,
                       spgeom2 = bgatz,
                       byid = T) %>% 
        which(.==TRUE)
        

bg_CAC <- bg_seattle[overlap,]

leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = nhoods_CAC,
                    fillColor = red, 
                    fillOpacity = .5,
                    color = white,
                    opacity = .5,
                    popup = ~S_HOOD) %>% 
        addPolygons(data = bg_CAC,
                    fillColor = blue,
                    opacity = 0) %>% 
        addPolygons(data = bgatz,
                    fillOpacity = 0,
                    color = black)

# GET DEMOGRAPHIC DATA ----------------------------------------------------------------------------

# MERGE -------------------------------------------------------------------------------------------

# CREATE LEAFLET MAP ------------------------------------------------------------------------------

# SAVE MAP ----------------------------------------------------------------------------------------

