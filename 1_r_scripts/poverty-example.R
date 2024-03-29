## LEAFLET MAP EXPERIMENT

# LOAD PACKAGES -----------------------------------------------------------------------------------

require(rgdal)    # for readOGR and others
require(sp)       # for spatial objects
require(leaflet)  # for interactive maps (NOT leafletR here)
require(dplyr)    # for working with data frames
require(ggplot2)  # for plotting
require(tigris)
require(acs)
require(stringr) # to pad fips codes
require(purrr)
require(magrittr)
require(downloader)
require(tmap)
require(rgeos)

# PROJECT SETTINGS --------------------------------------------------------------------------------

options(scipen=999)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# GET AND SUBSET BLOCK.GROUP SPATIAL DATA --------------------------------------------------------------------

# bgSelect: A function to expedite the process of merging, buffering, and overlap selection of block groups

bgSelect <- function(shapes, multi = FALSE, buffer = -500, layer){
        
        if(multi == TRUE){
                shapes <-  rgeos::gUnaryUnion(spgeom = shapes)
                
                shp_buf <- spTransform(shapes, CRSobj = crs_geog) %>% 
                        gBuffer(spgeom = .,width = buffer) %>%
                        spTransform(., CRSobj = crs_proj)
                
                overlap <- gIntersects(spgeom1 = bg_sea,
                                       spgeom2 = shp_buf,
                                       byid = T) %>% 
                        which(.==TRUE)
                
                bg_sea[overlap,] %>%
                        writeOGR(dsn = "./2_inputs/",
                                 layer = layer,
                                 driver = "ESRI Shapefile")
                
        }
        
        if(multi == FALSE){
                shp_buf <- spTransform(shapes, CRSobj = crs_geog) %>% 
                        gBuffer(spgeom = .,width = buffer) %>%
                        spTransform(., CRSobj = crs_proj)
                
                overlap <- gIntersects(spgeom1 = bg_sea,
                                       spgeom2 = shp_buf,
                                       byid = T) %>% 
                        which(.==TRUE)
                
                bg_sea[overlap,] %>%
                        writeOGR(dsn = "./2_inputs/",
                                 layer = layer,
                                 driver = "ESRI Shapefile")
        }
        
}

# Note: to expedite the processing time, the following `if()` scripts are run once and the outputs
# are saved and accessed directly in all subsequent uses

# All Census block groups overlapping the CAC neighborhoods (excluding slivers)

if(!file.exists("./2_inputs/blockgroups_seattle.shp")){
        tigris::block_groups(state = "WA",county = "King") %>%  # Download the census block groups for King County
                spTransform(., CRSobj = crs_proj) %>% 
                writeOGR(dsn = "./2_inputs/","blockgroups_seattle",driver = "ESRI Shapefile")
}

bg_sea <- readOGR(dsn = "./2_inputs/",layer = "blockgroups_seattle") %>% 
        spTransform(.,CRSobj = crs_proj)

# All Census block groups overlapping the CAC neighborhoods (excluding slivers)

if(!file.exists("./2_inputs/blockgroups_CAC_nhoods.shp")){
        
        url <- "https://data.seattle.gov/download/2mbt-aqqx/application/zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
        
        nhoods <- readOGR(dsn = "./2_inputs/Neighborhoods/WGS84/",  # select YCC and adjacent neigborhood boundaries
                          layer = "Neighborhoods") %>% 
                spTransform(.,CRSobj = crs_proj)
        
        # select the Centreal Area Crescent neighborhoods from the small list
        CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 
        
        nhoods_CAC <- nhoods[nhoods$S_HOOD %in% CAC_sm,]
        
        bgSelect(shapes = nhoods_CAC,multi = TRUE,layer = "blockgroups_CAC_nhoods")
}

bg_nhoods <- readOGR(dsn = "./2_inputs/",   
                  layer = "blockgroups_CAC_nhoods") %>% 
        spTransform(CRSobj = crs_proj)

# All Census block groups in the Bailey-Gatzert attendance boundary

if(!file.exists("./2_inputs/blockgroups_bgatz.shp")){
        
        url <- "https://www.seattleschools.org/UserFiles/Servers/Server_543/File/District/Departments/Enrollment%20Planning/Maps/gisdata/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016.zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
        
        bgatz <- readOGR(dsn = "./2_inputs/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016_v2/",
                         layer = "sps_attendance_area_ES_2015_2016") %>% 
                .[.@data$ES_ZONE == "Gatzert",] %>%
                spTransform(., CRSobj = crs_proj)
        
        bgatz_buf <- spTransform(bgatz, CRSobj = crs_geog) %>% 
                gBuffer(spgeom = .,width = -500) %>%
                spTransform(., CRSobj = crs_proj)
        
        overlap <- gIntersects(spgeom1 = bg_sea,
                               spgeom2 = bgatz_buf,
                               byid = T) %>% 
                which(.==TRUE)
        
        bg_sea[overlap,] %>%
                writeOGR(dsn = "./2_inputs/",
                         layer = "blockgroups_bgatz",
                         driver = "ESRI Shapefile")
}

bg_bgatz <- readOGR(dsn = "./2_inputs/",layer = "blockgroups_bgatz") %>% 
        spTransform(CRSobj = crs_proj)

# All Census block groups within the boundary of the printed map (provided by Amy Gore, 12-14-2015)

myCACbound <- readOGR(dsn = "./2_inputs/myCACbound/",layer = "myCACbound")

bgSelect(shapes = myCACbound,buffer = -1000,layer = "blockgroups_myCACbound")

bg_myCAC <- readOGR(dsn = "./2_inputs/",layer = "blockgroups_myCACbound") %>% 
        spTransform(CRSobj = crs_proj)

# GET DEMOGRAPHIC DATA ----------------------------------------------------------------------------

# MERGE -------------------------------------------------------------------------------------------

# CREATE LEAFLET MAP ------------------------------------------------------------------------------

# A quick leaflet map for viewing one set of polygons
myLeaflet <- function(data){
        leaflet() %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = data)
}

myLeaflet(bg_myCAC)
# SAVE MAP ----------------------------------------------------------------------------------------

# SAVED OLD CODE ----------------------------------------------------------------------------------

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