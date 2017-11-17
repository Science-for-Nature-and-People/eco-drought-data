## TASKS:

## For each column in the csv, look for the corresponding shape file name and import the shape file
## Find the value in the shape file based off the corresponding ID 
## See if it matches with the value that matches the ID value in the csv file
## Keep a counter of how many mismatched values there are

library(rgdal) # Read in shape file
library(sp)
library(maptools)
library(tidyverse)
library(raster)
library(spatstat)

filepath <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors/ProjectClipMask"
all_files <- list.files(path = filepath)

## Read in shape file
shapefile <- readOGR(dsn = filepath, layer = "point_pixel_used")
ogrInfo(dsn = filepath, layer = "point_pixel_used")

## Read in CSV files
csv_templates <- list.files(path = filepath, pattern = "HUC8", full.names = TRUE)
csv_templates

## Merge all csv files into 1
csv_file <- read.csv(csv_templates[1])

for (i in 2:length(csv_templates)){
  HUC <- read.csv(csv_templates[i])
  csv_file <- rbind(csv_file, HUC)
}
# Contains everything but NA file 

names(csv_file)[11] <- "def.sum.climatology"
names(csv_file)[19] <- "nlcd.2001"

## Create an empty vector where you can store the names of all matching raster and csv files
all_equal_files <- character(0)

for (i in 4:length(csv_file)) {
  pattern_match <- names(csv_file)[i]
  pattern_match
  rasters <- list.files(path = filepath, pattern = pattern_match, full.names = TRUE)
  rasters ## character(0) for [3], in case of later loop, make an if statement to check if there were any files retrieved
  
  if(!is.empty(rasters)){ # If you can find a matching tiff file to the column in our csv file...
    ## Read in the raster
    imported_raster <- raster(rasters[1]) 
  }else {
    break
  }
  
  ## Prep pixel_id values
  shapedata <- shapefile@data
  names(shapedata)[1] <- "pixel_id"
  
  ## Extract values at specific points in the raster
  shapedata <- data.frame(shapedata, raster::extract(imported_raster, shapefile))
  names(shapedata)[2] <- "raster_data"
  
  ## Join values in csv and values in shapedata by pixel_id
  joined <- csv_file %>%
    dplyr::select(pixel_id, pattern_match)
  
  joined <- left_join(joined, shapedata, by = "pixel_id")
  
  ## Compare shapedata csv values and raster points to see if they match
  ## Current value in point shapefile should match the pixel id in the csv
  if (all.equal(joined[, pattern_match], joined[, "raster_data"])) {
    all_equal_files <- c(all_equal_files, pattern_match)
  }
}


## REPORT:
## [3] "UMHbasins.aYs.2005" -- Could not find a matching tiff raster file
## [4] "CHILI.UMH.30" EQUAL
## [5] "def.sum.2000" EQUAL
## [6] "def.sum.2001" EQUAL
## ... ALL ELSE EQUAL





