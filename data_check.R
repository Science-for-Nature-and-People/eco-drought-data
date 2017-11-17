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

filepath <- "/Users/celine/Desktop/sci-comp/SNAPP"


## Read in shape file
shapefile <- readOGR(dsn = filepath, layer = "point_pixel_used")

## Read in CSV files
csv_templates <- list.files(path = filepath, pattern = "HUC8", full.names = TRUE)
csv_templates

csv_file <- csv_templates[1]
csv_file

## Merge all csv files into 1
csv <- read.csv(csv_file)

for (i in 2:length(csv_templates)){
  HUC <- read.csv(csv_templates[i])
  csv_files <- rbind(csv, HUC)
}
# Contains everything but NA file 


## Read in shapefile
ogrInfo(dsn = filepath, layer = "point_pixel_used")

shapedata <- shapefile@data

## Read in raster
str_name <- "def_sum_2000_anom_epsg5070_clip.tif"
imported_raster <- raster(str_name)

## Extract values at specific points in the raster
shapedata <- data.frame(shapedata, raster::extract(imported_raster, shapefile))

## Join values in csv and values in shapedata by pixel_id
names(shapedata)[1] <- "pixel_id"

cut_csv <- csv %>%
  select(pixel_id, def.sum.2000)

test <- left_join(cut_csv, shapedata, by = "pixel_id")

## Compare shapedata csv values and raster points to see if they match
## Current value in point shapefile should match the pixel id in the csv
identical(test$def.sum.2000, test$raster..extract.imported_raster..shapefile.)










