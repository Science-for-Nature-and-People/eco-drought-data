# library(tools)
# library(foreach)
library(raster)
library(sf)
# library(ncdf4)
# library(doParallel)
library(RCurl)
library(eddi) # a datasource
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(googledrive)
library(doParallel)
registerDoParallel(6)

#using this tutorial as reference
#https://rpubs.com/ricardo_ochoa/416711


#############################################

### reading in raster files ###

#############################################

### Read in EDDI Data (https://www.earthdatascience.org/eddi/index.html) ==========================================================

## Single case (for simple, easily runnable example) ------------------------------------

eddi_data <- eddi::get_eddi(date = '2018-01-01', timescale = '1 month') 


## Function to read in many -----------------------------------------------------------

#' Read in monthly EDDI data. 
#' @param year is a string/int with the desired start year. Data is availble since 1980, but we default to 2000 to match with LERI data
#' @param scale see the timescale argument in the `eddi::get_eddi()`. Default to 1 month. note LERI data only has 1 month, 3 month, 12 months
#' @return A list of single-layer raster stacks, with each element representing a month starting from `year` to the latest full month
getMonthlyEDDI <- function(year = 2000, scale = '1 month'){
  if(nchar(year) != 4 | year < 1980 | year > lubridate::year(today())){
    stop('Year must be 4 digits')
  }
  start <- paste(year, "01", "01", sep = "-") %>% ymd()
  #for some reason, they don't let you put in 01/01/1980, so we add a day. hopefully 1/1/1980 wasn't a crazy day
  if(year == 1980){
    start <- start + days(1)
  }
  # Subtract a month from the end to make sure data is available
  end <- lubridate::today() - months(1)
  dates <- seq(start,end, by = '1 month')
  
  map(dates, ~eddi::get_eddi(date = .x, timescale = scale))
}


### Read in LERI Data (https://esrl.noaa.gov/psd/leri/#archive) ==========================================================

## Single case (for simple, easily runnable example) ----------------------------------

url <- 'ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/data/2019/'
filenames <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
  stringr::str_split('\n') %>% 
  unlist()

# what to save the new file as
new_leri_name <- paste(tools::file_path_sans_ext(filenames[1]))

#downloads file into a temp location
leri_filepath <- file.path(tempdir(), filenames[1])
utils::download.file(paste0(url, filenames[1]), destfile = leri_filepath)

# get it in a stack instead of a layer just to match format with eddi data
leri_data <- raster::stack(leri_filepath)
#file.remove(leri_filepath)



## Functions to read in many -----------------------------------------------------------

# It doesn't look like it would be any faster to parallelize ftp downloads, since the network bandwidthi is the limiting factor
# See for a possible alternative: https://stackoverflow.com/questions/16783551/downloading-multiple-file-as-parallel-in-r
 
#' Take a filename for a raster layer and read it into R as a stack
#' 
#' this is mostly just a helper function for getLERI
#' @param filename is a string containing a raster's full path 
filenameToStack <- function(filepath){
  filename <- filepath %>%
    basename %>%
    tools::file_path_sans_ext()
  path <- file.path(tempdir(), filename)
  utils::download.file(filepath, destfile = path)
  rast <- raster::stack(path)
  rast@layers[[1]]@data@names <- filename
  
  # The newer leri rasters (eg all in 2019) have a crs. the older ones don't. But I suspect the extent is the same between all of them.
    # This is the extent (in matrix form) of one form 2019. If the extent of each raster is the same as this, I'll set the crs to be the same
    # It's in matrix form since I couldn't find a way to compare Extent objects (identical and == didn't work)
  recent_leri_extent <- structure(c(-126.009, 23.949, -66.456, 49.545), 
                                  .Dim = c(2L, 2L), 
                                  .Dimnames = list(c("x", "y"), c("min", "max")))
  recent_leri_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  if(identical(as.matrix(rast@layers[[1]]@extent), recent_leri_extent)){
    rast@crs@projargs <- recent_leri_crs
  }
  
  rast
}

#' Download and read in leri files as a list of raster stacks
#' @param year is a specific year we want data for. min is 2000, max is 2019 (But 2019 doesn't have 12 mn data). default is NULL (download all)
#' @param scale is the timescale we want for each raster. options are "1 month", "3 month", "7 month", and "12 month"
getLERI <- function(year = NULL, scale = ""){
  if(!is.null(year)){
    url <- paste0('ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/data/', year, '/')
  } else {
    all_years <- 2000:2019
    url <- map_chr(all_years, ~paste0('ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/data/', .x, '/'))
  }
  
  if(!(scale %in% c("1 month", "3 month", "7 month", "12 month", ""))){
    stop('scale must be "1 month", "3 month", or "12 month", or empty string')
  } 
  timescale <- scale %>%
    str_replace("month", "mn") %>%
    str_remove(" ")
  
  # create a list, where each element is a character vector of filepaths corresponding to a year
  filenames <- map(url, ~RCurl::getURL(url = .x, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
                     stringr::str_split('\n') %>% 
                     unlist() %>%
                     setdiff("") %>%
                     paste0(.x, .) %>%
                     stringr:str_subset(timescale)
                
  )
  
  #filenameToStack works on individual filenames, but filenames is a list of vectors. map_depth lets us get the individual filenames                       
  map_depth(filenames, 2, ~filenameToStack(.x))
}




### Read in SPI Data (https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi) ==========================================================
#???






#############################################

### the box we're limiting our rasters to ###

#############################################
#domain_shapefile_path <- '/Users/nathan/Downloads/CNS_GP_RMDSW/CNS_RMDSW_studyarea.shp'

#create temp file path to hold the file.
path <- tempfile(fileext = '.zip')
nceas_drive_dribble <- googledrive::team_drive_get(name = 'NCEAS-SciComp')
googledrive::drive_find('CNS_GP_RMDSW.zip', team_drive = nceas_drive_dribble) %>%
  googledrive::drive_download(overwrite = TRUE, path = path)
cns_files <- unzip(path, exdir = tempdir())

#the shapefile we need has name "CNS_GP_states.shp. we extract it and read it into r
domain_shapefile_path <- cns_files[stringr::str_which(cns_files, 'CNS_GP_states.shp$')]

domain_shapefile <- sf::st_read(domain_shapefile_path) %>%
  sf::st_transform(crs=5070)

#plot(domain_shapefile$geometry)







#############################################

### get rasters all in same format ###

#############################################

#put all the raster stacks into a list
stack_list <- list(eddi_data, leri_data)


### this code generates stack_list, but on EVERYTHING (and will take forever)

# full_leri <- getLERI() %>%
#   unlist
# full_eddi <- getMonthlyEDDI()
# stack_list <- c(full_leri, full_eddi)

#' Function to project raster to coordinate system EPSG:5070, and name the projected raster with the old name
#' @param rast is a RasterStack with only 1 layer
#' @return A rasterLayer, projected onto CRS EPSG:5070
project_and_name <- function(rast){
  projected <- raster::projectRaster(rast, crs = "+init=epsg:5070")
  projected@data@names <- rast@layers[[1]]@data@names
  projected
}



##### Step 1: Check resolution first
#' Check that resolutions are comparable between two rasters
#' @param rast are rasters (stack or layer). 
#' @return TRUE/FALSE depending on whether the proj and datum match or not
compare_proj_datum <- function(rast1, rast2){
  proj1 <- rast1@crs@projargs %>%
    stringr::str_extract("\\+proj=\\w+")
  proj2 <- rast2@crs@projargs %>%
    stringr::str_extract("\\+proj=\\w+")
  
  datum1 <- rast1@crs@projargs %>%
    stringr::str_extract("\\+datum=\\w+")
  datum2 <- rast2@crs@projargs %>%
    stringr::str_extract("\\+datum=\\w+")
  
  (proj1 == proj2) & (datum1 == datum2)
}

# Checking a few values of each, we're pretty sure the proj/datums match. but we make sure
if(purrr::reduce(stack_list, compare_proj_datum)){
  pixel_area_list <- purrr::map(stack_list, ~ prod(raster::res(.x)))
  template_raw <- stack_list[[which.min(pixel_area_list)]]
} else {
  stop('check proj/datums')
}

#### Step 2: project and crop the template
template_cropped <- template_raw %>%
  project_and_name() %>%
  raster::crop(raster::extent(domain_shapefile)) %>%
  raster::mask(domain_shapefile)


### Step 3: reproject everything to the template
foreach::foreach(i=1:length(stack_list)) %dopar% {
  raster::projectRaster(stack_list[[i]], template_cropped,
                        filename = paste(stack_list[[i]]@layers[[1]]@data@names, "EPSG5070_cropped.tif", sep = '_'),
                        format = "GTiff", overwrite=TRUE, options="COMPRESS=LZW")
}





#### Deprecated =================================================================

#############################################

### get rasters all in same format? ###

#############################################

# Step 1. Put the rasters in a list and convert them to a consistent coordinate system (EPSG 5070) ======================
# note: we might need to split this up to do eddi data separately from leri


# stack_list_epsg <- purrr::map(stack_list, ~project_and_name(.x))
# 
# # Step 2. Crop them all to the boundary shape via masking ================================================================
# 

# stack_list_cropped <- purrr::map(stack_list_epsg, ~raster::crop(.x, raster::extent(domain_shapefile)) %>%
#                                     raster::mask(domain_shapefile)
# )
# 
# 
# # Step 3. Pick the one with finest resolution (this is our template, which we will use to match resolution/extent) ========
# 
# pixel_area_list <- purrr::map(stack_list_cropped, ~ prod(raster::res(.x)))
# template <- stack_list_cropped[[which.min(pixel_area_list)]]
# 
# 
# # Step 4. Match resolution/match extent even closer between the stacks and the template ====================================
# 
# stack_list_uniform <- purrr::map(stack_list_cropped, ~raster::resample(.x, template,
#                                                                        filename = paste(.x@data@names, "EPSG5070_cropped.tif", sep = '_'),
#                                                                        format = "GTiff", overwrite=TRUE, options="COMPRESS=LZW")
#                                  )
# 


#############################################

### cropping the raster ###

#############################################
#' @param stack should be a rasterstack object
#' @param domain_shape should be a shapefile that specifies the domain (must have consistent crs)
#' @param resolution should be the the minimum resolution of all the stacks (a 2 element numeric vector for x,y)
#' @param espg should be the espg code to use as the crs (4 numbers)
# project_mask_crop_save <- function(stack, domain_shape, espg, resolution){
#   #rasters need espg in a different format than shapefiles
#   espg_crs <- paste0("+init=espg:",espg)
#   projected_stack <-  raster::projectRaster(stack, crs = espg_crs, res = resolution)
#   
#   #create a mask (eg create a new raster by overlaying the eddi_data values onto the domain shapefile)
#   mask <- raster::mask(projected_stack, domain_shape)
#   #now crop this mask so that it fits into the box that we define earlier
#   cropped <- raster::crop(mask, raster::extent(domain_shape))
#   
#   raster::writeRaster()
# }
# 
# #test
# leri_cropped <- project_mask_crop_save(leri_data, domain_shapefile, 5070, resolution)
# 
# raster::plot(leri_cropped)







