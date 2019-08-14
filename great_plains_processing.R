# library(tools)
# library(foreach)
library(raster)
library(sf)
# library(ncdf4)
# library(doParallel)
library(RCurl)
library(eddi) # a datasource
library(dplyr)
library(googledrive)

#using this tutorial as reference
#https://rpubs.com/ricardo_ochoa/416711


#############################################

### reading in raster files ###

#############################################
#https://www.earthdatascience.org/eddi/index.html
#date and timescale are examples. are easily translated to a function/loop
eddi_data <- eddi::get_eddi(date = '2018-11-29', timescale = '1 month')


#https://esrl.noaa.gov/psd/leri/#archive
  # try to read direct from ftp
url <- 'ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/data/2019/'
filenames <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
  stringr::str_split('\n') %>% unlist

#[1] is just an example. can easily generalize this to a loop if we want all files, but very expensive
#downloads all the files into a temp location
leri_filepath <- paste(tempdir(), filenames[1], sep = '/')
leri_filepath <- file.path(tempdir(), filenames[1])
utils::download.file(paste0(url, filenames[1]), destfile = leri_filepath)

leri_data <- raster::stack(leri_filepath)
#file.remove(leri_filepath)

#https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi
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

### get rasters all in same format? ###

#############################################

# Step 1. Put the rasters in a list and convert them to a consistent coordinate system (EPSG 5070)

stack_list <- list(eddi_data, leri_data)
stack_list_epsg <- purrr::map(stack_list, ~raster::projectRaster(.x, crs = "+init=epsg:5070"))

# Step 2. Crop them all to the boundary shape via masking

stack_list_cropped <- purrr::map(stack_list_epsg, ~raster::mask(.x, domain_shapefile) %>%
                                  raster::crop(raster::extent(domain_shapefile)))


# Step 3. Pick the one with finest resolution (this is our template, which we will use to match resolution/extent)

pixel_area_list <- purrr::map(stack_list_cropped, ~ prod(raster::res(.x)))
template <- stack_list_cropped[[which.min(pixel_area_list)]]


# Step 4. Match resolution/match extent even closer between the stacks and the template

stack_list_uniform <- purrr::map(stack_list_cropped, ~raster::resample(.x, template))












#############################################

### cropping the raster ###

#############################################
#' @param stack should be a rasterstack object
#' @param domain_shape should be a shapefile that specifies the domain (must have consistent crs)
#' @param resolution should be the the minimum resolution of all the stacks (a 2 element numeric vector for x,y)
#' @param espg should be the espg code to use as the crs (4 numbers)
project_mask_crop_save <- function(stack, domain_shape, espg, resolution){
  #rasters need espg in a different format than shapefiles
  espg_crs <- paste0("+init=espg:",espg)
  projected_stack <-  raster::projectRaster(stack, crs = espg_crs, res = resolution)
  
  #create a mask (eg create a new raster by overlaying the eddi_data values onto the domain shapefile)
  mask <- raster::mask(projected_stack, domain_shape)
  #now crop this mask so that it fits into the box that we define earlier
  cropped <- raster::crop(mask, raster::extent(domain_shape))
  
  raster::writeRaster()
}

#test
leri_cropped <- project_mask_crop_save(leri_data, domain_shapefile, 5070, resolution)

raster::plot(leri_cropped)







