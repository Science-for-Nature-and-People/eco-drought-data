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
#leri_filepath <- paste0('/Users/nathan/Downloads/', filenames[1])
#downloads all the files into a temp location
leri_filepath <- tempfile(fileext = '.zip')
utils::download.file(paste0(url, filenames[1]), destfile = leri_filepath)

leri_data <- raster::stack(leri_filepath)


#https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi
#???


#############################################

### get rasters all in same format? ###

#############################################

#use the finest resolution
resolution <- pmin(raster::res(eddi_data), raster::res(leri_data))

#convert raster to same as above
new_eddi <- raster::projectRaster(eddi_data, crs = raster::crs(leri_data), res = resolution)








#############################################

### the box we're limiting our rasters to ###

#############################################
#domain_shapefile_path <- '/Users/nathan/Downloads/CNS_GP_RMDSW/CNS_RMDSW_studyarea.shp'

#set a constant coordinate reference system (EPSG:5070) so all our objects are comparable
EPSG_conus_alb_83 <- 5070

#create temp file path to hold the file.
path <- tempfile(fileext = '.zip')
nceas_drive_dribble <- googledrive::team_drive_get(name = 'NCEAS-SciComp')
googledrive::drive_find('CNS_GP_RMDSW.zip', team_drive = nceas_drive_dribble) %>%
  googledrive::drive_download(overwrite = TRUE, path = path)
cns_files <- unzip(path, exdir = tempdir())

#the shapefile we need has name "CNS_GP_states.shp. we extract it and read it into r
domain_shapefile_path <- cns_files[stringr::str_which(cns_files, 'CNS_GP_states.shp$')]
domain_shapefile <- sf::st_read(domain_shapefile_path) %>%
  sf::st_transform(crs = raster::crs(leri_data))

#plot(domain_shapefile$geometry)




#############################################

### cropping the raster ###

#############################################
#' @param data should be a rasterstack object
#' @param domain_shape should be a shapefile that specifies the domain (must have consistent crs)
mask_and_crop <- function(data, domain_shape){
  #create a mask (eg create a new raster by overlaying the eddi_data values onto the domain shapefile)
  mask <- raster::mask(data, domain_shape)
  #now crop this mask so that it fits into the box that we define earlier
  raster::crop(mask, raster::extent(domain_shape))
}

eddi_cropped <- mask_and_crop(eddi_data, domain_shapefile)
leri_cropped <- mask_and_crop(leri_data, domain_shapefile)


