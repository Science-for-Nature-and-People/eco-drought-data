library(tidyverse)
library(sf)
library(sp)
# devtools::install_github("tidyverse/ggplot2")
library(ggplot2)
library(raster)
library(ncdf4)
library(RColorBrewer)
library(data.table)

predict_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain/"

# Predicors
swe_file <- "shelley2.swe"
soil_file <- "shelley.soil"


## Domain ----

huc8_file_1 <- "UMH_HUC8/wbdhu8_a_10020001.shp"
uhm_file <- "UMH_Basins_outside_boundary_shapefile/UMH_Basins_outside_boundary.shp"

# Read Domain data
umh <- st_read(file.path(domain_dir,umh_boundary_file))
huc8_1 <- st_read(file.path(domain_dir,huc8_file_1))

# plot
ggplot() +
  geom_sf(data=umh, aes(fill = HUC8)) +
  geom_sf(data=huc8_1, aes(fill = HUC8)) 


## Predicors ----
swe_file <- "shelley2.swe"
soil_file <- "shelley2.soil"

# Read Predictor data
swe_data <- as.data.table(read_table(file.path(predict_dir,swe_file),col_names = TRUE))
soil_data <- as.data.table(read_table(file.path(predict_dir,soil_file),col_names = TRUE))


# Read the netcf tmax
ncin <- nc_open(file.path(predict_dir,"tmax_anomalies_2000_MissouriHeadwaters_JJA.nc"))
ncin2 <- brick(file.path(predict_dir,"tmax_anomalies_2000_MissouriHeadwaters_JJA.nc"), "tmax")
# get the lat/long
lon <- ncvar_get(ncin, varid = "lon")
lat <- ncvar_get(ncin, varid = "lat")
# get the variable
tmp_array <- ncvar_get(ncin,"tmax")

image(lon,lat,tmp_array, col=rev(brewer.pal(10,"RdBu")))

# Read the netcf tmin
ncin <- nc_open(file.path(predict_dir,"tmin_anomalies_2000_MissouriHeadwaters_DJF.nc"))
# get the lat/long
lon <- ncvar_get(ncin, varid = "lon")
lat <- ncvar_get(ncin, varid = "lat")
# get the variable
tmp_array <- ncvar_get(ncin,"tmin")

image(lon,lat,tmp_array, col=rev(brewer.pal(10,"RdBu")))


# Read the netcf precip
ncin <- raster(file.path(predict_dir,"Precip_AnnualAnomalies2000.nc"))
# # get the lat/long
# lon <- ncvar_get(ncin, varid = "lon")
# lat <- ncvar_get(ncin, varid = "lat")
# # get the variable
# tmp_array <- ncvar_get(ncin,"precip")
# image(lon,lat,tmp_array, col=rev(brewer.pal(10,"RdBu")))
image(ncin, col=rev(brewer.pal(10,"RdBu")))


# Read the netcf EDDI (evoparitve demand drought index)
ncin <- raster(file.path(predict_dir,"EDDI_MJJAS_2000.nc"))
# get the lat/long
# lon <- ncvar_get(ncin, varid = "lon")
# lat <- ncvar_get(ncin, varid = "lat")
# # get the variable
# tmp_array <- ncvar_get(ncin,"EDDI")
#image(lon,lat,tmp_array, col=rev(brewer.pal(10,"RdBu")))
image(ncin, col=rev(brewer.pal(10,"RdBu")))

# Raster climatic water deficit

def_sum <- raster(file.path(predict_dir, "def_sum_1980.tif"))
image(def_sum)
def_sum
