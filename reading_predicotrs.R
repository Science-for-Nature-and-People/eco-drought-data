# Load libraries
library(sf) # st_read()
# devtools::install_github("tidyverse/ggplot2")
library(ggplot2) # ggplot()
library(raster) # raster()
library(ncdf4) # nc_open(), ncvar_get()
library(RColorBrewer) # brewer.pal()
library(data.table) # as.data.table()

predict_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir  <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain"

## Domain ----

huc8_file <- "UMH_HUC8/wbdhu8_a_10020001.shp"
umh_file  <- "UMH_Basins_outside_boundary_shapefile/UMH_Basins_outside_boundary.shp"

# Read Domain data
umh <- st_read(file.path(domain_dir, umh_file))
huc8_1 <- st_read(file.path(domain_dir, huc8_file))

# Plot
ggplot() +
  geom_sf(data=umh, aes(fill = HUC8)) + # Error: could not find function "geom_sf"
  geom_sf(data=huc8_1, aes(fill = HUC8)) 

## Predictors ----

swe_file  <- "shelley2.swe"
soil_file <- "shelley2.soil"

# Read Predictor data
swe_data <- as.data.table(read_table(file.path(predict_dir,swe_file),col_names = TRUE))
soil_data <- as.data.table(read_table(file.path(predict_dir,soil_file),col_names = TRUE))

## necf:                                            ||  tmp_variable:
# "tmax_anomalies_2000_MissouriHeadwaters_JJA.nc"   ||  "tmax"
# "tmin_anomalies_2000_MissouriHeadwaters_DJF.nc"   ||  "tmin"
# "Precip_AnnualAnomalies2000.nc"                   ||  "precip"
# "EDDI_MJJAS_2000.nc"                              ||  "EDDI"

view_netcf <- function(netcf, tmp_variable) {
  ncin <- nc_open(file.path(predict_dir,netcf))
  lon <- ncvar_get(ncin, varid = "lon")
  lat <- ncvar_get(ncin, varid = "lat")
  tmp_array <- ncvar_get(ncin, tmp_variable)
  image(lon,lat,tmp_array, col=rev(brewer.pal(10,"RdBu")))
}

# Raster climatic water deficit
def_sum <- raster(file.path(predict_dir, "defSum", "def_sum_1980.tif")) # File does not exist
image(def_sum)
def_sum
