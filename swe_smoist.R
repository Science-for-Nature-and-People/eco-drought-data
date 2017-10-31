library(readr)
library(data.table)
library(sf)
library(dplyr)
library(sp)

devtools::install_github("ecohealthalliance/fasterize")
library(fasterize)

#### CONSTANT ----

year_start <- 2001
year_end <- 2005

predict_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain/"
## Output directory
out_dir <- "ProjectClipMask"
out_dir_full <- file.path(predict_dir, out_dir)

# Predicors
swe_file <- "shelley2.swe"
soil_file <- "shelley2.soil"


NDVI_trends_file <- "UMHbasins_aYs_2005_2006_jDs_200_250_epoch5_NDVI_landsat_TDDlm2.tif"
ndvi_trends <- raster(file.path(response_dir,NDVI_trends_file))

#hrus shapefile
hrus_cleaned <- file.path(predict_dir,"hrus_10U_umh_c2.shp")
hrus

#### FUNCTION ----

#'Return a string giving the water year for a date
#'Source: https://r-forge.r-project.org/scm/viewvc.php/pkg/R/water.year.R?view=markup&revision=33&root=iha
#'Returns a number specifying the water year for a date.
#'@param x a date-time object which can be handled by lubridate
#'@importFrom lubridate month year
#'@export
snow_year <- function(y,m){
  snowy <- ifelse(m > 9 & m <= 12 , y + 1, y)
  snowy <- ifelse(m > 4 & m <= 9 , NA, snowy)
  return(snowy)
}

#'Return a string giving the water year for a date
#'Source: https://r-forge.r-project.org/scm/viewvc.php/pkg/R/water.year.R?view=markup&revision=33&root=iha
#'Returns a number specifying the water year  for a date.
#'@param x a date-time object which can be handled by lubridate
#'@importFrom lubridate month year
#'@export
soil_moist_year <- function(y,m){
  smoisty <- ifelse(m > 4 & m <= 9 , NA, y)
  return(smoisty)
}


#### MAIN ----

# Read the shapefile in
hrus <- st_read(hrus_cleaned)

# Read  data
swe_data <- as.data.table(read_table(file.path(predict_dir,swe_file),col_names = TRUE))



## Snow ----
# Compute the snow year
swe_data[, snow_year:=snow_year(YEAR, MTH)]
# Check
# head(swe_data[ , .(YEAR, MTH, snow_year)] , 20)

# Remove NA
swe_data <- na.omit(swe_data, cols="snow_year")

# Do the aggregation
snow <- swe_data[, lapply(.SD, sum(., na.rm = TRUE)), by = "snow_year"]
# Check
head(snow[,1:10])

# Remove the year and month
snow[, c("YEAR","MTH"):=NULL]

# transpose  the table to have the Hrus as row (For join)
snowT <- dcast(melt(snow, id.vars = "snow_year"), variable ~ snow_year)

# Drop 2000 as the year is incomplete
snowT[, "2000":=NULL]
# rename
names(snowT)[1] <- "hru_id"
# Remove factors
snowT[, hru_id:=as.numeric(hru_id)]

# join
hrus_year <- merge(hrus, snowT)

for (the_year in year_start:year_end) {
  r <- fasterize(hrus_year, ndvi_trends, field = paste0("X", the_year))
  # write the output
  writeRaster(r, 
            filename=file.path(out_dir_full, paste0("swe_",the_year,"_epsg5070_clip.tif")), 
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite=TRUE)
}


## soil moisture ----

soil_data <- as.data.table(read_table(file.path(predict_dir,soil_file),col_names = TRUE))

# Compute the soil year
soil_data[ , smoist_year:=soil_moist_year(YEAR, MTH)]
# Check
# head(soil_data[ , .(YEAR, MTH, smoist_year)] , 20)

# Remove NA
soil_data <- na.omit(soil_data, cols="smoist_year")

# Do the aggregation
soilm <- soil_data[, lapply(.SD, sum(., na.rm = TRUE)), by = "smoist_year"]
# Check
head(soilm[,1:10])

# Remove the year and month
soilm[, c("YEAR","MTH"):=NULL]

# transpose  the table to have the Hrus as row (For join)
soilmT <- dcast(melt(soilm, id.vars = "smoist_year"), variable ~ smoist_year)

# Drop 2000 as the year is incomplete
# soilmT[, c("2000"):=NULL]
names(soilmT)[1] <- "hru_id"
soilmT[, hru_id:=as.numeric(hru_id)]

# join
hrus_year_sm <- merge(hrus, soilmT)

for (the_year in 2000:2005) {
  cat(the_year, "\n")
  r <- fasterize(hrus_year_sm, ndvi_trends, field = paste0("X", the_year))
  # write the output
  writeRaster(r, 
              filename=file.path(out_dir_full, paste0("soil_moisture_",the_year,"_epsg5070_clip.tif")), 
              format="GTiff",
              options="COMPRESS=LZW",
              overwrite=TRUE)
}
