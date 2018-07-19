library(data.table)
library(stringr)
library(raster)
library(RColorBrewer)

rasterOptions(maxmemory = 1e10)

### CONSTANTS -----
# Number of pixels
nb_pix <- 67642244

## Path
predict_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain"
response_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Response"

# Create the path to the data
input_dir_full <- file.path(predict_dir,"ProjectClipMask")

# List all the clipped datasets
input_rasters <- list.files(path = input_dir_full, pattern = ".tif$", full.names = TRUE, )


## Create a list of ids with values eg tree area
# Response
NDVI_trends_file <- "UMHbasins_aYs_2005_2006_jDs_200_250_epoch5_NDVI_landsat_TDDlm2.tif"
NDVI_R2_file <- "UMHbasins_aYs_2005_2006_jDs_200_250_epoch5_NDVI_landsat_TDDlr2R2.tif"

ndvi_trends <- raster(file.path(response_dir,NDVI_trends_file))
NDVI_R2 <- raster(file.path(response_dir,NDVI_R2_file))


### MAIN -----
## Plots
#R2
hist(NDVI_R2[])
cuts=c(0.6,0.75,0.8)
pal <- colorRampPalette(c("blue","red"))
plot(NDVI_R2)
plot(NDVI_R2, breaks=cuts, col = pal(3), add=TRUE)
#trends
hist(ndvi_trends[])
plot(ndvi_trends)
cuts_trends <- c(-1,-0.5,-0.25,0,0.25,0.5,1)
plot(ndvi_trends, breaks=cuts_trends, col = pal(7))


# Initialize the data.table
DTmask <- data.table(pixel_id = 1:nb_pix)
# Set the keys
setkey(DTmask,pixel_id)
# Create a binary column where pixel with value are TRUE
DTmask[, mask:=!is.na(getValues(ndvi_trends))]
# List the values
keys_id <- DTmask[mask==TRUE, pixel_id]

# Initialize the dataframe
DT <- data.table(pixel_id = keys_id)
setkey(DT, pixel_id)

for(raster_file in input_rasters){
  print(raster_file)
  # Read the raster in
  r <- raster(raster_file)
  # Get the variable name
  var_name_long <- gsub("_epsg5070_masked.tif","",basename(raster_file))
  var_name_split <- str_split(var_name_long,"_")[[1]]
  var_name <- paste(var_name_split[1], var_name_split[2], sep = ".")
  # Get the values from raster and add them as a column
  DT[, (var_name):=r[keys_id]]
}

data.table::fwrite(DT, file.path(input_dir_full,"model_inputs_201807.csv"))
