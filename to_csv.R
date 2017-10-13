library(data.table)
library(stringr)
library(raster)
rasterOptions(maxmemory = 1e10)

# Number of pixels
nb_pix <- 67642244

## Path
predict_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain"
response_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Response"

# Create the path to the data
input_dir <- "ProjectClipMask"
input_dir_full <- file.path(predict_dir,input_dir)

# List all the cliped datasets
input_rasters <- list.files(path = input_dir_full, pattern = "*.tif", full.names = TRUE)


## Create a list of ids with values eg tree area
# Response
NDVI_trends_file <- "UMHbasins_aYs_2005_2006_jDs_200_250_epoch5_NDVI_landsat_TDDlm1.tif"
ndvi_trends <- raster(file.path(response_dir,NDVI_trends_file))

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
  # Read the raster in
  r <- raster(raster_file)
  # Get the variable name
  var_name_long <- gsub("_epsg5070_masked.tif","",basename(raster_file))
  var_name_split <- str_split(var_name_long,"_")[[1]]
  var_name <- paste(var_name_split[1], var_name_split[2], sep = ".")
  # Get the values from raster and add them as a column
  DT[, (var_name):=r[keys_id]]
}

data.table::fwrite(DT, file.path(input_dir_full,"model_inputs.csv"))
