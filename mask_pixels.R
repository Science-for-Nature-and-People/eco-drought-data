library(stringr)
library(raster)
library(data.table)


# Threshold

threshold_mask <- -0.036
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
input_rasters <- list.files(path = input_dir_full, pattern = "*_clip.tif", full.names = TRUE)
NDVI_trends_file <-"UMHbasins_aYs_2005_2006_jDs_200_250_epoch5_NDVI_landsat_TDDlm2.tif"
NDVI_trends_file_full <- file.path(response_dir, NDVI_trends_file)
huc8_file <- "huc8_umh.tif"
huc8_file_full <- file.path(domain_dir,"UMH_HUC8", huc8_file)
input_rasters_all <- c(huc8_file_full, NDVI_trends_file_full, input_rasters)

# load the raster created by multiplying the trend by the R2
response_file <- "NDVI_R2lm2.tif"
response_combined <- raster(file.path(response_dir, response_file))

# Look at the histogram
# hist(response_combined[])

# Do some plotting
# plot(response_combined)
# cuts_trends <- c(-0.05,-0.025,-0.01,0,0.01,0.025,0.05) 
# plot(response_combined, breaks=cuts_trends, col = pal(7))
# cuts_trends <- c(-0.05,-0.036) 
# plot(response_combined, breaks=cuts_trends, col = pal(3))

## Create the mask for the threshold
# drought pixels
drought_mask <- response_combined
drought_mask[drought_mask > threshold_mask] <-NA 
# Get the number of pixels left represents
# sum(!is.na(drought_mask[]))

# No trend pixels
no_trend_mask <- response_combined
no_trend_mask[no_trend_mask < -0.000000001]  <-NA 
no_trend_mask[no_trend_mask > 0.000000001]  <-NA 

image(no_trend_mask)
# sum(!is.na(no_trend_mask[]))


# Initialize the data.table
DTmask <- data.table(pixel_id = 1:nb_pix)
# Set the keys
setkey(DTmask,pixel_id)
# Create a binary column where pixel with value are TRUE
DTmask[, mask_drought:=!is.na(getValues(drought_mask))]
DTmask[, mask_notrend:=!is.na(getValues(no_trend_mask))]
# List the values
keys_id1 <- DTmask[mask_drought==TRUE, pixel_id]
keys_id2 <- DTmask[mask_notrend==TRUE, pixel_id]
keys_id <- sort(c(keys_id1,keys_id2))

# Initialize the dataframe
DT <- data.table(pixel_id = keys_id)
setkey(DT, pixel_id)

# Loop trhough the variable
for(raster_file in input_rasters_all){
  # Read the raster in
  r <- raster(raster_file)
  # Get the variable name
  var_name_long <- gsub(".tif","", basename(raster_file))
  var_name_long <- gsub("_epsg5070_clip","", var_name_long)
  var_name_split <- str_split(var_name_long,"_", simplify = TRUE)
  # Detect numeric part in the name (years)
  suppressWarnings(nc <- !is.na(as.numeric(var_name_split)))
  # if there is a numeric part
  if (sum(nc) > 0) {
    if (length(var_name_split) > 2) {
      var_name <- paste(var_name_split[1], var_name_split[2], var_name_split[nc][1], sep = ".")
    } else {
      var_name <- paste(var_name_split[1], var_name_split[2], sep = ".")
    }
  } else {
    var_name <- paste(var_name_split[1], var_name_split[2], sep = ".")
  }
  # Get the values from raster and add them as a column
  # cat(var_name, "\n")
  DT[, (var_name):=r[keys_id]]
}

# Write the main table
data.table::fwrite(DT, file.path(input_dir_full,"model_inputs.csv"))

# Fix the header



# Split the table by HUC

list_huc_data <- split(DT, by=c("huc8.umh"))

list_huc_data[["NA"]] <- NULL

lapply(1:length(list_huc_data), function(i) write.csv(list_huc_data[[i]], 
                                                file = file.path(input_dir_full, paste0("HUC8_",names(list_huc_data[i]), ".csv")),
                                                row.names = FALSE))


