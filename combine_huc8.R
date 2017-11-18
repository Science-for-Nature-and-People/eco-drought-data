library(raster)
library(sf)

## CONSTANTS ----

# Main directories
predict_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain"
response_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Response"

# Full path to HUC8 shapefiles
HUC8_dir_full <- file.path(domain_dir,"UMH_HUC8")

# Raster grid to match
NDVI_trends_file <- "UMHbasins_aYs_2005_2006_jDs_200_250_epoch5_NDVI_landsat_TDDlm2.tif"

# Projection
EPSG_conus_alb_83 <- 5070

## FUNCTIONS ----

#' Combine a list of non overlaping shapefiles into one
#'
#' @param top_dir a character string to the directory containing the shapefiles to merge
#'
#' @return simple feature object
#' @export
#'
#' @examples combine_shapefiles("/home/shares/ecodrought/VulnerabilityAnalysis/Domain/UMH_HUC8")
combine_shapefiles <- function(top_dir){
  # List all the cliped datasets
  shp_files <- list.files(path = top_dir, pattern = "*.shp", full.names = TRUE)
  # Read the data in a list
  shp_List <- lapply(shp_files, st_read)
  # Combine the data
  shp_all <-  do.call("rbind",shp_List)
  return(shp_all)
}


## MAIN ----

# Combine shapefiles
umh_huc8_all <- readOGR(HUC8_dir_full, "umh_huc8_all")

# Reproject
umh_huc8_all_epsg_5070 <- spTransform(umh_huc8_all, crs(ndvi_trends))


# Rasterize
ndvi_trends <- raster(file.path(response_dir, NDVI_trends_file))
huc8_rast   <- rasterize(umh_huc8_all_epsg_5070, ndvi_trends,
                         filename=file.path(HUC8_dir_full, "umh_huc8.tif"), overwrite=TRUE)

# rclm <-matrix(data = NA, nrow = 8, ncol = 3, byrow = FALSE,
#        dimnames = NULL)
# rclm[,1]<- 1:8
# rclm[,2]<- 1:8
# rclm[,3]<- 1:8umh_huc8_all_epsg_5070@data$HUC8
