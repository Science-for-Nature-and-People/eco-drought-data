library(tools)
library(foreach)
# library(rgdal)
library(raster)
# library(ggplot2)
# library(rgeos)
# library(mapview)
# library(leaflet)
# library(broom) # if you plot with ggplot and need to turn sp data into dataframes
library(sf)

library(doParallel)
registerDoParallel(12)

options(stringsAsFactors = FALSE)


### CONSTANTS ----

## Path
predict_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain"
response_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Response"

predictor_files <- c("nlcd_2001_impervious_2011_edition_2014_10_10/nlcd_2001_impervious_2011_edition_2014_10_10.img",
                     "nlcd2001_canopy_mosaic_1-29-08/nlcd_canopy_mosaic_1-29-08.img",
                     #"lf3_30T_GNLCC/lf3_30T_GNLCC.tif",
                     "UMH/evh/w001001.adf",
                     "UMH/evt/w001001.adf")

out_dir <- "ProjectClipMask"
out_dir_full <- file.path(predict_dir, out_dir)
# Create the directory
dir.create(out_dir_full, showWarnings = FALSE)

## Shapefile
# Domain
Domain_shp_file <- file.path(domain_dir,"UMH_Basins_outside_boundary_shapefile/UMH_Basins_outside_boundary.shp")

## Raster
# Response
NDVI_trends_file <- "UMHbasins_aYs_2005_2006_jDs_200_250_epoch5_NDVI_landsat_TDDlm1.tif"
# lf3_file <- "lf3_30T_GNLCC/lf3_30T_GNLCC.tif"

## CRS
EPSG_conus_alb_83 <- 5070
rough_extent <- extent(-115, 42, -108, 48)
  
### MAIN ----
#read domain
domain_shp <- st_read(Domain_shp_file)
st_bbox(domain_shp)

# reproject to conus albers NAD 83
domain_shp_83 <- st_transform(domain_shp, EPSG_conus_alb_83)
st_bbox(domain_shp_83)
        
# Output layer
st_write(domain_shp_83, 
         dsn = file.path(domain_dir,"UMH_Basins_outside_boundary_shapefile/UMH_Basins_NAD83_outside_boundary.shp"),
         driver = "ESRI Shapefile", delete_layer = TRUE
         )

# Load the trnds to be used as mask
ndvi_trends <- raster(file.path(response_dir,NDVI_trends_file))

# processing of the layers in parallel (slow processing; check gdalwrap )
foreach(i=1:length(predictor_files)) %dopar% {
  data_file <- predictor_files[i]
  # Load the file to be masked
  data_raster <- raster(file.path(predict_dir, data_file))
  # Reproject and clip
  data_raster_5070 <- projectRaster(data_raster, ndvi_trends, method = 'ngb')
  # mask tree only
  masked <- mask(data_raster_5070, ndvi_trends)
  # Build ouptut name
  if (file_ext(data_file) == "adf"){
    parts <- str_split(data_file,"/")[[1]]
    file_name <- paste(parts[1],parts[2],sep="_")
    outputfile_name <- paste0(file_name,"_epsg5070_masked.tif")
  }else{
    outputfile_name <- basename(paste0(file_path_sans_ext(data_file), "_epsg5070_masked.tif"))
  }
  outputfile_name_full <- file.path(out_dir_full,outputfile_name)
  # outputfile_name_full
  writeRaster(masked, filename=outputfile_name_full, format="GTiff", overwrite=TRUE, options="COMPRESS=LZW")
}
