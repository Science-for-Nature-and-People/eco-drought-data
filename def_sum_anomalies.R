library(raster)
library(gdalUtils)


## CONSTANTS ----

## Path
predict_dir  <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir   <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain"
response_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Response"
def_sum_dir  <- file.path(predict_dir,"defSum")

## Output
out_dir <- file.path(predict_dir, "ProjectClipMask")

NDVI_trends_file <- "UMHbasins_aYs_2005_2006_jDs_200_250_epoch5_NDVI_landsat_TDDlm2.tif"
ndvi_trends <- raster(file.path(response_dir,NDVI_trends_file))

# Read the raster and stack
def_sum_files <- list.files(path = def_sum_dir, pattern = "*.tif$", full.names = TRUE)
def_stack8009 <- stack(def_sum_files)
names(def_stack8009)

# Compute the climatology
def_climatology <- mean(def_stack8009, na.rm = TRUE)
def_climatology[] <- as.integer(def_climatology[])

# Reproject
# projectRaster(def_climatology, ndvi_trends, method="ngb", 
#               filename=file.path(out_dir_full, "def_sum_climatology_epsg5070_clip.tif"),
#               format="GTiff",
#               options="COMPRESS=LZW",
#               overwrite=TRUE)

# Compute the anomalies
def_drought0005 <- subset(def_stack8009, 21:26)
names(def_drought0005)

def_anomalies0005 <- def_drought0005 - def_climatology

# Reproject
data_raster_5070_epsg5070 <- projectRaster(def_anomalies0005, ndvi_trends, method="ngb")

## GDAL approch for reprojection
# data_raster_5070_epsg5070 <- gdalwarp(srcfile = def_anomalies0005,
#          dstfile = file.path(out_dir_full,"tmp.tif"),
#          s_srs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ",
#          t_srs = "EPSG:5070",
#          te = c(-1389630,2464260,-1143270,2711370),
#          tr = c(30,30),
#          r = "near",
#          # wm = 100000,
#          multi = TRUE,
#          output_Raster=TRUE,
#          overwrite=TRUE,
#          verbose=TRUE
# )

# Fix the names
names(data_raster_5070_epsg5070) <- names(def_drought0005)

# Write the output
writeRaster(data_raster_5070_epsg5070, 
            filename=file.path(out_dir, paste0(names(data_raster_5070_epsg5070),"_anom_epsg5070_clip.tif")), 
            bylayer=TRUE,
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite=TRUE)
