library(raster)
library(gdalUtils)


## CONSTANTS ----

# Main directories
predict_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Predictors"
domain_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Domain"
response_dir <- "/home/shares/ecodrought/VulnerabilityAnalysis/Response"

out_dir <- "ProjectClipMask"
out_dir_full <- file.path(predict_dir, out_dir)

# read the raster and stack
def_sum_files <- list.files(path = predict_dir, pattern = "def_sum", full.names = TRUE)
def_stack8009 <- stack(def_sum_files)
names(def_stack8009)

# Compute the climatoloty
def_climatology <- as.integer(mean(def_stack8009, na.rm = TRUE))


# Compute the anomalies
def_drought0005 <- subset(def_stack8009, 21:26)
names(def_drought0005)

def_anomalies0005 <- as.integer(def_drought0005 - def_climatology)

# Reproject
data_raster_5070_epsg5070 <- projectRaster(def_anomalies0005, ndvi_trends, method="ngb")

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
# fix the names
names(data_raster_5070_epsg5070) <- names(def_drought0005)

# write the output
writeRaster(data_raster_5070_epsg5070, 
            filename=file.path(out_dir_full, paste0(names(data_raster_5070_epsg5070),"_anom_epsg5070_masked.tif")), 
            bylayer=TRUE,
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite=TRUE)
