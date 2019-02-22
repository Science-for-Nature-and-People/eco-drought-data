# SNAPP Ecological Drought - Data Processing for modeling

Ecological Drought Data processing for model inputs

Need to create a csv files where each raster layer is becoming a column and the rows become the pixels

Here is the main redmine ticket: https://projects.nceas.ucsb.edu/snap/issues/471

Here is the list of pending improvements: https://github.nceas.ucsb.edu/SNAPP/eco-drought-data/issues and https://github.nceas.ucsb.edu/SNAPP/snapp-wg-scicomp/issues/3

Data log: https://docs.google.com/spreadsheets/d/14JEBFiIr3187JOkGmqrlQeCzsu1iAioLqWPH6psi7P0/edit#gid=1991379894

## The main scripts are:

- `swe_moist.R`: preprocessing for soil moisture and swe
- `reproject_clip.R`: to be used to reproject and resample the predictor rasters to match the response. If the data are not in a raster format or need preprocessing, you will need to first use the more file specific scripts.
- `mask_pixels.R`: to be used to extract pixels with strong response signal and create the csv files per HUC

