#############################################

# Purpose: Subset CHIRTS data easily from NetCDF
# Date   : 5/2024
# Author : Evelyn Shu

#############################################

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(geodata, terra, sf, raster, spdep)

setwd("C:/Users/eshu/path_here")

# Temperature with CHIRTS ------------------------------------------------

# temporarily open NetCDF to extract wanted dimensions
nc_file <- nc_open("CHIRTS-ERA5.monthly_Tmax.monthly.nc")

# view attributes of NetCDF
# print(nc_file)

# retrieve date information
time <- ncvar_get(nc_file,"time")
tunits <- ncatt_get(nc_file,"time","units")$value
t_origin <- as.Date(sub(".*since ","",tunits))
dates <- t_origin + time

# create date range
start_date <- as.Date("2018-11-03")
end_date <- as.Date("2023-12-17")

# specify IDs for which dates to include
time_idx <- which(dates >= start_date & dates <= end_date)

nc_close(nc_file)
gc()

# open RasterBrick
temp_raster <- brick("CHIRTS-ERA5.monthly_Tmax.monthly.nc")

# subset RasterBrick to identified dates
temp_raster_time <- temp_raster[[time_idx]]
temp_raster_time <- setZ(temp_raster_time, dates[time_idx], name="time")

# country boundaries
n_shp <- geodata::gadm(country = "NGA", level = 0, path = tempdir())
n_shp <- st_as_sf(n_shp)

# crop and mask to Nigeria boundaries
temp_raster_crop <- crop(temp_raster_time, extent(n_shp))
temp_raster_masked <- mask(temp_raster_crop, n_shp)

rm(temp_raster, temp_raster_time, temp_raster_crop)

plot(temp_raster_masked)
