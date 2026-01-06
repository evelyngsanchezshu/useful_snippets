# Header ------------------------------------------------------------------

# Purpose: CHIRPS calculate, map, extract points (Bangladesh example)
# Date   : 1/2025
# Author : Evelyn Shu

# Packages ----------------------------------------------------------------

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(terra, geodata, exactextractr, ncdf4, chirps, tidyverse, sf, raster, spdep, ggplot2, data.table, dplyr, foreach, doParallel, parallel)

setwd("C:/Users/eshu/path_here") 

# Data --------------------------------------------------------------------
options(stringsAsFactors = FALSE) 

# get admin level 0 (country) boundaries ~~~~~~~~
adm0 <- geodata::gadm(country = "BD", level = 0, path = tempdir())
adm0 <- st_as_sf(adm0)

# points file ~~~~~~~~
points <- st_read("datasets/raw/gps_points_data.shp") 

# CHIRPS netcdf ~~~~~~~~
# open and crop ncdf (need to download)
nc_file <- nc_open("datasets/raw/chirps-v2.0.2022.days_p05.nc")

# view the attributes of the NetCDF file
print(nc_file)

# retrieve date information
time <- ncvar_get(nc_file,"time")
tunits <- ncatt_get(nc_file,"time","units")$value
t_origin <- as.Date(sub(".*since ","",tunits))
dates <- t_origin + time

# create date range
start_date <- as.Date("2022-05-01")
end_date <- as.Date("2022-08-01")

# specify IDs for which dates to include
time_idx <- which(dates >= start_date & dates <= end_date)

nc_close(nc_file)
gc()

# open RasterBrick 
chirps_brick <- brick("datasets/raw/chirps-v2.0.2022.days_p05.nc")

# subset RasterBrick to the dates we identified above
chirps_raster <- chirps_brick[[time_idx]]
chirps_raster <- setZ(chirps_raster, dates[time_idx], name="time")

# crop and mask to BD boundaries
chirps_crop <- crop(chirps_raster, extent(adm0))
chirps_mask <- mask(chirps_crop, adm0)

rm(chirps_brick, chirps_raster, chirps_crop)
gc()

chirps_mask <- rast(chirps_mask)

# Calculate metrics and map --------------------------------------

# max daily precip ~~~~~~~~
chirps_max <- app(chirps_mask, fun=max)

# plot and save out
plot(chirps_max,
     main = "Bangladesh 2022 max. precip.", 
     col = terrain.colors(100),
     legend = T,
     box = FALSE, axes = FALSE,
     font.main=2, cex.main = 1)
plot(points, add=T, col="black", pch=16, cex=0.2)
plot(st_geometry(adm0), add = TRUE, col = NA, border = "black", lwd = 1)

# avg daily precip ~~~~~~~~
chirps_avg <- app(chirps_mask, fun=mean)

# plot and save out
plot(chirps_avg,
     main = "Bangladesh 2022 avg. precip.", 
     col = terrain.colors(100),
     legend = T,
     box = FALSE, axes = FALSE,
     font.main=2, cex.main = 1)
plot(points, add=T, col="black", pch=16, cex=0.2)
plot(st_geometry(adm0), add = TRUE, col = NA, border = "black", lwd = 1)

# total precip over season ~~~~~~~~
chirps_sum <- app(chirps_mask, fun=sum)

# plot and save out
plot(chirps_sum,
     main = "Bangladesh 2022 total precip.", 
     col = terrain.colors(100),
     legend = T,
     box = FALSE, axes = FALSE,
     font.main=2, cex.main = 1)
plot(points, add=T, col="black", pch=16, cex=0.2)
plot(st_geometry(adm0), add = TRUE, col = NA, border = "black", lwd = 1)

# Extract data at points ---------------------------------------

coords <- st_coordinates(points)

chirps_max_vect <- terra::extract(chirps_max, coords)
chirps_avg_vect <- terra::extract(chirps_avg, coords)
chirps_sum_vect <- terra::extract(chirps_sum, coords)

chirps_max_vect <- chirps_max_vect[, 1]
chirps_avg_vect <- chirps_avg_vect[, 1]
chirps_sum_vect <- chirps_sum_vect[, 1]

chirps_df <- data.frame(
  dhsclust = points$DHSCLUST,
  lon = coords[,1],
  lat = coords[,2],
  chirps_max_22 = chirps_max_vect,
  chirps_avg_22 = chirps_avg_vect,
  chirps_sum_22 = chirps_sum_vect)

fwrite(chirps_df, "datasets/clean/chirps_2022.csv")

