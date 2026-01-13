#############################################

# Purpose: Subset CHIRTS data easily from NetCDF
# Date   : 5/2024
# Author : Evelyn Shu

#############################################
# Packages ------------------------------------------------
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(geodata, ntcdf4, terra, sf, raster, spdep)

setwd("C:/Users/eshu/path_here")

# Temperature with CHIRTS ------------------------------------------------
# get NetCDF info and create subset parameters ------------------------------------------------

# temporarily open NetCDF to extract wanted time dimensions
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

# country boundaries for spatial subset
n_shp <- geodata::gadm(country = "NGA", level = 0, path = tempdir())
n_shp <- st_as_sf(n_shp)

# RasterBrick if using in memory now (if writing, use terra) ------------------------------------------------
temp_raster <- brick("CHIRTS-ERA5.monthly_Tmax.monthly.nc")

# subset RasterBrick to identified dates
temp_raster_time <- temp_raster[[time_idx]]
temp_raster_time <- setZ(temp_raster_time, dates[time_idx], name="time")

# crop and mask to Nigeria boundaries
temp_raster_crop <- crop(temp_raster_time, extent(n_shp))
temp_raster_masked <- mask(temp_raster_crop, n_shp)

rm(temp_raster, temp_raster_time, temp_raster_crop)

plot(temp_raster_masked)

# terra if writing NetCDF ------------------------------------------------
temp_raster <- rast("CHIRTS-ERA5.monthly_Tmax.monthly.nc")

# make sure country vect is same CRS as raster
n_shp <- st_transform(n_shp, crs(temp_raster))
n_v   <- vect(n_shp)

# crop and mask to country
temp_crop   <- crop(temp_raster, n_v)
temp_mask <- mask(temp_crop, n_v)

# subset to identified dates
temp_sub <- temp_mask[[time_idx]]
time_sub_dates <- dates[time_idx]

# attach time
time(temp_sub) <- time_sub_dates

# optional: set names to something consistent for later
names(temp_sub) <- format(time_sub_dates, "%Y_%m")

# quick check
# plot(temp_sub[[1]], main = paste("First layer:", names(temp_sub)[1]))

# write NetCDF ----
out_nc <- file.path("C:/Users/eshu/path_here",
                    paste0("CHIRTS_Tmax_monthly_NGA_",
                           format(min(time_sub_dates), "%Y"),
                           ".nc"))

writeCDF(
  x = temp_sub,
  filename = out_nc,
  varname  = "tmax",
  longname = "CHIRTS-ERA5 monthly maximum temperature (subset to NGA)",
  unit     = "degC",
  overwrite = TRUE,
  zname  = "time"
)

cat("Wrote:", out_nc, "\n")

