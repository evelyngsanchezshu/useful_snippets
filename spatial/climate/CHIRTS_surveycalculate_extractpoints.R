# Header ------------------------------------------------------------------

# Purpose: CHIRTS calculate by survey dates, extract points (Mozambique example)
# Date   : 2/2025
# Author : Evelyn Shu

# Packages ----------------------------------------------------------------

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(terra, geodata, exactextractr, ncdf4, tidyverse, sf, raster, spdep, ggplot2, data.table, dplyr, foreach, doParallel, parallel)

setwd("C:/Users/eshu/path_here") 

# Data --------------------------------------------------------------------
options(stringsAsFactors = FALSE) 

# get admin level 0 (country) boundaries ~~~~~~~~
adm0 <- geodata::gadm(country = "MZ", level = 0, path = tempdir())
adm0 <- st_as_sf(adm0)

# get local hot day raster (created on GEE, can drop) ~~~~~~~~
hotday <- rast("datasets/clean/MZ_95th_Temp.tif")
hotday <- ifel(hotday >= 0, hotday, NA)
hotday <- crop(hotday, adm0)
hotday <- mask(hotday, adm0)
plot(hotday) # save out if wanted

# points shapefiles ~~~~~~~~
points_shp <- st_read("datasets/raw/points_GPS.shp")
points <- st_drop_geometry(points_shp)
points <- points[,c("DHSCLUST","LONGNUM","LATNUM")]

# points survey date ~~~~~~~~
dhs <- read_dta("datasets/raw/points_csv.dta")
dhs <- dhs[,c("v001","v002","v003","caseid")]

dhs2 <- read_dta("datasets/raw/points_csv2.dta")
dhs2 <- dhs2[,c("hv001","hv002","hv003","hv008a")]

dhs <- merge(dhs, dhs2, by.x=c("v001","v002","v003"), by.y=c("hv001","hv002","hv003"), all.x=T)
summary(dhs$hv008a)

dhs <- merge(dhs, points, by.x="v001", by.y="DHSCLUST", all.x=T)

reference_date <- as.Date("1900-01-01")
dhs$date_survey <- reference_date %m+% days(dhs$hv008a)
summary(dhs$date_survey)

# CHIRTS month before survey --------------------------------------------------------

dhs$month_survey <- dhs$date_survey %m-% months(1)
summary(dhs$month_survey)

# Heatwave proportion function ~~~~~~~~
prop_heatwave <- function(temps, threshold) {
  bin_rle <- rle(temps >= threshold)
  is_heatwave <- bin_rle$values & (bin_rle$lengths >= 3)
  sum(bin_rle$lengths[is_heatwave]) / length(temps)
} 

# Load NetCDF files into a list to call in loop ~~~~~~~~
netcdf_files <- list(
  "2022" = rast("datasets/raw/CHIRTS-ERA5.daily_Tmax.2022.nc"),
  "2023" = rast("datasets/raw/CHIRTS-ERA5.daily_Tmax.2023.nc")
)

# Loop over each row ~~~~~~~~
for (i in 1:nrow(dhs)) {
  row <- dhs[i, ]
  start_date <- as.Date(row$month_survey)
  end_date <- as.Date(row$date_survey)
  
  if(is.na(start_date)) {
    dhs$month_maxt[i] <- NA
    dhs$month_phd[i] <- NA
    dhs$month_phw[i] <- NA
    next
  }  
  
  coords <- matrix(c(row$LONGNUM, row$LATNUM), ncol = 2)

  threshold <- terra::extract(hotday, coords)[, 1]
  
  # filter years
  start_year <- format(start_date, "%Y")
  end_year <- format(end_date, "%Y")
  
  # create a list to hold the temperature data for the date range
  temp_values <- numeric()
  
  # Loop through each year for the row
  for (year in seq(as.numeric(start_year), as.numeric(end_year))) {
    # Get the raster for the current year
    nc <- netcdf_files[[as.character(year)]]
    
    # pick either the start date as the beginning of the year, or the week date, whichever is higher
    year_start_date <- max(as.Date(paste0(year, "-01-01")), start_date)
    # same idea but opposite
    year_end_date <- min(as.Date(paste0(year, "-12-31")), end_date)
    
    # Get the indices for dates
    days <- seq(year_start_date, year_end_date, by = "day")
    time_indices <- as.numeric(difftime(days, as.Date(paste0(year, "-01-01")), units = "days")) + 1
    
    # Extract data
    for (day_index in time_indices) {
      if (day_index > nlyr(nc)) {
        warning(paste("Invalid day index:", day_index, "for year:", year))
        next  # Skip if index is invalid
      }
      temp_raster <- nc[[day_index]]
      
      if (is.null(temp_raster)) {
        warning(paste("No valid raster for day index:", day_index, "in year:", year))
        next  # Skip if  raster is NULL
      }
      temp_value <- terra::extract(temp_raster, coords)[, 1]
      temp_values <- c(temp_values, temp_value)
    }
  }
  
  if (length(temp_values) > 0) {
    max_m_temp <- max(temp_values, na.rm = TRUE)
    prop_m_hd <- mean(temp_values > threshold, na.rm = TRUE)
    heatwave_prop <- prop_heatwave(temp_values, threshold)
    
  } else {
    max_m_temp <- NA
    prop_m_hd <- NA
    heatwave_prop <- NA
    
  }
  
  dhs$month_maxt[i] <- max_m_temp
  dhs$month_phd[i] <- prop_m_hd
  dhs$month_phw[i] <- heatwave_prop
  
}

# dhs$month_maxt <- ifelse(is.na(dhs$month_survey),NA,dhs$month_maxt)
# dhs$month_phd <- ifelse(is.na(dhs$month_survey),NA,dhs$month_phd)
# dhs$month_phw <- ifelse(is.na(dhs$month_survey),NA,dhs$month_phw)


fwrite(dhs, "datasets/clean/chirts_month_before_survey.csv")

