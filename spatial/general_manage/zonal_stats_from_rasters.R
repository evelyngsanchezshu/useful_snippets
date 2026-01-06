# Header ------------------------------------------------------------------

# Purpose: quickly extract zonal stats from raster data
# Date   : 1/2025
# Author : Evelyn Shu

# Packages ----------------------------------------------------------------

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(exactextractr, geodata, sf, raster, ggplot2)

# Example with Brazil---------------------------------------------------------------
# Get administrative boundaries for Brazil
brazil <- geodata::gadm(country = "BRA", level = 2, path = tempdir())

# Convert to sf object
brazil_sf <- st_as_sf(brazil)

# Get worldclim precipitation data
prec <- geodata::worldclim_tile(var = "prec", res = 10, lon = -55, lat = -10, path = tempdir())

# Convert raster to brick for handling layers easily
prec_brick <- raster::brick(prec)

# Calculate mean precipitation for December
brazil_sf$mean_dec_prec <- exact_extract(prec_brick[[12]], brazil_sf, 'mean')

# Calculate minimum and maximum precipitation for all months
stats <- exact_extract(prec_brick, brazil_sf, c('min', 'max'))

# Combine statistics with the sf object
brazil_sf <- cbind(brazil_sf, stats)


# Plot mean December precipitation
ggplot(data = brazil_sf) +
  geom_sf(aes(fill = mean_dec_prec), color = "black") +  # Fill regions based on precipitation
  scale_fill_viridis_c(option = "plasma", name = "Mean Dec Precip (mm)") +
  theme_minimal() +
  labs(
    title = "Mean December Precipitation in Brazil",
    subtitle = "WorldClim Data (1970-2000)",
    caption = "Data source: WorldClim"
  )


# Example with Kenya---------------------------------------------------------------

# get admin level 2 boundaries
kenya <- geodata::gadm(country = "KEN", level = 2, path = tempdir())
kenya_sf <- st_as_sf(kenya)

# get WorldClim precipitation data for Kenya
prec <- geodata::worldclim_country(country = "KEN", var = "prec", res = 10, path = tempdir())

# convert raster to brick for handling layers easily
prec_brick <- raster::brick(prec)

# plot the monthly rasters
plot(prec_brick)

# calculate zonal mean precipitation for December
kenya_sf$mean_dec_prec <- exact_extract(prec_brick[[12]], kenya_sf, 'mean')

# plot mean December precipitation
ggplot(data = kenya_sf) +
  geom_sf(aes(fill = mean_dec_prec), color = "black") + 
  scale_fill_viridis_c(option = "plasma", name = "Mean Dec Precip (mm)") +
  theme_minimal() +
  labs(
    title = "Mean December Precipitation in Kenya",
    subtitle = "WorldClim Data (1970-2000)",
    caption = "Data source: WorldClim"
  )


