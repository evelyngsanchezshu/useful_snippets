# Header ------------------------------------------------------------------

# Purpose: Quickly pull US Census data and create interactive map
# Date   : 1/2025
# Author : Evelyn Shu

# Packages ----------------------------------------------------------------

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tigris, mapview, leaflet.extras2)

# Code ----------------------------------------------------------------

options(tigris_use_cache=T)

# pull and view US tract data
us_tracts <- tracts(cb=T, year=2023, resolution="5m")
mapview::mapview(us_tracts)

# pull and view US CBSAs
cbsa21 <- core_based_statistical_areas(cb=T, year=2021)
cbsa23 <- core_based_statistical_areas(cb=T, year=2023)

mapview::mapview(cbsa21, layer.name="2021 boundaries", col.regions="red") |
  mapview::mapview(cbsa23, layer.name="2023 boundaries", col.regions="blue")
