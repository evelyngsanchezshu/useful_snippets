# Header ------------------------------------------------------------------

# Purpose: quick US census data examples at County and state level
# Date   : 11/2024
# Author : Evelyn Shu

# Packages ----------------------------------------------------------------

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(dplyr, data.table, ggplot2, tidycensus, tigris, sf, tidyr)

setwd("C:/Users/eshu/path_here")

# get shapefiles for counties ------------------------------------

options(tigris_use_cache = TRUE)

county_boundaries <- counties(cb = TRUE, year = 2022)

st_write(county_boundaries, "us_county_boundaries.geojson")

# get some data ---------------------------------------------------

acs_variables <- load_variables(year=2022, "acs5", cache = TRUE)

# B19013E_001 (median hh income), 
# B19025E_001 (agg hh income), 
# B19301E_001 (per capita income), 
# B08522_002 (people below poverty level), B08522_001(total pop?) 

variables <- c("B19013E_001","B19025E_001","B19301E_001","B08522_002","B08522_001")

county_data <- get_acs(
  geography = "county",
  variables = variables,
  year = 2022,
  survey = "acs5"
)

county_data_wide <- county_data %>%
  dplyr::select(GEOID, NAME, variable, estimate) %>% # Keep only relevant columns
  pivot_wider(
    names_from = variable, # Variables as column names
    values_from = estimate # Use estimates as values
  )

names(county_data_wide) <- c("GEOID","NAME","total_pop","pop_below_poverty",
                             "med_hh_income","agg_hh_income","per_cap_income")

county_data_wide$perc_pop_poverty <- 100*(county_data_wide$pop_below_poverty / county_data_wide$total_pop)

fwrite(county_data_wide, "ex_county_census_data.csv")

# map it ------------------------------------------------------

county_data_wide$GEOID <- str_pad(county_data_wide$GEOID, width = 5, pad = "0", side="left")
county_boundaries$GEOID <- str_pad(county_boundaries$GEOID, width = 5, pad = "0", side="left")

county_map_data <- left_join(county_boundaries, county_data_wide, by = c("GEOID" = "GEOID"))

county_map_data <- county_map_data %>%
  filter(!STATEFP %in% c("02", "15", "72"))

contiguous_us_bbox <- st_bbox(c(
  xmin = -125, xmax = -66.5, # Longitude range for the contiguous US
  ymin = 23, ymax = 49.5   # Latitude range for the contiguous US
), crs = st_crs(county_map_data))

county_map_data <- st_crop(county_map_data, contiguous_us_bbox)

state_boundaries <- state_boundaries %>%
  filter(!STATEFP %in% c("02", "15", "72"))
state_boundaries <- st_crop(state_boundaries, contiguous_us_bbox)

ggplot(data = county_map_data) +
  geom_sf(aes(fill = perc_pop_poverty), color = NA) + 
  geom_sf(data = state_boundaries, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90", name = "% Below Poverty") +
  coord_sf(crs = 5070) +
  theme_minimal() +
  labs(
    title = "Percentage of Population Below Poverty Level",
    subtitle = "County-Level Data from ACS 2022",
    caption = "Source: U.S. Census Bureau"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# by State instead --------------------------------------------

# Get shapefiles for states ------------------------------------
state_boundaries <- states(cb = TRUE, year = 2022)

st_write(state_boundaries, "us_state_boundaries.geojson")

# acs data by state ----------------------------------------
state_data <- get_acs(
  geography = "state",
  variables = variables,
  year = 2022,
  survey = "acs5"
)

state_data_wide <- state_data %>%
  dplyr::select(GEOID, NAME, variable, estimate) %>% # Keep only relevant columns
  pivot_wider(
    names_from = variable, # Variables as column names
    values_from = estimate # Use estimates as values
  )

names(state_data_wide) <- c("GEOID", "NAME", "total_pop", "pop_below_poverty",
                            "med_hh_income", "agg_hh_income", "per_cap_income")

state_data_wide$perc_pop_poverty <- 100 * (state_data_wide$pop_below_poverty / state_data_wide$total_pop)

fwrite(state_data_wide, "ex_state_census_data.csv")

# map states ----------------------------------------

state_data_wide$GEOID <- str_pad(state_data_wide$GEOID, width = 2, pad = "0", side = "left")
state_boundaries$GEOID <- str_pad(state_boundaries$GEOID, width = 2, pad = "0", side = "left")

state_map_data <- left_join(state_boundaries, state_data_wide, by = c("GEOID" = "GEOID"))

state_map_data <- state_map_data %>%
  filter(!STATEFP %in% c("02", "15", "72"))

contiguous_us_bbox <- st_bbox(c(
  xmin = -125, xmax = -66.5, 
  ymin = 23, ymax = 49.5  
), crs = st_crs(state_map_data))

state_map_data <- st_crop(state_map_data, contiguous_us_bbox)

# Plot the map
ggplot(data = state_map_data) +
  geom_sf(aes(fill = perc_pop_poverty), color = "black") + 
  scale_fill_viridis_c(option = "plasma", na.value = "gray90", name = "% Below Poverty") +
  coord_sf(crs = 5070) + # Albers Equal Area projection
  theme_minimal() +
  labs(
    title = "Percentage of Population Below Poverty Level",
    subtitle = "State-Level Data from ACS 2022",
    caption = "Source: U.S. Census Bureau"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )
