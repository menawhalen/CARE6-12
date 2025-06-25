library(tidyverse)
library(sf)
library(tmap)
library(lubridate)

# Read and prepare data
maptest <- read_csv("data/maptesting.csv", skip = 8)
names(maptest) <- c("id", "session", "date", "latitude", "longitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
maptest <- maptest %>% mutate(date = ymd_hms(date))

# Convert to sf
maptest_sf <- st_as_sf(maptest, coords = c("longitude", "latitude"), crs = 4326)

# Reproject to metric CRS (Web Mercator)
maptest_sf_proj <- st_transform(maptest_sf, 3857)

# Snap time to 5-minute intervals
maptest_sf_proj$rounded_time <- floor_date(maptest_sf_proj$date, unit = "5 minutes")

# Create a 100m x 100m spatial grid over the extent
grid <- st_make_grid(maptest_sf_proj, cellsize = 50, square = TRUE)
grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)

# Join points to grid
maptest_with_grid <- st_join(maptest_sf_proj, grid_sf, join = st_within)

# Aggregate by grid cell + time
agg_sf <- maptest_with_grid %>%
  group_by(grid_id, rounded_time) %>%
  summarise(pm2_5 = mean(pm2_5, na.rm = TRUE), .groups = "drop")

# Attach grid geometry by matching grid_id
agg_sf$geometry <- grid_sf$geometry[match(agg_sf$grid_id, grid_sf$grid_id)]

# Convert to sf again
agg_sf <- st_as_sf(agg_sf, crs = st_crs(grid_sf)) %>%
  st_transform(4326)

# Plot
tmap_mode("view")  # interactive

tm_basemap("OpenStreetMap") +
  tm_shape(agg_sf) +
  tm_polygons(
    col = "pm2_5",          # NOTE: 'col' instead of 'fill' in view mode
    palette = "YlOrRd",     # directly specify palette
    alpha = 0.7,
    style = "quantile"      # needed for binning
  ) +
  tm_layout(
    title = "PM2.5 Aggregated by 5-Minute Interval & 100m Grid",
    legend.outside = TRUE
  )