library(tidyverse)
library(sf)
library(tmap)
library(lubridate)

maptest <- read_csv("data/maptesting.csv", skip = 8)
names(maptest) <- c("id", "session", "date", "latitude", "longitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
maptest <- maptest %>% mutate(date = ymd_hms(date))


maptest_sf <- st_as_sf(maptest, coords = c("longitude", "latitude"), crs = 4326)


maptest_sf_proj <- st_transform(maptest_sf, 3857)


maptest_sf_proj$rounded_time <- floor_date(maptest_sf_proj$date, unit = "5 minutes")


grid <- st_make_grid(maptest_sf_proj, cellsize = 50, square = TRUE)
grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)


maptest_with_grid <- st_join(maptest_sf_proj, grid_sf, join = st_within)


agg_sf <- maptest_with_grid %>%
  group_by(grid_id, rounded_time) %>%
  summarise(pm2_5 = mean(pm2_5, na.rm = TRUE), .groups = "drop")


agg_sf$geometry <- grid_sf$geometry[match(agg_sf$grid_id, grid_sf$grid_id)]

agg_sf <- st_as_sf(agg_sf, crs = st_crs(grid_sf)) %>%
  st_transform(4326)


#Map 1:

tmap_mode("view")  

tm_basemap("OpenStreetMap") +
  tm_shape(agg_sf) +
  tm_polygons(
    col = "pm2_5",          
    palette = "YlOrRd",     
    alpha = 0.7,
    style = "quantile"      
  ) +
  tm_layout(
    title = "PM2.5 Aggregated by 5-Minute Interval & 100m Grid",
    legend.outside = TRUE
  )

#Map 2:


library(ggplot2)

ggplot(agg_sf) +
  geom_sf(aes(fill = pm2_5), color = NA) +
  scale_fill_viridis_c(option = "C", na.value = NA) +
  labs(
    title = "PM2.5 Aggregated by 5-Minute Interval & 100m Grid",
    fill = "PM2.5"
  ) +
  theme_minimal()



#Map 3:
library(ggplot2)
library(maptiles)
library(sf)
library(ggspatial)
library(plotly)


agg_sf_web <- st_transform(agg_sf, 3857)


bbox <- st_bbox(agg_sf_web)
expand_factor <- 0.05  

bbox_expanded <- bbox
bbox_expanded["xmin"] <- bbox["xmin"] - (bbox["xmax"] - bbox["xmin"]) * expand_factor
bbox_expanded["xmax"] <- bbox["xmax"] + (bbox["xmax"] - bbox["xmin"]) * expand_factor
bbox_expanded["ymin"] <- bbox["ymin"] - (bbox["ymax"] - bbox["ymin"]) * expand_factor
bbox_expanded["ymax"] <- bbox["ymax"] + (bbox["ymax"] - bbox["ymin"]) * expand_factor


basemap <- get_tiles(bbox_expanded, provider = "OpenStreetMap", crop = TRUE)


ggplot() +
  layer_spatial(basemap) +
  geom_sf(data = agg_sf_web, aes(fill = pm2_5), color = NA, alpha = 0.7) +
  scale_fill_gradientn(
    colours = c("green", "yellow", "red"),
    values = scales::rescale(c(0, 9, 35, 55, 150)),
    na.value = NA,
    guide = guide_colorbar(reverse = TRUE)
  ) +
  labs(
    title = "PM2.5 Aggregated by 5-Minute Interval & 100m Grid",
    fill = "PM2.5 (µg/m³)"
  ) +
  theme_minimal()

#Map 4:
library(leaflet)
library(sf)
library(RColorBrewer)

bins <- c(0, 12, 35.4, 55.4, 150.4, 250.4, Inf)
palette_colors <- c("green", "yellow", "orange", "red", "purple", "maroon")

colors <- colorBin(
  palette = palette_colors,
  domain = agg_sf$pm2_5,
  bins = bins,
  na.color = "transparent"
)
labels <- c(
  "Good (0–12)",
  "Moderate (12.1–35.4)",
  "Unhealthy for Sensitive (35.5–55.4)",
  "Unhealthy (55.5–150.4)",
  "Very Unhealthy (150.5–250.4)",
  "Hazardous (250.5+)"
)

leaflet(data = agg_sf) %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(
    fillColor = ~colors(pm2_5),
    weight = 0.5,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 1,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0("PM2.5: ", round(pm2_5, 1), " µg/m³"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    colors = palette_colors,
    labels = labels,
    opacity = 0.7,
    title = "EPA Air Quality Index",
    position = "bottomright"
  )