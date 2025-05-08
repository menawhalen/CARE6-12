# Authors: Mena & Alyssa
# Created: 4/25/2025
# Last Updated: 5/4/2025
# Intended Use: Create a dashboard for students/teachers to upload and visualize 
# PurpleAir data
############################################################################

# Load packages
library(shiny)
library(tidyverse)
library(sf)
library(lubridate)
library(bslib)
library(leaflet)

# Load and clean data
dat <- read_csv("walking.csv", skip = 8)
names(dat) <- c("id", "session", "date", "latitude","longitude", "temp", "pm1", "pm10", "pm2_5", "humidity")

dat <- dat %>% 
  mutate(date = ymd_hms(date))

# Load Chicago community areas
chi_map <- st_read("Chicago_Community_Areas/chi_comm_areas.shp")

# Create SF object of sensors
sensor_dat <- dat %>%
  select(session, longitude, latitude) %>%
  filter(!duplicated(session)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = st_crs(chi_map)) %>%
    st_transform(crs = 4326)


# Merge map geeometry with sf object sensor data
chi_map <- st_transform(chi_map, crs = 4326)
chi_map_sensor <- st_join(
  x = chi_map,
  y = sensor_dat,
  join = st_intersects)


## Define theme for dashboard
# Create a custom theme
my_theme <- bs_theme(
                    bg = "#F7F7F7",
                     fg = "#AA44AA",
                     accent = "#A7F7BE",
                     primary = "#AA44AA",
                     base_font = "sans-serif",
                     bootswatch = "pulse")
#bs_theme_preview(my_theme)

# Define UI for application that draws a histogram
ui <- page_navbar(
  theme = my_theme,
  title = "CARE Teacher Dashboard",
  nav_spacer(), # push nav items to the right
  
  nav_panel(
    "Home",
    h2("Welcome to the Dashboard"),
    layout_columns(
      bslib::value_box(
        title = "Total Number of Sensors",
        value = textOutput("totalSensorsText"),
        theme = "info",
        showcase = fontawesome::fa_i("hashtag"),
        showcase_layout = "top right"
      ),
      bslib::value_box(
        title = "Average PM2.5 Levels",
        value = textOutput("avgPM25Text"),
        theme = "secondary",
        showcase = fontawesome::fa_i("smog"),
        showcase_layout = "top right"
      ),
      bslib::value_box(
        title = "Standard Deviation of PM2.5 Levels",
        value = textOutput("sdPM25Text"),
        theme = "primary",
        showcase = fontawesome::fa_i("plus-minus"),
        showcase_layout = "top right"
      )
    ),
    ## Temporal Aggregation Value (Seconds)
    radioButtons("time_agg", label = h3("Select Temporal Aggregation Value (s)"),
                 choices = list("10s" = 10, "30s" = 20, "60s" = 60), 
                 selected = 60),
    plotOutput("distPlot")
  ),
  
  nav_panel(
    "Map",
    h3("Sensors in Chicago"),
    leafletOutput("mapPlot", height = 600)
  ),
  
  nav_panel(
    "Upload",
    h3("Upload Sensor Data"),
    p("This page will be used to upload data. Please upload AirBeam csv files downloaded from www.aircasting.habitatmap.org. Multple files can be uploaded together."),
    fileInput("file1", "Choose CSV File", accept = ".csv", multiple = TRUE)
  ),

  nav_panel(
    "About",
    h3("About this Dashboard"),
    p("This dashboard is for CARE 6-12 to work with local teachers to educate on environmental justice and air quality impacts in Chicago. This dashboard was created by Nora Lee, Mena Whalen and Alyssa Peer in partnership with the Loyola Center for Data Science and Consulting!")
  )

 )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
 ## Adding Data
  data_to_use <- reactive({
    if (is.null(input$file1)) {
      dat
    } else {
      # Loop over each file
      all_data <- purrr::map_dfr(input$file1$datapath, ~ {
        # Read each CSV (adjust if you need different skips etc.)
        df <- readr::read_csv(.x, skip = 8)
        
        # Standardize column names
        names(df) <- c("id", "session", "date", "latitude", "longitude",
                       "temp", "pm1", "pm10", "pm2_5", "humidity")
        
        # Format date
        df <- df %>% mutate(date = lubridate::ymd_hms(date))
        
        df
      })
      
      # Optional: you can add a check to stop if a file doesn't match expected cols
      expected_cols <- c("id", "session", "date", "latitude", "longitude",
                         "temp", "pm1", "pm10", "pm2_5", "humidity")
      if (!all(expected_cols %in% names(all_data))) {
        shiny::showNotification("Error: One or more files do not have the expected format.", type = "error")
        return(NULL)
      }
      
      all_data
    }
  })
  # Create reactive expression for aggregated pm values on line chart 
  time_averaged <- reactive({
    data_to_use() %>%
      pivot_longer(
        cols = c(pm1, pm10, pm2_5),
        names_to = "pm_size",
        values_to = "value"
      ) %>%
      group_by(
        agg = floor_date(date, unit = paste(input$time_agg, "seconds")),
        pm_size,
        session
      ) %>%
      summarise(
        avg_pm = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
  })
  ## sensor map reactive
  
  sensor_dat_reactive <- reactive({
    data_to_use() %>%
      select(session, longitude, latitude) %>%
      filter(!duplicated(session)) %>%
      st_as_sf(
        coords = c("longitude", "latitude"),
        crs = st_crs(chi_map)
      ) %>%
      st_transform(crs = 4326)
  })
  
  output$totalSensorsText <- renderText({
    n_distinct(data_to_use()$session)
  })
  
  output$avgPM25Text <- renderText({
    round(mean(data_to_use()$pm2_5, na.rm = TRUE), 2)
  })
  
  output$sdPM25Text <- renderText({
    round(sd(data_to_use()$pm2_5, na.rm = TRUE), 2)
  })
  # Render line chart of aggregated pm values across time
  output$distPlot <- renderPlot({
    ggplot(time_averaged(), aes(x = agg, y = avg_pm, color = pm_size)) +
      geom_line(size = 1) +
      scale_color_manual(
        values = c(
          "pm1" = "#AA44AA",
          "pm2_5" = "#A7F7BE",
          "pm10" = "#08B2E3")
      ) +
      labs(
        x = "Datetime",
        y = "Averaged PM",
        title = "Averaged PM Levels by Size Over Time",
        color = "Particle Size") +
      facet_wrap(~session, scales = "free", ncol = 1) +
      theme_minimal() +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  # Render Map with Sensors plotted
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = chi_map,
        color = "black",
        weight = 1,
        fillOpacity = 0.2
      ) %>%
      addCircleMarkers(
        data = sensor_dat_reactive(),
        radius = 7,
        color = "#AA44AA",
        popup = ~session
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
