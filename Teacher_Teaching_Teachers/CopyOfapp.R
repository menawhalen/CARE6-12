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

# Load and clean initial default data
dat <- read_csv("walking.csv", skip = 8)
names(dat) <- c("id", "session", "date", "latitude","longitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
dat <- dat %>% mutate(date = ymd_hms(date))

# Load Chicago community areas
chi_map <- st_read("Chicago_Community_Areas/chi_comm_areas.shp")

# Create a custom theme
my_theme <- bs_theme(
  bg = "#F7F7F7",
  fg = "#AA44AA",
  accent = "#A7F7BE",
  primary = "#AA44AA",
  base_font = "sans-serif",
  bootswatch = "pulse"
)

ui <- page_navbar(
  theme = my_theme,
  title = "CARE Teacher Dashboard",
  nav_spacer(),
  
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
    radioButtons("time_agg", label = h3("Select Temporal Aggregation Value (s)"),
                 choices = list("10s" = 10, "30s" = 30, "60s" = 60), 
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
    p("This page will be used to upload data."),
    fileInput("file1", "Choose CSV File", accept = ".csv", multiple = TRUE),
    actionButton("process_btn", "Load Data")
  ),
  
  nav_panel(
    "About",
    h3("About this Dashboard"),
    p("This is a student dashboard using a custom Bootstrap theme!")
  )
)

server <- function(input, output, session) {
  # --- NEW: Track uploaded files separately ---
  uploaded_files <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    uploaded_files(input$file1)
  })
  
  # --- NEW: Only process data on button click ---
  data_to_use <- eventReactive(input$process_btn, {
    files <- uploaded_files()
    
    if (is.null(files)) {
      return(dat)
    }
    
    all_data <- purrr::map_dfr(files$datapath, ~ {
      df <- readr::read_csv(.x, skip = 8)
      names(df) <- c("id", "session", "date", "latitude", "longitude",
                     "temp", "pm1", "pm10", "pm2_5", "humidity")
      df %>% mutate(date = lubridate::ymd_hms(date))
    })
    
    expected_cols <- c("id", "session", "date", "latitude", "longitude",
                       "temp", "pm1", "pm10", "pm2_5", "humidity")
    
    if (!all(expected_cols %in% names(all_data))) {
      showNotification("Error: One or more files do not have the expected format.", type = "error")
      return(NULL)
    }
    
    all_data
  })
  
  # Reactive for averaged time series
  time_averaged <- reactive({
    req(data_to_use())
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
  
  # Sensor data as sf for map
  sensor_dat_reactive <- reactive({
    req(data_to_use())
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
    req(data_to_use())
    n_distinct(data_to_use()$session)
  })
  
  output$avgPM25Text <- renderText({
    req(data_to_use())
    round(mean(data_to_use()$pm2_5, na.rm = TRUE), 2)
  })
  
  output$sdPM25Text <- renderText({
    req(data_to_use())
    round(sd(data_to_use()$pm2_5, na.rm = TRUE), 2)
  })
  
  output$distPlot <- renderPlot({
    req(time_averaged())
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
  
  output$mapPlot <- renderLeaflet({
    req(sensor_dat_reactive())
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

shinyApp(ui = ui, server = server)