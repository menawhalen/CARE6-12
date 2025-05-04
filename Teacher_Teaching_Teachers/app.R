# Authors: Mena & Alyssa
# Last Updated: 4/25/2025
# Intended Use: Create a dashboard for students to upload and visualize 
# PurpleAir data
############################################################################

# Load packages
library(shiny)
library(tidyverse)
library(sf)
library(lubridate)
library(bslib)

# Load and clean data
dat <- read_csv("4-11dat.csv", skip = 8)
names(dat) <- c("id", "session", "date", "longitude", "latitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
dat <- dat %>% 
  mutate(date = ymd_hms(date))

## Define theme for dashboard
# Create a custom theme
my_theme <- bs_theme(
                    bg = "#F7F7F7",
                     fg = "#AA44AA",
                     accent = "#A7F7BE",
                     primary = "#AA44AA",
                     base_font = "sans-serif",
                     bootswatch = "yeti",
                     rounded = TRUE,
              "card-border-radius" = "1rem"       # specifically for cards and value_box()
                     )
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
        title = "Total Number of Sensors", value = n_distinct(dat$session), theme = "info",
        showcase = fontawesome::fa_i("hashtag"), showcase_layout = "top right",
        full_screen = FALSE, fill = TRUE, height = NULL,
        card_wrapper =TRUE
      ),
      bslib::value_box(
        title = "Average PM2.5 Levels", value = round(mean(dat$pm2_5,na.rm = TRUE), digits = 2),
        theme = "secondary", showcase = fontawesome::fa_i("smog"),
        showcase_layout = "top right", full_screen = FALSE, fill = TRUE,
        height = NULL,
        card_wrapper =TRUE
      ),
      bslib::value_box(
        title = "Standard Deviation of PM2.5 Levels", value = round(sd(dat$pm2_5,na.rm = TRUE), digits = 2),
        theme = "primary", showcase = fontawesome::fa_i("hand-holding-dollar"),
        showcase_layout = "top right", full_screen = FALSE, fill = TRUE,
        height = NULL,
        card_wrapper =TRUE
      )
    ),
    ## Temporal Aggregation Value (Seconds)
    radioButtons("time_agg", label = h3("Select Temporal Aggregation Value (s)"),
                 choices = list("10s" = 10, "30s" = 20, "60s" = 60), 
                 selected = 3),
    plotOutput("distPlot")
  ),
  
  nav_panel(
    "Upload",
    h3("Upload Sensor Data"),
    p("This page will be used to upload data.")
  ),

  nav_panel(
    "About",
    h3("About this Dashboard"),
    p("This is a student dashboard using a custom Bootstrap theme!")
  )

 )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    time_averaged <- reactive({
    
      dat %>% 
        group_by(agg = floor_date(date, unit = paste(as.character(input$time_agg), " seconds"))) %>% 
        summarise(ave_pm2_5 = mean(pm2_5, na.rm = T))
      })
    output$distPlot <- renderPlot({
        ## time series plot
      ggplot(time_averaged(), aes(agg, ave_pm2_5)) +
        geom_line() +
        labs(x = "Datetime",
             y = "Average PM2.5")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
