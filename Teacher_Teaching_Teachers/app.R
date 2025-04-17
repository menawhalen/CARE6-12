#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

dat <- read_csv("4-11dat.csv", skip = 8)
### how to clean this
## think of other things
names(dat) <- c("id", "session", "date", "longitude", "latitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
dat <- dat %>% 
  mutate(date = ymd_hms(date))

# Define UI for application that draws a histogram
ui <- fluidPage(
    ## slider for picking time aggregation
    sliderInput(
        "time_agg",
        "Set Temporal Aggregation Value (Seconds)",
        min = 1,
        max = 60,
        value = 3,
        step = 1
      ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
