install.packages("DBI")
install.packages("RSQLite")

library(DBI)

library(RSQLite)

library(readr)

library(dplyr)
library(lubridate)
library(purrr)
library(stringr)

# con <- dbConnect(RSQLite::SQLite(), "C:/Users/noral/OneDrive/Desktop/CDSC/care6-12/CARE6-12/care612Database.db")

# dat1 <- read_csv("data/1.csv", skip = 8)
# names(dat1) <- c("id", "session", "date", "latitude","longitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
# dat1 <- dat1 %>% mutate(date = ymd_hms(date))

con <- dbConnect(RSQLite::SQLite(), "/Users/menawhalen/Library/CloudStorage/OneDrive-LoyolaUniversityChicago/Consulting/care_6_12/careDB.db")


clean_data <- function(file_path) {
  df <- read_csv(file_path, skip = 8)
  
  names(df) <- c("row_id", "session", "date", "latitude", "longitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
  
  df <- df %>%
    mutate(date = ymd_hms(date),
           session_id = paste0(str_to_lower(str_replace_all(session, " ", "_")), "_", min(date))) %>% 
    select(session_id, session, date, latitude, longitude, temp, pm1, pm10, pm2_5, humidity)
  
  return(df)
}

csv_files <- list.files("/Users/menawhalen/Library/CloudStorage/OneDrive-LoyolaUniversityChicago/Consulting/care_6_12/data", pattern = "\\.csv$", full.names = TRUE)


dat <- map(csv_files, ~ clean_data(.x))

# map(dat, ~.x %>% pull(session) %>% unique())

dat <- dat %>% 
  bind_rows() %>% 
  mutate(session = factor(session))

unique(dat$session_id)

dbWriteTable(conn = con, name = "historical", dat)

dbListTables(con)
# Disconnect when done
dbDisconnect(con)


dbListTables(con)