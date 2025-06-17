install.packages("DBI")
install.packages("RSQLite")

library(DBI)

library(RSQLite)

library(readr)

library(dplyr)
library(lubridate)


con <- dbConnect(RSQLite::SQLite(), "C:/Users/noral/OneDrive/Desktop/CDSC/care6-12/CARE6-12/care612Database.db")

# dat1 <- read_csv("data/1.csv", skip = 8)
# names(dat1) <- c("id", "session", "date", "latitude","longitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
# dat1 <- dat1 %>% mutate(date = ymd_hms(date))



clean_data <- function(file_path) {
  df <- read_csv(file_path, skip = 8)
  
  names(df) <- c("id", "session", "date", "latitude", "longitude", "temp", "pm1", "pm10", "pm2_5", "humidity")
  
  df <- df %>%
    mutate(date = ymd_hms(date))
  
  return(df)
}

csv_files <- list.files("C:/Users/noral/OneDrive/Desktop/CDSC/care6-12/CARE6-12/data", pattern = "\\.csv$", full.names = TRUE)

# Loop through files, clean and write each to the database
for (file in csv_files) {
  table_name <- tools::file_path_sans_ext(basename(file))
  
  df_clean <- clean_data(file)
  
  dbWriteTable(con, table_name, df_clean, overwrite = TRUE)
  
  cat("Imported cleaned table:", table_name, "\n")
}



dbListTables(con)
# Disconnect when done
dbDisconnect(con)


dbListTables(con)