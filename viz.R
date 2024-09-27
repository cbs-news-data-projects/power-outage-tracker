library(dplyr)
library(DatawRappr)
library(dotenv)
library(janitor)
library(lubridate)

# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# Bringing in state data
## FLORIDA
fl <- read.csv("data/fl/latest_file.csv") %>% clean_names
## GEORGIA
ga <- read.csv("data/ga/latest_file.csv") %>% clean_names
## SOUTH CAROLINA
sc <- read.csv("data/sc/latest_file.csv") %>% clean_names
## NORTH CAROLINA
nc <- read.csv("data/nc/latest_file.csv") %>% clean_names
## TENNESSEE
tn <- read.csv("data/tn/latest_file.csv") %>% clean_names
## VIRGINIA
va <- read.csv("data/va/latest_file.csv") %>% clean_names

# Combine the two data frames
combined_data <- rbind(fl, ga, sc, nc, tn, va)
combined_data <- combined_data %>% filter(customer_out != 0) #removing 0 outages

# Formatting date/time
# Assuming the timestamps are in Pacific Time (adjust tz argument as necessary)
combined_data$timestamp <- as.POSIXct(combined_data$timestamp, format="%a %b %d %Y %H:%M:%S", tz="America/New_York")

# Convert from Pacific Time to Eastern Time and format to 12-hour time with AM/PM
combined_data$timestamp_eastern <- format(with_tz(combined_data$timestamp, tzone = "America/New_York"), "%a %b %d %Y %I:00 %p")


# Make the datawrapper
dw_data_to_chart(combined_data, "Uhq3Z", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "Uhq3Z",
  api_key = dw_api_key,
  title = "Tracking power outages in Hurricane Helene's path",
  annotate = "Note: Data updates every 30 minutes",
  byline = "Taylor Johnston / CBS News",
  source_name = "Find Energy",
  source_url = "https://findenergy.com/power-outage/",
  folderId = "267166"
)

# Publish the chart
dw_publish_chart(chart_id = "Uhq3Z")
