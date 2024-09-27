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
## WEST VIRGINIA
wv <- read.csv("data/wv/latest_file.csv") %>% clean_names

# Combine the two data frames
combined_data <- rbind(fl, ga, sc, nc, tn, va, wv)
combined_data <- combined_data %>% filter(customer_out != 0) #removing 0 outages

# Formatting date/time
# Assuming the timestamps are in Pacific Time (adjust tz argument as necessary)
combined_data$timestamp <- as.POSIXct(combined_data$timestamp, format="%a %b %d %Y %H:%M:%S", tz="America/New_York")

# Convert from Pacific Time to Eastern Time and format to 12-hour time with AM/PM
combined_data$timestamp_eastern <- format(with_tz(combined_data$timestamp, tzone = "America/New_York"), "%a %b %d %Y %I:%M %p")

# Clean data
clean <- combined_data %>% select(county, county_fips, customer_out, timestamp_eastern)

result <- clean %>%
  group_by(county_fips) %>%
  summarise(
    county = first(county),  # Keep the first occurrence of county name
    total_customer_out = sum(customer_out),  # Sum the customer_out
    latest_timestamp = max(timestamp_eastern),  # Get the latest timestamp
    .groups = 'drop'  # Drop the grouping
  )

# Get a total number of outages for the chatter
total_outages <- sum(combined_data$customer_out)
total_outages <- format(total_outages, big.mark = ",", scientific = FALSE)


# Make the datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(result, "Uhq3Z", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "Uhq3Z",
  api_key = dw_api_key,
  title = "Tracking power outages in Hurricane Helene's path",
  intro = paste( "There are currently", "<b>", total_outages, "</b>", "customers experiencing power outages in the states highlighted on the map below. Click or hover over a county to see more information."),
  annotate = "Note: Data updates every 30 minutes",
  byline = "Taylor Johnston / CBS News",
  source_name = "Find Energy",
  source_url = "https://findenergy.com/power-outage/",
  folderId = "267166"
)

# Publish the chart
dw_publish_chart(chart_id = "Uhq3Z")
