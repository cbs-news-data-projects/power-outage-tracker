library(dplyr)
library(DatawRappr)
library(dotenv)
library(janitor)
library(lubridate)
library(stringr)

# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})

dw_api_key <- Sys.getenv("DW_API_KEY")

# Texas
# Reading in data
texas_market <- read.csv("data/tx/latest_file.csv") %>% 
  clean_names %>% 
  mutate(county = str_remove(county, " County")) %>%  # Remove "County" from county column
  filter(customer_out != 0) #removing 0 outages

# Formatting date/time
# Replace '24:' with '00:' and convert to POSIXct
texas_market$timestamp <- gsub("24:", "00:", texas_market$timestamp)

# Convert the cleaned timestamp to POSIXct
texas_market$timestamp <- as.POSIXct(texas_market$timestamp, format="%a %b %d %Y %H:%M:%S")

# Handle cases where the time was converted from 24:00
texas_market$timestamp[texas_market$timestamp == as.POSIXct("2024-09-28 00:00:40", tz="UTC")] <- 
  texas_market$timestamp[texas_market$timestamp == as.POSIXct("2024-09-28 00:00:40", tz="UTC")]

# Format to desired output: "Fri Sep 27 2024 11:16 PM"
texas_market$timestamp_formatted <- format(texas_market$timestamp, "%a %b %d %Y %I:%M %p")

# Clean data
texas_market <- texas_market %>%
  select(county, county_fips, state, customer_out, est_customers_served, timestamp_formatted)

texas_market <- texas_market %>%
  group_by(county_fips, state) %>%  # Group by both county_fips and state
  summarise(
    county = first(county),  # Keep the first occurrence of county name
    total_customer_out = sum(customer_out),  # Sum customer_out for each county
    est_customers_served = sum(est_customers_served),  # Sum total customers served
    pct_customers_out = round((total_customer_out / est_customers_served) * 100, 1),  # Calculate percentage and round to 1 decimal
    latest_timestamp = max(timestamp_formatted),  # Get the latest timestamp
    .groups = 'drop'  # Drop the grouping
  )


# Get a total number of outages for the chatter
total_outages <- sum(texas_market$total_customer_out)
total_outages <- format(total_outages, big.mark = ",", scientific = FALSE)

# Make the datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(texas_market, "BOG4D", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "BOG4D",
  api_key = dw_api_key,
  title = "Tracking power outages in Texas",
  intro = paste( "There are currently", "<b>", total_outages, "</b>", "customers experiencing power outages in Texas. Click or hover over a county to see more information."),
  annotate = "Note: Data updates every 30 minutes",
  byline = "Taylor Johnston / CBS News",
  source_name = "Find Energy",
  source_url = "https://findenergy.com/power-outage/",
  folderId = "268316"
)

# Publish the chart
dw_publish_chart(chart_id = "BOG4D")

