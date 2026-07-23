library(dplyr)
library(dotenv)
library(janitor)
library(lubridate)

source("datawrapper.R")

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
## ALABAMA
al <- read.csv("data/al/latest_file.csv") %>% clean_names()
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
## KENTUCKY
ky <- read.csv("data/ky/latest_file.csv") %>% clean_names
## OHIO
oh <- read.csv("data/oh/latest_file.csv") %>% clean_names
## CT
ct <- read.csv("data/ct/latest_file.csv") %>% clean_names
## VT
vt <- read.csv("data/vt/latest_file.csv") %>% clean_names
## DE
de <- read.csv("data/de/latest_file.csv") %>% clean_names
## ME
me <- read.csv("data/me/latest_file.csv") %>% clean_names
## MA
ma <- read.csv("data/ma/latest_file.csv") %>% clean_names
## RI
ri <- read.csv("data/ri/latest_file.csv") %>% clean_names
## NY
ny <- read.csv("data/ny/latest_file.csv") %>% clean_names
## NJ
nj <- read.csv("data/nj/latest_file.csv") %>% clean_names
## PA
pa <- read.csv("data/pa/latest_file.csv") %>% clean_names
## MD
md <- read.csv("data/md/latest_file.csv") %>% clean_names
## IN
ind <- read.csv("data/in/latest_file.csv") %>% clean_names
## NH
nh <- read.csv("data/nh/latest_file.csv") %>% clean_names
## TX
tx <- read.csv("data/tx/latest_file.csv") %>% clean_names
## LA
la <- read.csv("data/la/latest_file.csv") %>% clean_names
## MS
ms <- read.csv("data/ms/latest_file.csv") %>% clean_names
## AR
ar <- read.csv("data/ar/latest_file.csv") %>% clean_names
## OK
ok <- read.csv("data/ok/latest_file.csv") %>% clean_names
## NM
nm <- read.csv("data/nm/latest_file.csv") %>% clean_names
## MO
mo <- read.csv("data/mo/latest_file.csv") %>% clean_names
## MN
mn <- read.csv("data/mn/latest_file.csv") %>% clean_names
## IL
il <- read.csv("data/il/latest_file.csv") %>% clean_names
## IA
ia <- read.csv("data/ia/latest_file.csv") %>% clean_names
## WI
wi <- read.csv("data/wi/latest_file.csv") %>% clean_names
## MI
mi <- read.csv("data/mi/latest_file.csv") %>% clean_names
## NE
ne <- read.csv("data/ne/latest_file.csv") %>% clean_names
## KS
ks <- read.csv("data/ks/latest_file.csv") %>% clean_names
## ND
nd <- read.csv("data/nd/latest_file.csv") %>% clean_names
## SD
sd <- read.csv("data/sd/latest_file.csv") %>% clean_names
## CO
co <- read.csv("data/co/latest_file.csv") %>% clean_names
## WY
wy <- read.csv("data/wy/latest_file.csv") %>% clean_names
## MT
mt <- read.csv("data/mt/latest_file.csv") %>% clean_names

# Combine all state data frames
combined_data <- rbind(fl, al, ga, sc, nc, tn, va, wv, ky, oh, ct, vt, de, me, ma, ri, ny, nj, pa, md, ind, nh, tx, la, ms, ar, ok, nm, mo, mn, il, ia, wi, mi, ne, ks, nd, sd, co, wy, mt)
combined_data <- combined_data %>% filter(customer_out != 0) #removing 0 outages

# Formatting date/time
# Replace '24:' with '00:' and convert to POSIXct
combined_data$timestamp <- gsub("24:", "00:", combined_data$timestamp)

# Convert the cleaned timestamp to POSIXct
combined_data$timestamp <- as.POSIXct(combined_data$timestamp, format="%a %b %d %Y %H:%M:%S")

# Format to desired output: "Fri Sep 27 2024 11:16 PM"
combined_data$timestamp_formatted <- format(combined_data$timestamp, "%a %b %d %Y %I:%M %p")


# Clean data
clean <- combined_data %>%
  select(county, county_fips, state, customer_out, est_customers_served, timestamp_formatted)

result <- clean %>%
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
total_outages <- sum(combined_data$customer_out)
total_outages <- format(total_outages, big.mark = ",", scientific = FALSE)


# Make the datawrapper
dw_upload_data("MJmSd", result, dw_api_key)

dw_update_chart(
  chart_id = "MJmSd",
  api_key = dw_api_key,
  title = "Tracking power outages along winter storm's path",
  intro = paste( "There are currently", "<b>", total_outages, "</b>", "customers experiencing power outages in the states highlighted on the map below. Click or hover over a county to see more information."),
  annotate = "Data updates daily",
  byline = "Taylor Johnston / CBS News",
  source_name = "PowerOutage.com",
  source_url = "https://poweroutage.com/",
  folder_id = "267166"
)

# Publish the chart
dw_publish("MJmSd", dw_api_key)

write.csv(result, "output/clean_data.csv")
