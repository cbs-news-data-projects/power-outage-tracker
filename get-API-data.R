library(httr)
library(jsonlite)
library(dplyr)
library(dotenv)
library(tibble)
library(lubridate)

source("datawrapper.R")


# ----------------------------
# Load env vars
# ----------------------------
tryCatch(load_dot_env(), error = function(e) {})

dw_api_key <- Sys.getenv("DW_API_KEY")
poweroutage_api_key <- Sys.getenv("POWEROUTAGE_API_KEY")

if (poweroutage_api_key == "") {
  stop("POWEROUTAGE_API_KEY not found")
}

# ----------------------------
# Pull county outage data
# ----------------------------
base_url <- "https://poweroutage.us/api/json_v1.6/county"

response <- GET(
  base_url,
  query = list(key = poweroutage_api_key)
)

data <- content(response, as = "text", encoding = "UTF-8") |>
  fromJSON(flatten = TRUE) |>
  as_tibble() |>
  select(
    US_State_FIPS,
    US_Full_FIPS,
    CountyName,
    CustomersTracked,
    CustomersOut,
    LastUpdatedDateTime
  )

# ----------------------------
# BLS USPS + FIPS lookup table
# Source:
# https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm
# ----------------------------
state_lookup <- tibble(
  US_State_FIPS = c(
    "01","02","04","05","06","08","09","10","11","12","13","15","16","17",
    "18","19","20","21","22","23","24","25","26","27","28","29","30","31",
    "32","33","34","35","36","37","38","39","40","41","42","44","45","46",
    "47","48","49","50","51","53","54","55","56"
  ),
  StateAbbrev = c(
    "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL",
    "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
    "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD",
    "TN","TX","UT","VT","VA","WA","WV","WI","WY"
  ),
  StateName = c(
    "Alabama","Alaska","Arizona","Arkansas","California","Colorado",
    "Connecticut","Delaware","District of Columbia","Florida","Georgia",
    "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
    "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
    "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
    "New Jersey","New Mexico","New York","North Carolina","North Dakota",
    "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
    "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia",
    "Washington","West Virginia","Wisconsin","Wyoming"
  )
)

# ----------------------------
# Join state names to counties
# ----------------------------
# After joining state_lookup and computing pct_out

data <- data %>%
  mutate(
    US_State_FIPS = sprintf("%02d", US_State_FIPS),
    
    # Compute pct_out
    pct_out = round(CustomersOut / CustomersTracked * 100, 1),
    
    # If pct_out is 0, NA, or NaN → leave blank
    pct_out = ifelse(is.na(pct_out) | is.nan(pct_out), "", as.character(pct_out)),
    
    # Parse UTC datetime from API
    LastUpdatedDateTime = as.POSIXct(
      LastUpdatedDateTime,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    
    # Convert UTC to Eastern Time
    LastUpdatedET = with_tz(LastUpdatedDateTime, tzone = "America/New_York"),
    
    # Friendly format: Jan. 30 at 9:40 am ET
    LastUpdatedET = format(
      LastUpdatedET,
      "%b. %d at %I:%M %p ET"
    ),
    
    # Remove leading zero from hour
    LastUpdatedET = sub("^0", "", LastUpdatedET),
    
    # Total customers out per county
    total_out = CustomersOut
  ) %>%
  # Add state names and abbreviations
  left_join(state_lookup, by = "US_State_FIPS") %>%
  # Keep only valid rows
  filter(
    !is.na(StateAbbrev),
    !is.na(StateName),
    !is.na(US_Full_FIPS)
  )
total_people_out <- sum(data$CustomersOut, na.rm = TRUE)
total_people_out <- format(total_people_out, big.mark = ",", scientific = FALSE)
# Top three states by customers out, e.g. "<b>1,200</b> in Florida, <b>900</b> in Texas and <b>400</b> in Georgia"
top_states <- data %>%
  group_by(StateName) %>%
  summarise(out = sum(CustomersOut, na.rm = TRUE), .groups = "drop") %>%
  filter(out > 0) %>%
  arrange(desc(out)) %>%
  slice_head(n = 3)
top_parts <- paste0("<b>", format(top_states$out, big.mark = ",", trim = TRUE),
                    "</b> in ", top_states$StateName)
n_parts <- length(top_parts)
top_states_text <- if (n_parts == 0) {
  ""
} else if (n_parts == 1) {
  top_parts
} else {
  paste0(paste(top_parts[-n_parts], collapse = ", "), " and ", top_parts[n_parts])
}
top_states_sentence <- if (n_parts == 0) NULL else paste0(
  "The outages include ", top_states_text, "."
)

# Newest county timestamp from the API (data freshness, not run time)
latest_update <- max(data$LastUpdatedDateTime, na.rm = TRUE) %>%
  with_tz(tzone = "America/New_York") %>%
  format("%b. %d at %I:%M %p ET") %>%
  sub("^(\\w+\\. )0", "\\1", .) %>%
  sub(" at 0", " at ", .)

# ----------------------------
# Push to Datawrapper
# ----------------------------
dw_upload_data("MJmSd", data, dw_api_key)

dw_update_chart(
  chart_id = "MJmSd",
  api_key = dw_api_key,
  title = "Tracking power outages",
  intro = paste( "There are currently", "<b>", total_people_out, "</b>", "customers experiencing power outages in the states highlighted on the map below.", top_states_sentence, "Click or hover over a county to see more information."),
  annotate = paste0("Data updates twice daily. Last updated: ", latest_update, "."),
  byline = "",
  source_name = "PowerOutage.com",
  source_url = "https://poweroutage.com/"
)

dw_publish("MJmSd", dw_api_key)