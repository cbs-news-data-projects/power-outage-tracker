library(dplyr)

# Reading in cleaned data
result <- read.csv("output/clean_data.csv")

# Filter counties with at least 80% or 90% of customers without power
counties_80_percent <- result %>% filter(pct_customers_out >= 80)
counties_90_percent <- result %>% filter(pct_customers_out >= 90)

# Get the number of counties
num_counties_80 <- nrow(counties_80_percent)
num_counties_90 <- nrow(counties_90_percent)

# Get the current time (formatted as of today)
current_time <- format(Sys.time(), "%I:%M %p")

# Display the information
cat("There are", num_counties_80, "counties where at least 80% of people are without power as of", current_time, "today.\n")
cat("There are", num_counties_90, "counties where at least 90% of people are without power as of", current_time, "today.\n")