library(aws.s3)
library(dotenv)

# Set AWS credentials and region
Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("AWS_ACCESS_KEY_ID"),
           "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
           "AWS_DEFAULT_REGION" = Sys.getenv("AWS_DEFAULT_REGION"))

bucket_name <- "fe-outage-data"
file_list_path <- "downloaded_files.txt"

# Load existing file list if it exists
if (file.exists(file_list_path)) {
  downloaded_files <- readLines(file_list_path)
} else {
  downloaded_files <- character(0)  # Initialize as empty character vector
}

# Get today's date in "YYYY-MM-DD" format
today_date <- format(Sys.Date(), "%Y-%m-%d")

# Define the minutes and seconds
minute_second_pairs <- c("00:01", "15:01", "30:01", "45:01")

# Generate time intervals for all hours (00 to 23) with the same minutes and seconds
time_intervals <- sapply(0:23, function(hour) {
  paste(sprintf("%02d", hour), minute_second_pairs, sep = ":")
})

# Flatten the array to get a single vector of time intervals
time_intervals <- as.vector(time_intervals)

# Function to download the latest file for a given prefix
download_latest_file <- function(prefix) {
  # Create a vector to hold the full file paths for today
  file_paths_today <- paste0(prefix, "/", today_date, "-", time_intervals, ".csv")
  
  # List the files in the S3 bucket for today's date
  files <- get_bucket(bucket_name, prefix = paste0(prefix, "/", today_date))
  
  # Get the names of files in the bucket (extract the Key for each object)
  bucket_files <- sapply(files, function(x) x$Key)
  
  # Identify files that are relevant for today
  relevant_files <- bucket_files[basename(bucket_files) %in% basename(file_paths_today)]
  
  # Identify files that need to be downloaded (not already downloaded)
  files_to_download <- setdiff(relevant_files, downloaded_files)
  
  # Download the latest file (if available)
  if (length(files_to_download) > 0) {
    # Convert file names to datetime objects for comparison
    file_dates <- as.POSIXct(sub(paste0(prefix, "/|\\.csv"), "", files_to_download), format = "%Y-%m-%d-%H:%M:%S")
    
    # Find the index of the latest file
    latest_index <- which.max(file_dates)
    latest_file <- files_to_download[latest_index]
    
    # Specify destination of file
    destination_file_path <- file.path("data", prefix, "latest_file.csv")
    
    # Download the latest file
    save_object(object = latest_file, bucket = bucket_name, file = destination_file_path)
    
    # Update the list of downloaded files
    downloaded_files <<- unique(c(downloaded_files, latest_file))
    
    # Save the updated list of filenames
    writeLines(downloaded_files, file_list_path)
  } else {
    cat(paste("No new files to download for", prefix, "today.\n"))
  }
}

# Download the latest files for these prefixes
download_latest_file("fl")
download_latest_file("ga")
download_latest_file("tn")
download_latest_file("sc")
download_latest_file("nc")
download_latest_file("va")
download_latest_file("wv")
download_latest_file("ky")
download_latest_file("oh")

# Run Rscript -e 'source("get-s3-data.R")'