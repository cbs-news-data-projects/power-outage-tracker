library(aws.s3)
library(dotenv)

# Set AWS credentials and region
Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("AWS_ACCESS_KEY_ID"),
           "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
           "AWS_DEFAULT_REGION" = Sys.getenv("AWS_DEFAULT_REGION"))

bucket_name <- "fe-outage-data"
file_list_path <- "downloaded_files.txt"

# Load existing file list if it exists
downloaded_files <- if (file.exists(file_list_path)) {
  readLines(file_list_path)
} else {
  character(0)  # Initialize as empty character vector
}

# Get today's date in "YYYY-MM-DD" format
today_date <- format(Sys.Date(), "%Y-%m-%d")

# Define time intervals (minutes and seconds)
minute_second_pairs <- c("00:01", "15:01", "30:01", "45:01")

# Generate time intervals for all hours (00 to 23)
time_intervals <- sprintf("%02d:%s", rep(0:23, each = length(minute_second_pairs)), rep(minute_second_pairs, times = 24))

# Function to download the latest file for a given prefix
download_latest_file <- function(prefix) {
  # Create the expected file names for today
  file_paths_today <- paste0(prefix, "/", today_date, "-", time_intervals, ".csv")
  
  # List the files in the S3 bucket for today's date
  files <- get_bucket(bucket_name, prefix = paste0(prefix, "/", today_date))
  
  # Check if files is a list and extract the names of files in the bucket
  if (is.list(files) && length(files) > 0) {
    bucket_files <- sapply(files, function(x) x$Key)
  } else {
    bucket_files <- character(0)  # Initialize as empty character vector if no files found
    cat(paste("No files found in the bucket for prefix:", prefix, "\n"))
  }
  
  # Debugging output to check the structure of bucket_files
  cat("Bucket files for prefix", prefix, ":", bucket_files, "\n")
  cat("Expected file paths today:", file_paths_today, "\n")
  
  # Ensure bucket_files is a character vector before proceeding
  if (!is.character(bucket_files)) {
    stop("Error: bucket_files is not a character vector.")
  }
  
  # Identify relevant files for today
  relevant_files <- bucket_files[basename(bucket_files) %in% basename(file_paths_today)]
  
  # Identify files that need to be downloaded (not already downloaded)
  files_to_download <- setdiff(relevant_files, downloaded_files)
  
  # Download the latest file if available
  if (length(files_to_download) > 0) {
    # Convert file names to datetime objects for comparison
    file_dates <- as.POSIXct(sub(paste0(prefix, "/|\\.csv"), "", files_to_download), format = "%Y-%m-%d-%H:%M:%S")
    
    # Find the latest file
    latest_file <- files_to_download[which.max(file_dates)]
    
    # Specify destination of file
    destination_file_path <- file.path("data", prefix, "latest_file.csv")
    
    # Create destination directory if it doesn't exist
    dir.create(dirname(destination_file_path), recursive = TRUE, showWarnings = FALSE)
    
    # Download the latest file
    save_object(object = latest_file, bucket = bucket_name, file = destination_file_path)
    
    # Update the list of downloaded files
    downloaded_files <<- unique(c(downloaded_files, latest_file))
    
    # Save the updated list of filenames
    writeLines(downloaded_files, file_list_path)
    
    cat(paste("Downloaded:", latest_file, "\n"))
  } else {
    cat(paste("No new files to download for", prefix, "today.\n"))
  }
}

# Prefixes to download
prefixes <- c("fl", "ga", "tn", "sc", "nc", "va", "wv", "ky", "oh")

# Loop through each prefix and download the latest files
lapply(prefixes, download_latest_file)