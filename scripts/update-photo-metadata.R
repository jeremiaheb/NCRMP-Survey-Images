# ==============================================================================
# NCRMP Metadata Updater: Add "Has_Photos" column from GCS Bucket
# ==============================================================================
# This script reads your survey data and checks a Google Cloud Storage bucket
# to see if the exact Year/Region/SurveyID prefix exists, updating the CSV.

library(readr)
library(dplyr)
library(rstudioapi)
library(httr2)
library(gargle)
library(jsonlite)
library(arrow)

# --- HARDCODED VARIABLES ---
gcs_bucket_url <- "https://storage.googleapis.com/ncrmp-atlantic-photos"
bucket_name <- "ncrmp-atlantic-photos"
# ---------------------------

# 1. Interactive Inputs
cat("Waiting for you to select your survey_data.csv...\n")
csv_path <- selectFile(
  caption = "Select your survey_data.csv file",
  filter = "CSV Files (*.csv)",
  existing = TRUE
)

if (is.null(csv_path)) stop("CSV selection cancelled.")

cat("\nPlease select your NOAA Google Service Account JSON key file...\n")
json_key_path <- selectFile(
  caption = "Select your Service Account JSON key",
  filter = "JSON Files (*.json)"
)

if (is.null(json_key_path)) stop("JSON key selection cancelled.")

# 2. Authenticate and Fetch Bucket Contents
cat("\nAuthenticating and fetching folder structure from Google Cloud Storage...\n")
credentials <- gargle::credentials_service_account(
  path = json_key_path,
  scopes = "https://www.googleapis.com/auth/cloud-platform"
)
token <- credentials$credentials$access_token

# Build the API URL using the hardcoded bucket name
base_url <- sprintf("https://storage.googleapis.com/storage/v1/b/%s/o?fields=nextPageToken,items/name", bucket_name)

all_paths <- character()
page_token <- NULL

# Pagination loop (required if you have thousands of photos)
repeat {
  url <- base_url
  if (!is.null(page_token)) {
    url <- paste0(url, "&pageToken=", page_token)
  }

  req <- request(url) |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_error(is_error = ~ FALSE)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    stop(paste("Failed to read bucket. HTTP Error:", resp_status(resp), "\nDetails:", resp_body_string(resp)))
  }

  body <- resp_body_json(resp)

  if (!is.null(body$items)) {
    names <- sapply(body$items, function(x) x$name)
    all_paths <- c(all_paths, names)
  }

  page_token <- body$nextPageToken
  if (is.null(page_token)) break
}

# Extract unique "folders" from the full file paths (e.g., "2025/STTSTJ/5011")
bucket_folders <- unique(dirname(all_paths))

cat("Successfully indexed", length(bucket_folders), "unique site folders in the bucket.\n\n")

# 3. Read the data
data <- read_csv(csv_path, show_col_types = FALSE)

# 4. Update the dataset by checking against the bucket list
data <- data %>%
  mutate(
    # A. Map Region to the shortened abbreviation (must match your folder structure!)
    CleanRegion = case_when(
      Region == "St. Thomas/John" ~ "STTSTJ",
      Region == "St. Croix" ~ "STX",
      Region == "Puerto Rico" ~ "PRICO",
      Region == "Dry Tortugas" ~ "DRTO",
      Region == "Florida Keys" ~ "FKEYS",
      Region == "Southeast Florida" ~ "SEFL",
      Region == "Flower Garden Banks" ~ "FGB",
      TRUE ~ gsub("/", "_", Region)
    ),

    # B. Build the expected cloud prefix (using forward slashes)
    Expected_Prefix = paste(Year, CleanRegion, SurveyID, sep = "/"),

    # C. Check if that exact prefix exists in our pulled bucket list
    Has_Photos = ifelse(Expected_Prefix %in% bucket_folders, "Yes", "No")
  ) %>%
  # Clean up the temporary columns so they don't get saved into the CSV
  select(-CleanRegion, -Expected_Prefix)

# 5. Count how many we matched for the console output
matched_count <- sum(data$Has_Photos == "Yes")
cat("Matched exact Year/Region/SurveyID folders for", matched_count, "out of", nrow(data), "total survey sites.\n")

# 6. Save the updated CSV (overwriting the original)
write_csv(data, csv_path)
cat("Successfully updated and saved:", csv_path, "\n")
cat("Your map dashboard will now accurately filter by 'Has Photos'!\n")
