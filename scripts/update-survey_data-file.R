# ==============================================================================
# NCRMP Data Updater: Fetch and append new RVC survey data to master CSV
# ==============================================================================
library(rvc)
library(dplyr)
library(readr)
library(rstudioapi)

# 1. Interactive User Inputs
year_input <- rstudioapi::showPrompt(
  title = "Input Survey Year",
  message = "Enter the survey Year (e.g., 2024):"
)
if (is.null(year_input) || year_input == "") stop("Year input cancelled.")
year_input <- as.numeric(year_input)
if (is.na(year_input)) stop("Invalid year provided.")

region_code <- rstudioapi::showPrompt(
  title = "Input RVC Region",
  message = "Enter the RVC Region Code (e.g., FLA KEYS, DRY TORT, PRICO, STTSTJ, STX):"
)
if (is.null(region_code) || region_code == "") stop("Region input cancelled.")
region_code <- toupper(trimws(region_code))

# 2. Map RVC Server Codes to Dashboard Clean Names
region_mapping <- c(
  "FLA KEYS" = "Florida Keys",
  "DRY TORT" = "Dry Tortugas",
  "SEFCRI"   = "Southeast Florida",
  "FGBNMS"   = "Flower Garden Banks",
  "PRICO"    = "Puerto Rico",
  "STTSTJ"   = "St. Thomas/John",
  "STX"      = "St. Croix"
)

clean_region <- region_mapping[region_code]
if (is.na(clean_region)) {
  warning("Region code not recognized in standard mapping. Using raw input.")
  clean_region <- region_code
}

# 3. Select and Load the Master CSV
cat("Please select your master survey_data.csv file...\n")
csv_path <- rstudioapi::selectFile(
  caption = "Select your survey_data.csv file",
  filter = "CSV Files (*.csv)",
  existing = TRUE
)
if (is.null(csv_path)) stop("CSV selection cancelled.")

master_data <- read_csv(csv_path, show_col_types = FALSE)

# 4. Validation: Check for Pre-existing Data
existing_check <- master_data %>%
  filter(Year == year_input, Region == clean_region)

if (nrow(existing_check) > 0) {
  stop(sprintf("\n[ABORTED] %d survey sites for '%s' in %d already exist in the dataset! Run aborted to prevent duplicates.",
               nrow(existing_check), clean_region, year_input))
}

# 5. Download Data from RVC API
cat(sprintf("\nFetching RVC data for %s (%d). This may take a moment...\n", region_code, year_input))

rvc_raw <- tryCatch({
  getRvcData(years = year_input, regions = region_code)
}, error = function(e) {
  stop("\n[API ERROR] Failed to download data from RVC server. Ensure year/region are valid on the server.\nDetails: ", e$message)
})

# 6. Validation: Check for Empty Payload
if (is.null(rvc_raw$sample_data) || nrow(rvc_raw$sample_data) == 0) {
  stop("\n[DATA ERROR] The RVC server returned empty sample_data for this Year/Region combination.")
}

# 7. Process and Format the New Data
cat("Processing downloaded survey data...\n")
new_data <- rvc_raw$sample_data %>%
  group_by(REGION, YEAR, PRIMARY_SAMPLE_UNIT) %>%
  summarise(
    Lat = mean(LAT_DEGREES, na.rm = TRUE),
    Lon = mean(LON_DEGREES, na.rm = TRUE),
    Depth = mean(DEPTH, na.rm = TRUE),
    Habitat = first(HABITAT_CD),
    .groups = "drop" # Drops grouping to prevent downstream warnings
  ) %>%
  mutate(
    Region = clean_region,
    Has_Photos = "No"
  ) %>%
  rename(
    Year = YEAR,
    SurveyID = PRIMARY_SAMPLE_UNIT
  ) %>%
  # Select and order columns to align closely with standard master_data
  select(SurveyID, Region, Year, Depth, Habitat, Lat, Lon, Has_Photos)

# 8. Append and Save
updated_data <- bind_rows(master_data, new_data)
write_csv(updated_data, csv_path)

cat("\n=======================================================\n")
cat(sprintf("SUCCESS: Appended %d new survey sites for %s %d to the master CSV.\n", nrow(new_data), clean_region, year_input))
cat("File saved to:", csv_path, "\n")

# NOTE: If you are using .parquet files for your Quarto app performance,
# you can easily convert this updated CSV to parquet here:
# arrow::write_parquet(updated_data, sub("\\.csv$", ".parquet", csv_path))
