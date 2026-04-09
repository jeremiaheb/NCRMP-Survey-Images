# ==============================================================================
# NCRMP Metadata Updater: Add "Has_Photos" column to survey data
# ==============================================================================
# This script reads your processed image directories and updates your CSV
# so the Leaflet map knows exactly which sites have photos available to filter.
# It explicitly checks for the Year/Region/SurveyID hierarchy.

library(readr)
library(dplyr)
library(rstudioapi)

# 1. Select the CSV file
cat("Waiting for you to select your survey_data.csv...\n")
csv_path <- selectFile(
  caption = "Select your survey_data.csv file",
  filter = "CSV Files (*.csv)",
  existing = TRUE
)

if (is.null(csv_path)) stop("CSV selection cancelled.")

# 2. Select the base directory containing the Year folders
cat("Waiting for you to select the base Output directory...\n")
img_dir <- selectDirectory(
  caption = "Select the root Output directory (the folder containing '2025', '2021', etc.)"
)

if (is.null(img_dir)) stop("Directory selection cancelled.")

# 3. Read the data
data <- read_csv(csv_path, show_col_types = FALSE)

# 4. Update the dataset by checking if the exact hierarchical folder exists
data <- data %>%
  mutate(
    # A. Map Region to the shortened abbreviation (must match your folder structure!)
    CleanRegion = case_when(
      Region == "St. Thomas/John" ~ "STTSTJ",
      Region == "St. Croix" ~ "STX",
      Region == "Puerto Rico" ~ "PRICO",
      TRUE ~ gsub("/", "_", Region)
    ),

    # B. Build the full expected folder path on your hard drive
    # Example: "C:/My_Photos/2025/STTSTJ/5011"
    Expected_Folder = file.path(img_dir, as.character(Year), CleanRegion, as.character(SurveyID)),

    # C. Check if that exact folder exists, assign Yes/No
    Has_Photos = ifelse(dir.exists(Expected_Folder), "Yes", "No")
  ) %>%
  # Clean up the temporary columns so they don't get saved into the CSV
  select(-CleanRegion, -Expected_Folder)

# 5. Count how many we matched for the console output
matched_count <- sum(data$Has_Photos == "Yes")
cat("Matched exact Year/Region/SurveyID folders for", matched_count, "out of", nrow(data), "total survey sites.\n")

# 6. Save the updated CSV (overwriting the original)
write_csv(data, csv_path)
cat("Successfully updated and saved:", csv_path, "\n")
cat("Your map dashboard will now accurately filter by 'Has Photos'!\n")
