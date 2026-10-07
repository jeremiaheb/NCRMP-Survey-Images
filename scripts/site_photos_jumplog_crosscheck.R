# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(rstudioapi)

# 1. Interactive Inputs
cat("Please select the parent directory containing the site folders...\n")
parent_dir <- rstudioapi::selectDirectory(caption = "Select Parent Directory")
if (is.null(parent_dir) || parent_dir == "") stop("Directory selection cancelled.")

cat("Please select the Excel jumplog spreadsheet...\n")
excel_path <- rstudioapi::selectFile(
  caption = "Select Excel File",
  filter = "Excel Files (*.xls, *.xlsx)"
)
if (is.null(excel_path)) stop("Excel file selection cancelled.")

# 2. Find and Load the Correct Worksheet
sheets <- excel_sheets(excel_path)
target_sheet <- sheets[grepl("_jumplog_corrected", sheets, ignore.case = TRUE)]

if (length(target_sheet) == 0) {
  stop("No worksheet found containing '_jumplog_corrected' in the name.")
} else if (length(target_sheet) > 1) {
  cat("Multiple matching sheets found. Defaulting to the first one:", target_sheet[1], "\n")
  target_sheet <- target_sheet[1]
}

cat("Reading data from sheet:", target_sheet, "\n")
df <- read_excel(excel_path, sheet = target_sheet)

# 3. Extract site IDs from the parent directory's folders
folder_names <- basename(list.dirs(parent_dir, recursive = FALSE))
folder_site_ids <- unique(str_extract(folder_names, "^\\d{4}"))
folder_site_ids <- folder_site_ids[!is.na(folder_site_ids)]

# 4. Process and Reshape the Jumplog Data
df_parsed <- df %>%
  mutate(
    # Extract the first 4 digits of the fieldid
    site_id = substr(fieldid, 1, 4),

    # Extract the diver role (A, B, J, X)
    role = toupper(str_extract(fieldid, "[A-Za-z]$")),

    # Extract only the last name (everything before the first underscore)
    last_name = str_extract(DIVER_NAME, "^[^_]+"),

    # Format the date as mm/dd/yyyy
    date_fmt = format(as.Date(date), "%m/%d/%Y")
  ) %>%
  # Keep only valid roles and site IDs
  filter(role %in% c("A", "B", "J", "X") & !is.na(site_id)) %>%
  select(site_id, role, date_fmt, last_name)

# Pivot wider so every role gets its own date and last_name column
df_wide <- df_parsed %>%
  pivot_wider(
    names_from = role,
    values_from = c(date_fmt, last_name),
    names_glue = "{.value}_{role}"
  )

# 5. Failsafe: Ensure all standard columns exist just in case a role is entirely missing
expected_cols <- c(
  "date_fmt_A", "date_fmt_B", "date_fmt_J", "date_fmt_X",
  "last_name_A", "last_name_B", "last_name_J", "last_name_X"
)

for (col in expected_cols) {
  if (!col %in% names(df_wide)) {
    df_wide[[col]] <- NA_character_
  }
}

# 6. Build the Final Output Structure
df_final <- df_wide %>%
  mutate(
    # Coalesce grabs the first non-NA date for that survey group
    fish_date = coalesce(date_fmt_A, date_fmt_B),
    benthic_date = coalesce(date_fmt_J, date_fmt_X),

    # Combine Fish Divers (A and B)
    fish_divers = case_when(
      !is.na(last_name_A) & !is.na(last_name_B) ~ paste(last_name_A, last_name_B, sep = "/"),
      !is.na(last_name_A) ~ last_name_A,
      !is.na(last_name_B) ~ last_name_B,
      TRUE ~ NA_character_
    ),

    # Combine Benthic Divers (J and X)
    benthic_divers = case_when(
      !is.na(last_name_J) & !is.na(last_name_X) ~ paste(last_name_J, last_name_X, sep = "/"),
      !is.na(last_name_J) ~ last_name_J,
      !is.na(last_name_X) ~ last_name_X,
      TRUE ~ NA_character_
    ),

    # Crosscheck against folder IDs
    Has_Site_Photos = ifelse(site_id %in% folder_site_ids, "Yes", "No")
  ) %>%
  # Clean up and order exactly as requested
  select(
    site_id,
    fish_date,
    fish_divers,
    benthic_date,
    benthic_divers,
    Has_Site_Photos
  ) %>%
  arrange(site_id)

# 7. Output Result
cat("\n=======================================================\n")
cat("Data reshaping and crosschecking complete.\n")
cat("Matched", sum(df_final$Has_Site_Photos == "Yes"), "sites with photos out of", nrow(df_final), "total sites in the jumplog.\n")

# View the final dataset in RStudio
View(df_final)

# Optional: Save it back to a CSV in the parent directory
output_file <- file.path(parent_dir, "jumplog_photo_crosscheck.csv")
write.csv(df_final, output_file, row.names = FALSE)
cat("Results saved to:", output_file, "\n")
