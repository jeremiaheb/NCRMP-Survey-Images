library(stringr)
library(rstudioapi)

# 1. Select the parent directory
cat("Please select the parent directory containing the site folders...\n")
parent_dir <- rstudioapi::selectDirectory(caption = "Select Parent Directory")

if (is.null(parent_dir) || parent_dir == "") {
  stop("Directory selection cancelled.")
}

# 2. Get a list of all immediate subdirectories
subfolders <- list.dirs(parent_dir, recursive = FALSE, full.names = TRUE)

if (length(subfolders) == 0) {
  stop("No subfolders found in the selected directory.")
}

cat("Processing", length(subfolders), "folders...\n\n")

for (folder_path in subfolders) {
  folder_name <- basename(folder_path)

  # 3. Check for any images in the folder
  images <- list.files(folder_path, pattern = "\\.(jpg|jpeg|png|tiff|bmp|gif)$", ignore.case = TRUE)

  # 4. If no images exist, delete the folder and skip to the next
  if (length(images) == 0) {
    cat(sprintf("[DELETED] '%s' - No images found in folder.\n", folder_name))
    unlink(folder_path, recursive = TRUE)
    next
  }

  # 5. Extract the base site number
  leading_numbers <- str_extract(folder_name, "^\\d+")

  if (is.na(leading_numbers)) {
    cat(sprintf("[SKIPPED] '%s' - Does not start with a numeric Site ID.\n", folder_name))
    next
  }

  # Truncate to a maximum of 4 digits for the base site number (handles 3-digit sites too)
  site_num <- substr(leading_numbers, 1, 4)

  # Extract ONLY the alphabetical letters from the entire folder name
  # This ignores extra digits (like the '1' in 80901) and punctuation (like '_')
  letters_only <- toupper(gsub("[^A-Za-z]", "", folder_name))

  # 6. Apply strict renaming logic
  new_name <- NULL

  if (letters_only == "") {
    # No letters at all (e.g., "8090" or "80901")
    new_name <- paste0(site_num, "_fish")

  } else if (grepl("BENTHIC|^X$|^J$", letters_only)) {
    # If the letters exactly match "X" or "J", or contain the word "BENTHIC"
    new_name <- paste0(site_num, "_benthic")

  } else if (grepl("FISH|^A$|^B$", letters_only)) {
    # If the letters exactly match "A" or "B", or contain the word "FISH"
    new_name <- paste0(site_num, "_fish")

  } else {
    cat(sprintf("[SKIPPED] '%s' - Extracted letters '%s' not recognized.\n", folder_name, letters_only))
    next
  }

  # 7. Rename the folder
  if (!is.null(new_name)) {
    new_path <- file.path(parent_dir, new_name)

    # Check if a folder with the new name already exists
    if (dir.exists(new_path)) {
      # If the folder already has its perfect final name, just silently skip it
      if (folder_name != new_name) {
        cat(sprintf("[ERROR] Cannot rename '%s'. Destination '%s' already exists!\n", folder_name, new_name))
      }
    } else {
      file.rename(from = folder_path, to = new_path)
      cat(sprintf("[RENAMED] '%s' -> '%s'\n", folder_name, new_name))
    }
  }
}

cat("\n=======================================================\n")
cat("Finished processing all folders.\n")
