# ==============================================================================
# NCRMP Folder Combiner: Merge any folders into their base Site ID folder
# ==============================================================================
# This script requires the 'stringr' and 'rstudioapi' packages.
# install.packages(c("stringr", "rstudioapi"))

library(stringr)
library(rstudioapi)

# 1. Ask the user to select the base directory
cat("Waiting for you to select the directory containing the site folders...\n")
base_dir <- selectDirectory(caption = "Select Directory with Site folders")

if (is.null(base_dir)) stop("Directory selection cancelled.")

cat("========================================\n")
cat("Target Directory:", base_dir, "\n")
cat("========================================\n")

# 2. Get a list of all immediate subdirectories
all_dirs <- list.dirs(base_dir, recursive = FALSE)

# 3. Filter to only include folders that start with a Site ID (numbers)
target_dirs <- all_dirs[grepl("^\\d+", basename(all_dirs))]
cat("Found", length(target_dirs), "folders starting with a numeric Site ID.\n\n")

if (length(target_dirs) == 0) {
  stop("No folders starting with numbers were found in the selected directory.")
}

# 4. Extract unique Site IDs from the folder names (e.g., "4523" from "4523_fish_misspelled")
folder_names <- basename(target_dirs)
site_ids <- unique(str_extract(folder_names, "^\\d+"))
site_ids <- site_ids[!is.na(site_ids)]

# 5. Loop through each Site ID, create the combined folder, and move the files
for (site in site_ids) {

  # A. Create the new clean combined folder (e.g., ".../4523")
  combined_dir <- file.path(base_dir, site)
  if (!dir.exists(combined_dir)) {
    dir.create(combined_dir)
  }

  # B. Find all folders that start with this exact Site ID
  # We check that the extracted leading number matches the current 'site'
  source_folders <- target_dirs[str_extract(basename(target_dirs), "^\\d+") == site]

  # Exclude the combined folder itself if it was already in the list to prevent errors
  source_folders <- source_folders[basename(source_folders) != site]

  # C. Move the files from those folders into the new combined folder
  for (src in source_folders) {

    # Identify the suffix of the folder to use as a filename prefix
    # e.g., "4523_fish_survey" -> "fish_survey", "4523benthic" -> "benthic"
    folder_suffix <- sub(paste0("^", site, "[_\\-]?"), "", basename(src))

    # Clean up the suffix just in case there was no text after the numbers
    if (nchar(folder_suffix) == 0) folder_suffix <- "misc"

    files <- list.files(src, full.names = TRUE)

    # --- NEW FILTERING LOGIC ---
    # Exclude files that start with a dot (e.g., .DS_Store) OR end with .ini (e.g., desktop.ini)
    files <- files[!grepl("^\\.|\\.ini$", basename(files), ignore.case = TRUE)]
    # ---------------------------

    if (length(files) > 0) {
      for (f in files) {
        # Add the folder suffix prefix to the filename (e.g., "fish_survey_IMG123.jpg")
        new_filename <- paste0(folder_suffix, "_", basename(f))
        new_filepath <- file.path(combined_dir, new_filename)

        # Move the file
        file.rename(from = f, to = new_filepath)
      }
    }

    # D. Delete the old folder now that it is empty (this also deletes the leftover junk files)
    unlink(src, recursive = TRUE)
  }

  cat("Successfully combined photos for Site:", site, "\n")
}

cat("\n========================================\n")
cat("Finished! All folders have been combined and cleaned up.\n")
