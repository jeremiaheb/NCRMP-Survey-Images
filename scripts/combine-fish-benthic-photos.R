# ==============================================================================
# NCRMP Folder Combiner: Merge _fish and _benthic folders into Site ID folders
# ==============================================================================
# This script requires the 'stringr' and 'rstudioapi' packages.
# install.packages(c("stringr", "rstudioapi"))

library(stringr)
library(rstudioapi)

# 1. Ask the user to select the base directory
cat("Waiting for you to select the directory containing the _fish and _benthic folders...\n")
base_dir <- selectDirectory(caption = "Select Directory with _fish and _benthic folders")

if (is.null(base_dir)) stop("Directory selection cancelled.")

cat("========================================\n")
cat("Target Directory:", base_dir, "\n")
cat("========================================\n")

# 2. Get a list of all immediate subdirectories
all_dirs <- list.dirs(base_dir, recursive = FALSE)

# 3. Filter to only include folders that end with _benthic or _fish
target_dirs <- all_dirs[grepl("_(fish|benthic)$", all_dirs, ignore.case = TRUE)]
cat("Found", length(target_dirs), "folders to process.\n\n")

if (length(target_dirs) == 0) {
  stop("No folders ending in _fish or _benthic were found in the selected directory.")
}

# 4. Extract unique Site IDs from the folder names (e.g., "4523" from "4523_fish")
folder_names <- basename(target_dirs)
site_ids <- unique(str_extract(folder_names, "^\\d+"))
site_ids <- site_ids[!is.na(site_ids)]

# 5. Loop through each Site ID, create the combined folder, and move the files
for (site in site_ids) {

  # A. Create the new combined folder (e.g., ".../4523")
  combined_dir <- file.path(base_dir, site)
  if (!dir.exists(combined_dir)) {
    dir.create(combined_dir)
  }

  # B. Find the specific _fish and _benthic folders for this exact site
  # Using regex to match exactly "4523_fish" or "4523_benthic"
  pattern <- paste0("^", site, "_(fish|benthic)$")
  source_folders <- target_dirs[grepl(pattern, basename(target_dirs), ignore.case = TRUE)]

  # C. Move the files from those folders into the new combined folder
  for (src in source_folders) {

    # Identify if this is the fish or benthic folder to use as a filename prefix
    # This prevents files with the same name from overwriting each other
    folder_type <- tolower(str_extract(basename(src), "(fish|benthic)$"))

    files <- list.files(src, full.names = TRUE)

    if (length(files) > 0) {
      for (f in files) {
        # Add the prefix to the filename (e.g., "fish_IMG123.jpg")
        new_filename <- paste0(folder_type, "_", basename(f))
        new_filepath <- file.path(combined_dir, new_filename)

        # Move the file
        file.rename(from = f, to = new_filepath)
      }
    }

    # D. Delete the old _fish or _benthic folder now that it's empty
    unlink(src, recursive = TRUE)
  }

  cat("Successfully combined photos for Site:", site, "\n")
}

cat("\n========================================\n")
cat("Finished! All folders have been combined and cleaned up.\n")
