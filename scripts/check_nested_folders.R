library(rstudioapi)

# 1. Select the PARENT directory containing all site folders
cat("Please select the parent directory containing your site folders...\n")
parent_dir <- rstudioapi::selectDirectory(caption = "Select Parent Directory (Contains Site Folders)")

if (is.null(parent_dir) || parent_dir == "") {
  stop("Directory selection was cancelled. Exiting script.")
}

# 2. Get top-level site directories
site_folders <- list.dirs(parent_dir, recursive = FALSE, full.names = TRUE)

if (length(site_folders) == 0) {
  stop("No site folders found in the selected parent directory.")
}

cat("Checking", length(site_folders), "site folders for unexpected nested folders...\n")
cat("=======================================================\n\n")

# Vector to store site IDs that contain nested directories
flagged_sites <- c()

# 3. Loop through each site folder and check for subdirectories
for (site_folder in site_folders) {
  site_id <- basename(site_folder)

  # Search for subdirectories directly inside this specific site folder
  nested_dirs <- list.dirs(site_folder, recursive = FALSE, full.names = TRUE)

  # If any subdirectories exist, flag this site
  if (length(nested_dirs) > 0) {
    flagged_sites <- c(flagged_sites, site_id)

    cat(sprintf("[FLAGGED] Site Folder '%s' contains %d nested folder(s):\n", site_id, length(nested_dirs)))
    for (nested in nested_dirs) {
      cat(sprintf("   └── Nested Folder: %s\n", basename(nested)))
    }
    cat("\n")
  }
}

# 4. Summary Report
cat("=======================================================\n")
cat("                  SEARCH COMPLETE                      \n")
cat("=======================================================\n")

if (length(flagged_sites) > 0) {
  cat(sprintf("Found %d site(s) with nested folders:\n\n", length(flagged_sites)))
  print(flagged_sites)
} else {
  cat("All clean! No nested folders were found in any of the site directories.\n")
}
