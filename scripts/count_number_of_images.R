library(rstudioapi)

# 1. Select the parent directory containing all the subfolders
cat("Please select the parent directory...\n")
parent_dir <- rstudioapi::selectDirectory(caption = "Select Parent Directory")

if (is.null(parent_dir) || parent_dir == "") {
  stop("Directory selection cancelled.")
}

# 2. Get a list of all immediate subdirectories
subfolders <- list.dirs(parent_dir, recursive = FALSE, full.names = TRUE)

if (length(subfolders) == 0) {
  stop("No subfolders found in the selected directory.")
}

# 3. Initialize a vector to store the names of folders with > 12 images
flagged_folders <- c()

cat("Scanning", length(subfolders), "folders for image counts...\n\n")

# 4. Loop through each folder to count the JPGs
for (folder in subfolders) {

  # List all JPG/JPEG files in this specific folder (case-insensitive)
  images <- list.files(folder, pattern = "\\.(jpg|jpeg)$", ignore.case = TRUE)

  image_count <- length(images)

  # If the count exceeds 12, extract the folder name and add it to our list
  if (image_count > 12) {
    folder_name <- basename(folder)
    flagged_folders <- c(flagged_folders, folder_name)

    # Print to console as it finds them
    cat(sprintf("[FLAGGED] Folder '%s' contains %d images.\n", folder_name, image_count))
  }
}

# 5. Final Summary Output
cat("\n=======================================================\n")
if (length(flagged_folders) > 0) {
  cat(sprintf("Found %d folder(s) with more than 12 images.\n", length(flagged_folders)))
  # Returns the actual vector of names in the console
  print(flagged_folders)
} else {
  cat("All clean! No folders have more than 12 images.\n")
}
