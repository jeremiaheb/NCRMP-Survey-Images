# ==============================================================================
# NCRMP Batch Image Resizer and Renamer (Interactive)
# ==============================================================================
# This script prepares downloaded Google Drive photos for Google Cloud Storage.
# It requires the 'magick', 'stringr', and 'rstudioapi' packages.
# Install them first if you don't have them: install.packages(c("magick", "stringr", "rstudioapi"))

library(magick)
library(stringr)
library(rstudioapi)

# 1. Interactive Directory Selection
cat("Waiting for you to select the INPUT directory...\n")
input_dir <- selectDirectory(caption = "Select INPUT Directory (Downloaded Folders)")

# Stop the script if the user cancels the prompt
if (is.null(input_dir)) {
  stop("Input directory selection was cancelled. Exiting script.")
}

cat("Waiting for you to select the OUTPUT directory...\n")
output_dir <- selectDirectory(caption = "Select OUTPUT Directory (For Processed Folders)")

if (is.null(output_dir)) {
  stop("Output directory selection was cancelled. Exiting script.")
}

# Create the output directory if it doesn't exist (in case they selected a parent folder)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("========================================\n")
cat("Input: ", input_dir, "\n")
cat("Output:", output_dir, "\n")
cat("========================================\n")

# 2. Get a list of all site directories in the input folder
# Changed recursive to TRUE so it searches through sub-folders automatically
all_dirs <- list.dirs(input_dir, recursive = TRUE, full.names = TRUE)

# Filter to only include folders whose names are entirely numbers (e.g., "113", "5011")
target_dirs <- all_dirs[grepl("^\\d+$", basename(all_dirs))]

cat("Found", length(target_dirs), "folders to process...\n")

# 3. Loop through each folder and process the images
for (dir in target_dirs) {

  # The folder name is exactly the Site ID
  site_id <- basename(dir)

  # Skip if no numbers were found (failsafe)
  if (is.na(site_id) || site_id == "") next

  # Create the clean target directory (e.g., "C:/.../processed_output/1068")
  target_site_dir <- file.path(output_dir, site_id)
  if (!dir.exists(target_site_dir)) dir.create(target_site_dir)

  # Grab all image files inside the current folder
  imgs <- list.files(dir, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE, full.names = TRUE)

  if (length(imgs) == 0) {
    cat("Skipping Site:", site_id, "- No images found\n")
    next
  }

  # --- NEW SORTING LOGIC ---
  # Extract the rank from the filename (e.g., "1_4125.JPG" -> 1)
  base_names <- basename(imgs)
  ranks <- as.numeric(sub("^([0-9]+)_.*", "\\1", base_names))

  # Sort the image paths numerically by their extracted rank
  sorted_imgs <- imgs[order(ranks)]

  # Grab up to the top 4 images (handles cases where there are fewer than 4)
  imgs_to_process <- head(sorted_imgs, 4)
  # -------------------------

  cat("Processing Site:", site_id, "- Formatting top", length(imgs_to_process), "images\n")

  # Loop through the selected images
  for (i in seq_along(imgs_to_process)) {

    img_path <- imgs_to_process[i]

    # Identify the actual rank of the current image to ensure correct naming
    current_rank <- ranks[order(ranks)][i]

    # Read the image using magick
    img <- image_read(img_path)

    # Resize the image so the width is exactly 800px (keeps aspect ratio)
    img_resized <- image_scale(img, "800")

    # Define the new filename using its actual AI rank (e.g., "1.jpg", "2.jpg")
    out_path <- file.path(target_site_dir, paste0(current_rank, ".jpg"))

    # Write the image as a compressed JPEG (Quality 80 is perfect for web)
    image_write(img_resized, path = out_path, format = "jpeg", quality = 80)
  }
}

cat("Finished processing all images!\n")
cat("You can now upload the contents of your output directory to your Google Cloud Bucket.\n")
