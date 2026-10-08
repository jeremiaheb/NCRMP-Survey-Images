# ==============================================================================
# NCRMP Batch Image Resizer and Renamer (Interactive)
# ==============================================================================
library(magick)
library(stringr)
library(rstudioapi)

# 1. Interactive Directory Selection
cat("Waiting for you to select the INPUT directory...\n")
input_dir <- selectDirectory(caption = "Select INPUT Directory (Downloaded Folders)")

if (is.null(input_dir)) stop("Input directory selection was cancelled.")

cat("Waiting for you to select the OUTPUT directory...\n")
output_dir <- selectDirectory(caption = "Select OUTPUT Directory (For Processed Folders)")

if (is.null(output_dir)) stop("Output directory selection was cancelled.")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("========================================\n")
cat("Input: ", input_dir, "\n")
cat("Output:", output_dir, "\n")
cat("========================================\n")

# 2. Get a list of all site directories in the input folder
all_dirs <- list.dirs(input_dir, recursive = TRUE, full.names = TRUE)

# Exclude the parent directory
all_dirs <- all_dirs[all_dirs != input_dir]

# Filter to only include folders whose names are entirely numbers (e.g., "113", "5011")
target_dirs <- all_dirs[grepl("^\\d+$", basename(all_dirs))]

cat("Found", length(target_dirs), "folders to scan...\n")

# 3. Loop through each folder and process the images
for (dir in target_dirs) {

  site_id <- basename(dir)

  # Failsafe: Skip if no numbers were found
  if (is.na(site_id) || site_id == "") next

  # Grab all image files immediately inside the current input folder
  imgs <- list.files(dir, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE, full.names = TRUE)

  # --- PASSIVE FILTERING LOGIC ---
  if (length(imgs) == 0) {
    cat("Skipping Folder:", site_id, "- No images found (Excluded from output).\n")
    next
  }

  # If images exist, NOW we create the clean target directory in the output folder
  target_site_dir <- file.path(output_dir, site_id)
  if (!dir.exists(target_site_dir)) dir.create(target_site_dir)

  # --- SORTING LOGIC ---
  # Extract the rank from the filename (e.g., "1_4125.JPG" -> 1)
  base_names <- basename(imgs)
  ranks <- as.numeric(sub("^([0-9]+)_.*", "\\1", base_names))

  # Sort the image paths numerically by their extracted rank
  sorted_imgs <- imgs[order(ranks)]

  # Grab up to the top 4 images
  imgs_to_process <- head(sorted_imgs, 4)

  cat("Processing Site:", site_id, "- Formatting top", length(imgs_to_process), "images\n")

  # Loop through the selected images
  for (i in seq_along(imgs_to_process)) {

    img_path <- imgs_to_process[i]
    current_rank <- ranks[order(ranks)][i]

    # Read and resize the image
    img <- image_read(img_path)
    img_resized <- image_scale(img, "800")

    # Define the new filename using its AI rank
    out_path <- file.path(target_site_dir, paste0(current_rank, ".jpg"))

    # Write the compressed JPEG
    image_write(img_resized, path = out_path, format = "jpeg", quality = 80)
  }
}

cat("\n========================================\n")
cat("Finished processing all valid folders!\n")
