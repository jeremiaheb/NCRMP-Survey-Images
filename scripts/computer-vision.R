# ==============================================================================
# AI Photo Ranking Test (Computer Vision with Clipboard Detection)
# ==============================================================================
# This script tests a local computer vision algorithm to rank photos by quality.
# It also searches for large blocks of white pixels to down-rank datasheets.

library(magick)
library(rstudioapi)

# 1. Ask the user to select a test directory
cat("Waiting for you to select a directory containing test photos...\n")
test_dir <- selectDirectory(caption = "Select Folder with Photos to Rank")

if (is.null(test_dir)) stop("Directory selection cancelled.")

# 2. Get list of images in that directory
imgs <- list.files(test_dir, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE, full.names = TRUE)

if (length(imgs) == 0) {
  stop("No images found in the selected directory.")
}

cat("Found", length(imgs), "images. Calculating AI quality scores...\n\n")

# Create an empty dataframe to store our results
results <- data.frame(
  FilePath = character(),
  FileName = character(),
  Contrast = numeric(),
  Sharpness = numeric(),
  MedianSat = numeric(),
  LowSatPct = numeric(),
  TotalScore = numeric(),
  stringsAsFactors = FALSE
)

# 3. Loop through each image and calculate its Computer Vision score
for (i in seq_along(imgs)) {
  img_path <- imgs[i]
  file_name <- basename(img_path)

  cat(sprintf("Analyzing [%d/%d]: %s... ", i, length(imgs), file_name))

  tryCatch({
    # A. Load the image and shrink it to 500px wide.
    img <- image_read(img_path)
    img_small <- image_scale(img, "500")
    img_gray <- image_convert(img_small, colorspace = "gray")

    # B. Get Pixel Data (0-255 scale) for contrast/sharpness
    pixels_gray <- as.vector(as.integer(image_data(img_gray)))

    # C. Calculate Contrast (Standard Deviation of pixels)
    contrast_score <- sd(pixels_gray)

    # D. Calculate Sharpness (Variance of Edges)
    img_edges <- image_edge(img_gray, radius = 1)
    pixels_edge <- as.vector(as.integer(image_data(img_edges)))
    sharpness_score <- var(pixels_edge)

    # E. CHECK FOR CLIPBOARD / DATASHEET (Saturation Calculation)
    # Extract the Saturation channel. Pixels will be 0 (no color) to 255 (full color)
    img_sat <- image_channel(img_small, "Saturation")
    pixels_sat <- as.vector(as.integer(image_data(img_sat)))

    median_sat <- median(pixels_sat)

    # Count how many pixels are severely lacking color (e.g., < 40 out of 255)
    # Datasheets reflect the ambient light but have no native color/pigment
    saturation_threshold <- 30
    desaturated_pixels <- sum(pixels_sat < saturation_threshold)
    low_sat_pct <- (desaturated_pixels / length(pixels_sat)) * 100

    # F. Calculate Base Total Score
    total_score <- contrast_score * sharpness_score

    # G. Apply the Clipboard Penalty!
    # If more than 15% of the image is completely desaturated, slash the score by 90%
    if (low_sat_pct > 15) {
      total_score <- total_score * 0.1
      cat(" [CLIPBOARD PENALTY APPLIED] ")
    }

    # Save results
    results <- rbind(results, data.frame(
      FilePath = img_path,
      FileName = file_name,
      Contrast = round(contrast_score, 2),
      Sharpness = round(sharpness_score, 2),
      MedianSat = median_sat,
      LowSatPct = round(low_sat_pct, 2),
      TotalScore = round(total_score, 0)
    ))

    cat("Score:", round(total_score, 0), "\n")

  }, error = function(e) {
    cat("Error reading image. Skipping.\n")
  })
}

# 4. Rank the images based on Total Score (Highest to Lowest)
results <- results[order(-results$TotalScore), ]
results$Rank <- 1:nrow(results)

cat("\n========================================\n")
cat("Ranking Complete! Renaming files...\n")
cat("========================================\n")

# 5. Rename the files in the directory so you can visually verify the results
for (i in 1:nrow(results)) {
  old_path <- results$FilePath[i]

  # Strip out any previous "RankX_" if you run this script multiple times
  clean_name <- sub("^Rank\\d+_", "", results$FileName[i])

  # Create the new filename with the rank (e.g., "Rank1_IMG123.jpg")
  new_name <- paste0("Rank", results$Rank[i], "_", clean_name)
  new_path <- file.path(dirname(old_path), new_name)

  file.rename(from = old_path, to = new_path)

  cat(sprintf("Rank %d: %s (Score: %d | MedianSat: %d | LowSatArea: %.1f%%)\n",
              results$Rank[i], clean_name, results$TotalScore[i],
              results$MedianSat[i], results$LowSatPct[i]))
}

cat("\nFinished! Open the folder on your computer to see how well the AI ranked your photos.\n")
