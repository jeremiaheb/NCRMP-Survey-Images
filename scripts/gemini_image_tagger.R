# Load required libraries
library(httr2)
library(magick)
library(base64enc)
library(jsonlite)
library(rstudioapi)

# 1. Securely ask for the Gemini API key
api_key <- rstudioapi::askForPassword("Enter your Google Gemini API Key:")

# 2. Select the directory containing the site photos
site_folder <- rstudioapi::selectDirectory(caption = "Select the folder containing site photos")

if (is.null(site_folder) || site_folder == "") {
  stop("No folder selected.")
}

# Automatically extract the Site ID from the folder name (e.g., /path/to/4226 -> "4226")
site_id <- basename(site_folder)
cat("Processing Site ID:", site_id, "\n")

# Get a list of all image files in the folder (jpg, jpeg, png)
image_files <- list.files(site_folder, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE, full.names = TRUE)

if(length(image_files) == 0) {
  stop("No images found in the selected directory.")
}

# 3. Process and compress images to save API bandwidth
cat("Processing", length(image_files), "images...\n")

# Start building the parts list with updated criteria
parts_list <- list(
  list(
    text = "You are an expert marine biologist assistant. I am providing you with a batch of photos from a single coral reef survey site. Your task is to rank ALL of the provided photos from best to worst based on how well they represent the reef landscape.

    CRITERIA FOR RANKING:
    1. Higher Rank: Clear and sharp visibility of the reef, showing the general landscape/benthic habitat and from a ground-level point of view.
    2. Lower Rank: Blurry, poor lighting, obscured views, aerial view persepectives.
    3. Lowest Rank (Penalize): Any photos showing clipboards, datasheets MUST be ranked at the very bottom of the list and prepend with X instead of rank number.

    Please return ONLY a JSON array containing the exact filenames of ALL the photos, ordered from best (first) to worst (last). For example: [\"best_photo.jpg\", \"second_best.jpg\", ..., \"worst_photo_with_clipboard.jpg\"]"
  )
)

for (img_path in image_files) {
  # Read and resize image
  img <- image_read(img_path)
  img_resized <- image_scale(img, "800x800")

  # Convert to raw memory buffer then to base64
  img_raw <- image_write(img_resized, format = "jpeg")
  img_b64 <- base64encode(img_raw)

  # Extract filename
  file_name <- basename(img_path)

  # Append the filename as text
  parts_list <- append(parts_list, list(list(text = paste("Filename:", file_name))))

  # Append the inline image data in the correct REST format
  parts_list <- append(parts_list, list(
    list(
      inline_data = list(
        mime_type = "image/jpeg",
        data = img_b64
      )
    )
  ))
}

# 4. Prepare the API request using the frontier model
model_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.6-flash:generateContent"

body <- list(
  contents = list(
    list(
      parts = parts_list
    )
  ),
  generationConfig = list(
    temperature = 0.2,
    responseMimeType = "application/json"
  )
)

# 5. Send the request
cat("Sending to Gemini API...\n")
req <- request(model_url) |>
  req_headers(
    "x-goog-api-key" = api_key,
    "Content-Type" = "application/json"
  ) |>
  req_body_json(body)

# Execute request
resp <- req_perform(req)
resp_json <- resp_body_json(resp)

# 6. Extract the results
ai_response <- resp_json$candidates[[1]]$content$parts[[1]]$text

# Print the final ranked list of photos
cat("\n=== GEMINI'S RANKED PHOTOS (BEST TO WORST) ===\n")
cat(ai_response, "\n")

# 7. Parse the JSON response and rename the files in the directory
cat("\nRenaming files in the directory...\n")

# Convert the JSON string returned by Gemini into an R list/vector
ranked_filenames <- fromJSON(ai_response)

for (i in seq_along(ranked_filenames)) {
  original_name <- ranked_filenames[i]
  old_path <- file.path(site_folder, original_name)

  if (file.exists(old_path)) {
    # Extract the original extension and uppercase it
    ext <- toupper(tools::file_ext(original_name))

    # Construct new name: Rank_SiteID.EXT (e.g., 1_4226.JPG)
    new_name <- paste0(i, "_", site_id, ".", ext)
    new_path <- file.path(site_folder, new_name)

    file.rename(from = old_path, to = new_path)
    cat("Renamed:", original_name, "->", new_name, "\n")
  } else {
    warning(paste("File not found, skipping:", original_name))
  }
}

cat("\nDone! Check your folder to see the sorted images.\n")
