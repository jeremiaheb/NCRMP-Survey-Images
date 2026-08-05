# Load required libraries
library(httr2)
library(magick)
library(base64enc)
library(jsonlite)
library(rstudioapi)

# 1. Securely ask for the Gemini API key once
api_key <- rstudioapi::askForPassword("Enter your Google Gemini API Key:")

# 2. Select the PARENT directory containing all the site folders
parent_dir <- rstudioapi::selectDirectory(caption = "Select the PARENT folder containing all Site ID folders")

if (is.null(parent_dir) || parent_dir == "") {
  stop("No parent folder selected.")
}

# Get a list of all subdirectories (site folders) inside the parent directory
site_folders <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)

if (length(site_folders) == 0) {
  stop("No site folders found inside the selected parent directory.")
}

cat("Found", length(site_folders), "site folders to process.\n\n")

# 3. Master loop to process each folder
for (folder_index in seq_along(site_folders)) {

  site_folder <- site_folders[folder_index]
  site_id <- basename(site_folder)

  cat(sprintf("\n--- Processing Folder %d of %d: Site ID %s ---\n", folder_index, length(site_folders), site_id))

  # Get images in this specific folder
  image_files <- list.files(site_folder, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE, full.names = TRUE)

  if (length(image_files) == 0) {
    cat("No images found in folder:", site_id, "- Skipping to next folder.\n")
    next
  }

  cat("Preparing", length(image_files), "images...\n")

  # Start building the parts list with the criteria
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

  # Process and compress images for this folder
  for (img_path in image_files) {
    img <- image_read(img_path)
    img_resized <- image_scale(img, "800x800")
    img_raw <- image_write(img_resized, format = "jpeg")
    img_b64 <- base64encode(img_raw)

    file_name <- basename(img_path)

    parts_list <- append(parts_list, list(list(text = paste("Filename:", file_name))))
    parts_list <- append(parts_list, list(
      list(
        inline_data = list(
          mime_type = "image/jpeg",
          data = img_b64
        )
      )
    ))
  }

  # 4. Prepare the API request
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

  # 5. Build the request with automatic retry logic for 503s and 429s
  req <- request(model_url) |>
    req_headers(
      "x-goog-api-key" = api_key,
      "Content-Type" = "application/json"
    ) |>
    req_body_json(body) |>
    req_retry(
      max_tries = 5,
      is_transient = function(resp) {
        status <- resp_status(resp)
        if (status %in% c(429, 503, 504)) {
          cat(sprintf("\n[SERVER ERROR %d] Temporary failure. Waiting to retry...\n", status))
          return(TRUE)
        }
        return(FALSE)
      }
    )

  # 6. Execute request and handle potential fatal errors without breaking the loop
  cat("Sending to Gemini API...\n")

  tryCatch({
    resp <- req_perform(req)
    resp_json <- resp_body_json(resp)

    # Extract results
    ai_response <- resp_json$candidates[[1]]$content$parts[[1]]$text
    ranked_filenames <- fromJSON(ai_response)

    cat("Successfully ranked", length(ranked_filenames), "images. Renaming files...\n")

    # 7. Rename the files
    for (i in seq_along(ranked_filenames)) {
      original_name <- ranked_filenames[i]
      old_path <- file.path(site_folder, original_name)

      if (file.exists(old_path)) {
        ext <- toupper(tools::file_ext(original_name))
        new_name <- paste0(i, "_", site_id, ".", ext)
        new_path <- file.path(site_folder, new_name)

        file.rename(from = old_path, to = new_path)
      } else {
        warning(paste("File not found, skipping:", original_name))
      }
    }

    cat("Finished Site ID:", site_id, "\n")

  }, error = function(e) {
    cat(sprintf("\n[FATAL ERROR] Could not process folder %s. Skipping to next. Error details: %s\n", site_id, e$message))
  })

  # 8. Sleep to respect the Requests Per Minute (RPM) limits (unless it's the last folder)
  if (folder_index < length(site_folders)) {
    cat("Sleeping for 6 seconds to respect API rate limits...\n")
    Sys.sleep(6)
  }
}

cat("\n=== BATCH PROCESSING COMPLETE ===\n")
