# Load required libraries
library(httr2)
library(magick)
library(base64enc)
library(jsonlite)
library(gargle)
library(rstudioapi)

# 1. Authenticate using NOAA Service Account JSON
cat("Please select your NOAA Google Service Account JSON key file...\n")
json_key_path <- rstudioapi::selectFile(
  caption = "Select your NOAA Google Service Account JSON key file",
  filter = "JSON Files (*.json)"
)

if (is.null(json_key_path)) {
  stop("No JSON key file selected.")
}

# Extract Project ID from the JSON key file automatically
json_data <- jsonlite::fromJSON(json_key_path)
project_id <- json_data$project_id

location <- "us"
model_id <- "gemini-3.5-flash"

# --- COST TRACKING VARIABLES ---
# Estimated prices per 1,000,000 tokens (Update based on current Google Cloud pricing)
price_per_1m_input <- 0.075
price_per_1m_output <- 0.30

total_batch_input_tokens <- 0
total_batch_output_tokens <- 0
total_batch_cost <- 0
# -------------------------------

# 2. Select the PARENT directory containing all the site folders
parent_dir <- rstudioapi::selectDirectory(caption = "Select the PARENT folder containing all Site ID folders")

if (is.null(parent_dir) || parent_dir == "") {
  stop("No parent folder selected.")
}

# Initialize CSV Ledger
ledger_file <- file.path(parent_dir, "batch_processing_cost_ledger.csv")
if (!file.exists(ledger_file)) {
  # Write headers if the file is brand new
  write.csv(data.frame(
    Site_ID = character(),
    Input_Tokens = numeric(),
    Output_Tokens = numeric(),
    Estimated_Cost_USD = numeric()
  ), ledger_file, row.names = FALSE)
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

  # Start building the parts list
  parts_list <- list(
    list(
      text = "You are an expert marine biologist assistant. I am providing you with a batch of photos from a single coral reef survey site. Your task is to rank ALL of the provided photos from best to worst based on how well they represent the reef landscape.

      CRITERIA FOR RANKING:
      1. Higher Rank: Clear and sharp visibility of the reef, showing the general landscape/benthic habitat (complex branching corals are a plus).
      2. Lower Rank: Blurry, poor lighting, or obscured views.
      3. Lowest Rank (Penalize): Any photos showing clipboards, datasheets MUST be ranked at the very bottom of the list.

      Please return ONLY a JSON array containing the exact filenames of ALL the photos, ordered from best (first) to worst (last). For example: [\"best_photo.jpg\", \"second_best.jpg\", ..., \"worst_photo_with_clipboard.jpg\"]"
    )
  )

  # Process and compress images
  for (img_path in image_files) {
    img <- image_read(img_path)
    img_resized <- image_scale(img, "800x800")
    img_raw <- image_write(img_resized, format = "jpeg")
    img_b64 <- base64encode(img_raw)

    file_name <- basename(img_path)

    parts_list <- append(parts_list, list(list(text = paste("Filename:", file_name))))

    parts_list <- append(parts_list, list(
      list(
        inlineData = list(
          mimeType = "image/jpeg",
          data = img_b64
        )
      )
    ))
  }

  # 4. Prepare the Enterprise Vertex AI Request
  model_url <- sprintf(
    "https://aiplatform.%s.rep.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent",
    location, project_id, location, model_id
  )

  body <- list(
    contents = list(
      list(
        role = "user",
        parts = parts_list
      )
    ),
    generationConfig = list(
      temperature = 0.2,
      responseMimeType = "application/json"
    )
  )

  credentials <- gargle::credentials_service_account(
    path = json_key_path,
    scopes = "https://www.googleapis.com/auth/cloud-platform"
  )
  token <- credentials$credentials$access_token

  # 5. Build the request with automatic retry logic
  req <- request(model_url) |>
    req_headers(
      "Authorization" = paste("Bearer", token),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~ FALSE) |>
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

  # 6. Execute request
  cat("Sending to Vertex AI API...\n")

  tryCatch({
    resp <- req_perform(req)

    if (resp_status(resp) >= 400) {
      cat(sprintf("\n[API ERROR DETAILED] Google responded with:\n%s\n", resp_body_string(resp)))
      stop(paste("HTTP", resp_status(resp), "Bad Request"))
    }

    resp_json <- resp_body_json(resp)

    # Extract results
    ai_response <- resp_json$candidates[[1]]$content$parts[[1]]$text
    ranked_filenames <- fromJSON(ai_response)

    # --- COST CALCULATOR MODULE ---
    input_tokens <- resp_json$usageMetadata$promptTokenCount
    output_tokens <- resp_json$usageMetadata$candidatesTokenCount

    # Handle NULLs safely
    if (is.null(input_tokens)) input_tokens <- 0
    if (is.null(output_tokens)) output_tokens <- 0

    folder_cost <- (input_tokens / 1000000 * price_per_1m_input) + (output_tokens / 1000000 * price_per_1m_output)

    total_batch_input_tokens <- total_batch_input_tokens + input_tokens
    total_batch_output_tokens <- total_batch_output_tokens + output_tokens
    total_batch_cost <- total_batch_cost + folder_cost

    # Append to CSV Ledger
    ledger_entry <- data.frame(
      Site_ID = site_id,
      Input_Tokens = input_tokens,
      Output_Tokens = output_tokens,
      Estimated_Cost_USD = round(folder_cost, 6)
    )
    write.table(ledger_entry, file = ledger_file, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)

    cat(sprintf("Cost for Site %s: $%0.6f (%d input / %d output tokens)\n", site_id, folder_cost, input_tokens, output_tokens))
    # ------------------------------

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

  # 8. Sleep briefly
  if (folder_index < length(site_folders)) {
    Sys.sleep(1)
  }
}

# --- BATCH SUMMARY REPORT ---
cat("\n=========================================\n")
cat("       BATCH PROCESSING COMPLETE         \n")
cat("=========================================\n")
cat(sprintf("Total Input Tokens Used:  %d\n", total_batch_input_tokens))
cat(sprintf("Total Output Tokens Used: %d\n", total_batch_output_tokens))
cat(sprintf("ESTIMATED TOTAL COST:     $%0.4f USD\n", total_batch_cost))
cat("=========================================\n")
cat("Detailed ledger saved to:", ledger_file, "\n")
