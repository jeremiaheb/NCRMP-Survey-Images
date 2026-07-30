# ==============================================================================
# Google Drive Bulk Downloader (Recursive)
# ==============================================================================
# This script downloads all files AND sub-folders from a specific Google Drive link.
# It requires the 'googledrive', 'fs', and 'rstudioapi' packages.

library(googledrive)
library(fs)
library(rstudioapi)

# 1. Authenticate with Google Drive
cat("Authenticating with Google Drive...\n")

# FIX: Clear any old/stale authentication tokens that might be causing the 403 error
drive_deauth()

# FIX: Force a fresh browser login and explicitly request the "Full Drive" scope
# Setting email = NA forces it to ignore cached emails and open the browser
drive_auth(scopes = "https://www.googleapis.com/auth/drive", email = NA)

# 2. Get the target Google Drive Folder URL from the user
cat("Waiting for you to provide the Google Drive folder URL...\n")
parent_url <- showPrompt(
  title = "Target Google Drive Folder",
  message = "Paste the URL of the Google Drive folder you want to completely download:",
  default = ""
)

if (is.null(parent_url) || parent_url == "") stop("No URL provided. Script cancelled.")

# 3. Select the local destination folder
cat("Waiting for you to select a local destination folder...\n")
local_dest <- selectDirectory(caption = "Select Folder to Save Downloads")
if (is.null(local_dest)) stop("Local destination selection cancelled.")

cat("========================================\n")
cat("Starting download process...\n")
cat("========================================\n")

# 4. Define the recursive download function
# This function calls itself anytime it finds a folder instead of a file!
download_folder_recursively <- function(drive_folder_id, current_local_path) {

  # List all contents of the current Drive folder
  contents <- drive_ls(as_id(drive_folder_id))

  if (nrow(contents) == 0) return() # Empty folder

  for (i in 1:nrow(contents)) {
    item <- contents[i, ]
    item_name <- item$name
    item_id <- item$id

    # Check if the item is a folder by looking at its mimeType
    is_folder <- item$drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder"

    if (is_folder) {
      cat(sprintf("📁 Found sub-folder: %s. Diving in...\n", item_name))

      # Create the folder on your local computer
      new_local_path <- path(current_local_path, item_name)
      dir_create(new_local_path)

      # RECURSION: Call this exact same function on the new sub-folder
      download_folder_recursively(item_id, new_local_path)

    } else {
      # It's a file, so we download it
      file_path <- path(current_local_path, item_name)

      # Only download if it doesn't already exist (allows you to resume interrupted downloads)
      if (!file_exists(file_path)) {
        cat(sprintf("  -> Downloading file: %s\n", item_name))
        drive_download(as_id(item_id), path = file_path, overwrite = FALSE)
      } else {
        cat(sprintf("  -> Skipping %s (already exists)\n", item_name))
      }
    }
  }
}

# 5. Kick off the recursive download
parent_id <- as_id(parent_url)
download_folder_recursively(parent_id, local_dest)

cat("\n========================================\n")
cat("Finished! All contents have been successfully downloaded to:\n")
cat(local_dest, "\n")
