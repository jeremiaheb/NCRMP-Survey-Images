library(rstudioapi)
library(stringr)
library(tools)

# 1. Select the Parent Directory
cat("Please select the parent directory (the one containing the Person/Date folders)...\n")
parent_dir <- rstudioapi::selectDirectory(caption = "Select Parent Directory")

if (is.null(parent_dir) || parent_dir == "") stop("Directory selection cancelled.")

cat("\n--- PHASE 1: MOVING NESTED SITE DIRECTORIES ---\n")
all_dirs <- list.dirs(parent_dir, full.names = TRUE, recursive = TRUE)
site_dirs_to_move <- c()

for (d in all_dirs) {
  if (d == parent_dir || dirname(d) == parent_dir) next

  sub_dirs <- list.dirs(d, recursive = FALSE)
  starts_with_number <- grepl("^\\d+", basename(d))

  if (length(sub_dirs) == 0 && starts_with_number) {
    site_dirs_to_move <- c(site_dirs_to_move, d)
  }
}

for (old_path in site_dirs_to_move) {
  folder_name <- basename(old_path)
  new_path <- file.path(parent_dir, folder_name)

  # Prevent overwriting if two divers had identically named folders
  if (dir.exists(new_path)) {
    unique_suffix <- format(Sys.time(), "%H%M%S")
    new_path <- file.path(parent_dir, paste0(folder_name, "_dup_", sample(100:999, 1)))
  }

  if (file.rename(from = old_path, to = new_path)) {
    cat(sprintf("[MOVED] '%s' -> Parent Root\n", folder_name))
  }
}

cat("\n--- PHASE 2: PRUNING EMPTY DIRECTORIES ---\n")
# Re-list directories, sort by depth so we delete the deepest nested folders first
current_dirs <- list.dirs(parent_dir, full.names = TRUE, recursive = TRUE)
current_dirs <- current_dirs[current_dirs != parent_dir]
depths <- str_count(current_dirs, "/")
current_dirs <- current_dirs[order(depths, decreasing = TRUE)]

pruned_count <- 0
for (d in current_dirs) {
  if (dir.exists(d)) {
    contents <- list.files(d, all.files = TRUE, no.. = TRUE, include.dirs = TRUE)
    if (length(contents) == 0) {
      unlink(d, recursive = TRUE)
      pruned_count <- pruned_count + 1
      cat("[DELETED] Empty folder:", basename(d), "\n")
    }
  }
}
if (pruned_count == 0) cat("No empty folders to prune.\n")

cat("\n--- PHASE 3: RENAMING AND MERGING ---\n")
root_dirs <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)
site_dirs <- root_dirs[grepl("^\\d+", basename(root_dirs))]

for (old_path in site_dirs) {
  folder_name <- basename(old_path)

  # Extract leading digits and truncate to exactly 4 (ignoring the 5th digit/SSU)
  leading_digits <- str_extract(folder_name, "^\\d+")
  site_num <- substr(leading_digits, 1, 4)

  # Extract only the letters to determine the suffix
  letters_only <- toupper(gsub("[^A-Za-z]", "", folder_name))

  # Apply renaming logic
  # If it contains X, J, or BENTHIC -> _benthic
  # If it contains A, B, FISH, or has NO letters at all -> _fish
  if (grepl("X|J|BENTHIC", letters_only)) {
    new_name <- paste0(site_num, "_benthic")
  } else {
    new_name <- paste0(site_num, "_fish")
  }

  new_path <- file.path(parent_dir, new_name)

  # If the folder doesn't already have its perfect name
  if (old_path != new_path) {
    if (!dir.exists(new_path)) {
      # Standard rename if destination doesn't exist
      file.rename(old_path, new_path)
      cat(sprintf("[RENAMED] '%s' -> '%s'\n", folder_name, new_name))
    } else {
      # MERGE: Destination already exists (e.g., matching A and B folders)
      cat(sprintf("[MERGING] Moving images from '%s' into existing '%s'\n", folder_name, new_name))
      imgs <- list.files(old_path, full.names = TRUE)

      for (img in imgs) {
        img_name <- basename(img)
        dest_img <- file.path(new_path, img_name)

        # Failsafe if two photos happen to have the exact same filename (like "1.jpg")
        if (file.exists(dest_img)) {
          rand_tag <- sample(1000:9999, 1)
          img_name <- paste0(file_path_sans_ext(img_name), "_", rand_tag, ".", file_ext(img_name))
          dest_img <- file.path(new_path, img_name)
        }
        file.rename(img, dest_img)
      }
      # Delete the now-empty old directory
      unlink(old_path, recursive = TRUE)
    }
  }
}

cat("\n=======================================================\n")
cat("Complete! All sites organized, renamed, and merged.\n")
