# This script refreshes WildTrax tag reports, obtains clip and spectrogram URLs,
# downloads media for verification and rating, and writes the tracking sheets
# used by reviewers.
#
# Run from the repository root with:
# Rscript scripts/05_prepare_manual_review.R verification.csv rating.csv \
#   filtered_main.rds private_output_directory

library(wildrtrax)
library(dplyr)
library(stringr)
library(purrr)
library(curl)

args <- commandArgs(trailingOnly = TRUE)

# Login ----
# Credentials are set outside this repository.
wt_auth()

################################################################################
# 1. Get the lists of tags for verification and rating ----
verif <- read.csv(args[1])
rate <- read.csv(args[2])

################################################################################
# 2. Get the combined dataset ----
filter_df <- readRDS(args[3])
root <- args[4]

################################################################################
# 3. Get the list of projects we need ----
projects <- data.frame(
  task_id = c(unique(verif$task_id), unique(rate$task_id))
) %>%
  left_join(
    filter_df %>%
      dplyr::select(organization, task_id, project_id) %>%
      unique()
  )

project_ids <- unique(projects$project_id)

################################################################################
# 4. Download the tag reports ----
safe_wt <- safely(wt_download_report, otherwise = NULL)

n <- length(project_ids)

dat.list <- map(seq_along(project_ids), function(i) {
  pid <- as.numeric(project_ids[i])

  res <- safe_wt(
    project_id = pid,
    sensor_id = "ARU",
    reports = "tag"
  )

  if (is.null(res$error)) {
    message("Project ", pid, " completed (", i, "/", n, ")")
  } else {
    message(
      "Project ", pid, " failed: ", res$error$message,
      " (", i, "/", n, ")"
    )
  }

  res
})

# Name each list element by project_id.
names(dat.list) <- project_ids

# Extract results and keep successful, non-empty reports.
results <- map(dat.list, "result")
results_success <- compact(results)
results_nonempty <- keep(results_success, ~ nrow(.x) > 0)

# Combine successful tag reports into one dataframe and save a copy beside the
# private review media.
combined_df_tag <- bind_rows(results_nonempty, .id = "project_id")

saveRDS(
  combined_df_tag,
  file.path(root, "combined_wt_download_tags.rds")
)

################################################################################
# 5. Get the URLs we need ----
verif_tags <- verif %>%
  left_join(
    combined_df_tag %>%
      dplyr::select(project, tag_id, media_url, clip_url, spectrogram_url),
    by = "tag_id"
  ) %>%
  mutate(folder = "Verification") %>%
  dplyr::select(
    species_code, organization, project, project_id, recording_date_time,
    tag_id, task_id, media_url, clip_url, spectrogram_url, folder
  )

rate_tags <- rate %>%
  left_join(
    combined_df_tag %>%
      dplyr::select(
        organization, project, project_id, recording_date_time, tag_id,
        media_url, clip_url, spectrogram_url
      ),
    by = "tag_id"
  ) %>%
  mutate(folder = "Rating") %>%
  dplyr::select(
    species_code, organization, project, project_id, recording_date_time,
    tag_id, task_id, media_url, clip_url, spectrogram_url, folder
  )

tags <- rbind(verif_tags, rate_tags)

# Save dataframe to disk.
saveRDS(tags, file.path(root, "tags_for_download.rds"))

## 5.1 Check for missing tags ----
# This check is retained from the historical script. Because verif_ids and
# rate_ids come from the joined tables, it does not identify failed URL joins;
# it documents the check that was actually performed.
verif_ids <- unique(verif_tags$tag_id)

verif_missing <- verif %>%
  filter(!tag_id %in% verif_ids) %>%
  select(task_id, tag_id, species_code)

rate_ids <- unique(rate_tags$tag_id)

rate_missing <- rate %>%
  filter(!tag_id %in% rate_ids) %>%
  select(task_id, tag_id, species_code)

tags_missing <- rbind(verif_missing, rate_missing)

saveRDS(tags_missing, file.path(root, "tags_missing.rds"))

################################################################################
# 6. Download the tag clips ----

# Create subfolders for clips.
folders_clips <- unique(
  file.path(root, tags$folder, "Clips", tags$species_code)
)
sapply(folders_clips, dir.create, recursive = TRUE, showWarnings = FALSE)

# Create subfolders for spectrograms.
folders_specs <- unique(
  file.path(root, tags$folder, "Specs", tags$species_code)
)
sapply(folders_specs, dir.create, recursive = TRUE, showWarnings = FALSE)

# Download files iteratively.
for (i in 1:nrow(tags)) {
  try(
    curl_download(
      tags$clip_url[i],
      destfile = file.path(
        root, tags$folder[i], "Clips", tags$species_code[i],
        paste0(tags$species_code[i], "_", tags$tag_id[i], ".flac")
      )
    )
  )

  try(
    curl_download(
      tags$spectrogram_url[i],
      destfile = file.path(
        root, tags$folder[i], "Specs", tags$species_code[i],
        paste0(tags$species_code[i], "_", tags$tag_id[i], ".jpg")
      )
    )
  )

  cat(i, " ")
}

################################################################################
# 7. Write tracking CSVs ----
ratings_csv <- tags %>%
  mutate(media_url = paste0(media_url, "/tag/", tag_id)) %>%
  mutate(rating = "") %>%
  filter(folder == "Rating") %>%
  select(species_code, tag_id, media_url, rating)

write.csv(
  ratings_csv,
  file.path(root, "Rating", "ratings.csv"),
  row.names = FALSE
)

verification_csv <- tags %>%
  mutate(media_url = paste0(media_url, "/tag/", tag_id)) %>%
  mutate(verification = "") %>%
  filter(folder == "Verification") %>%
  select(species_code, tag_id, media_url, verification)

write.csv(
  verification_csv,
  file.path(root, "Verification", "verifications.csv"),
  row.names = FALSE
)

