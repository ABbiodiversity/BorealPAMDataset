# This script refreshes recording URLs from the WildTrax database and downloads
# the recordings in the final dataset. It retains the original database query,
# four-way download split, and result parsing.
#
# The database connection script is private and is sourced from outside this
# repository; it must create an object named `con`.
#
# Run from the repository root with:
# Rscript scripts/08_download_audio.R final_dataset.Rdata \
#   private_connection_script.R output_directory

library(DBI)
library(RPostgres)
library(dplyr)
library(glue)
library(stringr)
library(wildrtrax)

args <- commandArgs(trailingOnly = TRUE)

# Read in connection settings from the private WildTrax backend functions
# project.
source(args[2])

# Read in output from Script 6.
load(args[1])
output_directory <- args[3]

################################################################################
# 1. Get list of recording IDs ----
recording_ids <- unique(dataset_filter$recording_id)

################################################################################
# 2. Generate new recording URLs from the recording IDs ----
recordings_download <- dbGetQuery(
  con,
  paste0(
    "
    SELECT
        a.*,
        l.location_name,
        l.location_organization_id AS organization_id,
        aru.get_recording_url(
            storage_id,
            storage_aru_name,
            storage_is_aws,
            recording_id,
            recording_uuid,
            recording_file_type
        ) AS url
    FROM aru.recording a
    JOIN common.lu_storage
        ON a.recording_storage_id = storage_id
    JOIN common.location l
        ON a.recording_location_id = l.location_id
    WHERE a.recording_id IN (",
    paste(recording_ids, collapse = ", "),
    ")
    "
  )
) %>%
  mutate(recording_date = format(recording_date, "%Y-%m-%d %H:%M:%S")) %>%
  rename(recording_url = url) %>%
  rename(location = location_name) %>%
  mutate(
    recording_date_time = as.POSIXct(
      recording_date,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  ) %>%
  mutate(location = str_replace_all(location, ":", "."))

################################################################################
# 3. Divide download list into chunks ----
# Number of download processes.
n_chunks <- 4

# Create grouping index.
groups <- cut(
  seq_len(nrow(recordings_download)),
  breaks = n_chunks,
  labels = FALSE
)

# Split into list of dataframes.
recording_list <- split(recordings_download, groups)

# Access each subset like this.
recordings1 <- recording_list[[1]]
recordings2 <- recording_list[[2]]
recordings3 <- recording_list[[3]]
recordings4 <- recording_list[[4]]

# Credentials are set outside this repository.
wt_auth()

################################################################################
# 4. Download recordings and capture success/failure ----
result_1 <- wt_download_media(
  recordings1,
  output = file.path(output_directory, "raw", "subset1"),
  type = "recording"
)
result_2 <- wt_download_media(
  recordings2,
  output = file.path(output_directory, "raw", "subset2"),
  type = "recording"
)
result_3 <- wt_download_media(
  recordings3,
  output = file.path(output_directory, "raw", "subset3"),
  type = "recording"
)
result_4 <- wt_download_media(
  recordings4,
  output = file.path(output_directory, "raw", "subset4"),
  type = "recording"
)

################################################################################
# 5. Combine results and identify failures ----
all_results <- c(result_1, result_2, result_3, result_4)

result_df <- tibble(raw = all_results) %>%
  mutate(
    success = str_detect(raw, "^Downloaded to"),
    file_path = if_else(
      success,
      str_remove(raw, "^Downloaded to "),
      NA_character_
    ),
    recording_id = case_when(
      success ~ str_remove(basename(file_path), "\\.flac$"),
      str_detect(raw, "recording_id") ~
        str_replace(raw, ".*recording_id\\s*", ""),
      TRUE ~ NA_character_
    )
  )

failure_df <- result_df %>%
  filter(!success)

write.csv(
  result_df,
  file.path(output_directory, "audio_download_log.csv"),
  row.names = FALSE
)
write.csv(
  failure_df,
  file.path(output_directory, "audio_download_failures.csv"),
  row.names = FALSE
)

