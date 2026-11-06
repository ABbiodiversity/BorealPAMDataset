# Refresh recording reports and download the audio in the final dataset.
#
# Usage:
#   Rscript scripts/09_download_audio.R <final_dataset.rds> <output_directory>
#
# The output directory receives refreshed recording metadata, download logs,
# missing-URL records, and a raw/ subdirectory containing downloaded audio.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(wildrtrax)
})

arguments <- commandArgs(trailingOnly = TRUE)
command_line <- commandArgs(trailingOnly = FALSE)
script_argument <- grep("^--file=", command_line, value = TRUE)
script_dir <- dirname(normalizePath(
  sub("^--file=", "", script_argument),
  mustWork = TRUE
))
source(file.path(script_dir, "lib", "pipeline_helpers.R"))
source(file.path(script_dir, "lib", "wildtrax_helpers.R"))

require_arguments(
  arguments,
  2,
  "Rscript scripts/09_download_audio.R <final_dataset.rds> <output_directory>"
)

validate_wildtrax_credentials()
wildrtrax::wt_auth()

final_bundle <- read_named_bundle(arguments[[1]], c("dataset"))
dataset <- final_bundle$dataset
output_dir <- normalizePath(arguments[[2]], mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
raw_audio_dir <- file.path(output_dir, "raw")
dir.create(raw_audio_dir, recursive = TRUE, showWarnings = FALSE)

assert_columns(dataset, c("recording_id", "project_id"), "final dataset")

selected_recordings <- dataset |>
  dplyr::select("recording_id", "project_id") |>
  dplyr::distinct()

recording_reports <- download_report_set(
  unique(as.integer(selected_recordings$project_id)),
  "recording"
)

readr::write_csv(
  recording_reports$log,
  file.path(output_dir, "recording_report_download_log.csv")
)

assert_columns(
  recording_reports$data,
  c("recording_id", "recording_url"),
  "downloaded recording reports"
)

recordings_to_download <- recording_reports$data |>
  dplyr::semi_join(selected_recordings, by = "recording_id") |>
  dplyr::distinct(.data$recording_id, .keep_all = TRUE)

missing_recordings <- selected_recordings |>
  dplyr::anti_join(recordings_to_download, by = "recording_id")

readr::write_csv(
  missing_recordings,
  file.path(output_dir, "recordings_missing_current_url.csv")
)

if (nrow(missing_recordings) > 0) {
  warning(
    nrow(missing_recordings),
    " selected recording(s) were absent from the refreshed WildTrax reports.",
    call. = FALSE
  )
}

raw_results <- wildrtrax::wt_download_media(
  recordings_to_download,
  output = raw_audio_dir,
  type = "recording"
)

download_log <- tibble::tibble(raw = as.character(raw_results)) |>
  dplyr::mutate(
    success = stringr::str_detect(.data$raw, "^Downloaded to"),
    file_path = dplyr::if_else(
      .data$success,
      stringr::str_remove(.data$raw, "^Downloaded to "),
      NA_character_
    )
  )

readr::write_csv(download_log, file.path(output_dir, "audio_download_log.csv"))

message(
  "Downloaded ",
  sum(download_log$success),
  " of ",
  nrow(download_log),
  " reported audio files."
)
