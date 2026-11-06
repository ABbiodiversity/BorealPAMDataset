# Refresh tag URLs and prepare clips, spectrograms, and tracking sheets for review.
#
# Usage:
#   Rscript scripts/06_prepare_manual_review.R \
#     <verification_tags.csv> <rating_tags.csv> <wildtrax_main.rds> \
#     <private_output_directory>
#
# The output directory receives private review media, tracking sheets, refreshed
# tag metadata, and download logs. It must be outside this repository.

suppressPackageStartupMessages({
  library(curl)
  library(dplyr)
  library(purrr)
  library(readr)
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
  4,
  paste(
    "Rscript scripts/06_prepare_manual_review.R",
    "<verification_tags.csv> <rating_tags.csv> <wildtrax_main.rds>",
    "<private_output_directory>"
  )
)

validate_wildtrax_credentials()
wildrtrax::wt_auth()

verification <- readr::read_csv(arguments[[1]], show_col_types = FALSE)
rating <- readr::read_csv(arguments[[2]], show_col_types = FALSE)
main_report <- readRDS(normalizePath(arguments[[3]], mustWork = TRUE))
output_dir <- normalizePath(arguments[[4]], mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

assert_columns(
  verification,
  c("tag_id", "task_id", "species_code"),
  "verification tags"
)
assert_columns(rating, c("tag_id", "task_id", "species_code"), "rating tags")
assert_columns(main_report, c("task_id", "project_id"), "WildTrax main report")

review_tasks <- dplyr::bind_rows(
  dplyr::transmute(verification, task_id = .data$task_id),
  dplyr::transmute(rating, task_id = .data$task_id)
) |>
  dplyr::distinct() |>
  dplyr::left_join(
    main_report |>
      dplyr::select("task_id", "project_id") |>
      dplyr::distinct(),
    by = "task_id",
    relationship = "many-to-one"
  )

if (anyNA(review_tasks$project_id)) {
  stop(
    "At least one review task could not be matched to a WildTrax project.",
    call. = FALSE
  )
}

tag_reports <- download_report_set(
  unique(as.integer(review_tasks$project_id)),
  "tag"
)

readr::write_csv(
  tag_reports$log,
  file.path(output_dir, "tag_report_download_log.csv")
)

assert_columns(
  tag_reports$data,
  c("tag_id", "media_url", "clip_url", "spectrogram_url"),
  "downloaded WildTrax tag reports"
)

tag_urls <- tag_reports$data |>
  dplyr::select(
    dplyr::any_of(c("organization", "project", "project_id", "recording_date_time")),
    "tag_id", "media_url", "clip_url", "spectrogram_url"
  ) |>
  dplyr::distinct(.data$tag_id, .keep_all = TRUE)

verification_downloads <- verification |>
  dplyr::left_join(tag_urls, by = "tag_id", relationship = "many-to-one") |>
  dplyr::mutate(review_type = "Verification")

rating_downloads <- rating |>
  dplyr::left_join(tag_urls, by = "tag_id", relationship = "many-to-one") |>
  dplyr::mutate(review_type = "Rating")

review_tags <- dplyr::bind_rows(verification_downloads, rating_downloads)

missing_tags <- review_tags |>
  dplyr::filter(
    is.na(.data$clip_url) |
      is.na(.data$spectrogram_url) |
      .data$clip_url == "" |
      .data$spectrogram_url == ""
  ) |>
  dplyr::select("review_type", "task_id", "tag_id", "species_code")

readr::write_csv(review_tags, file.path(output_dir, "tags_for_review.csv"))
readr::write_csv(missing_tags, file.path(output_dir, "missing_tags.csv"))

download_one <- function(url, destination) {
  if (is.na(url) || url == "") {
    return(list(status = "missing_url", message = NA_character_))
  }

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  tryCatch(
    {
      curl::curl_download(url, destination, quiet = TRUE)
      list(status = "success", message = NA_character_)
    },
    error = function(error) {
      list(status = "failed", message = conditionMessage(error))
    }
  )
}

download_log <- purrr::map_dfr(seq_len(nrow(review_tags)), function(index) {
  tag <- review_tags[index, ]
  clip_path <- file.path(
    output_dir,
    tag$review_type,
    "Clips",
    tag$species_code,
    paste0(tag$species_code, "_", tag$tag_id, ".flac")
  )
  spectrogram_path <- file.path(
    output_dir,
    tag$review_type,
    "Spectrograms",
    tag$species_code,
    paste0(tag$species_code, "_", tag$tag_id, ".jpg")
  )

  clip_result <- download_one(tag$clip_url, clip_path)
  spectrogram_result <- download_one(tag$spectrogram_url, spectrogram_path)

  tibble::tibble(
    review_type = tag$review_type,
    tag_id = tag$tag_id,
    clip_status = clip_result$status,
    clip_message = clip_result$message,
    spectrogram_status = spectrogram_result$status,
    spectrogram_message = spectrogram_result$message
  )
})

readr::write_csv(download_log, file.path(output_dir, "media_download_log.csv"))

rating_sheet <- rating_downloads |>
  dplyr::mutate(
    media_url = paste0(.data$media_url, "/tag/", .data$tag_id),
    rating = NA_character_
  ) |>
  dplyr::select("species_code", "tag_id", "media_url", "rating")

verification_sheet <- verification_downloads |>
  dplyr::mutate(
    media_url = paste0(.data$media_url, "/tag/", .data$tag_id),
    verification = NA_character_
  ) |>
  dplyr::select("species_code", "tag_id", "media_url", "verification")

readr::write_csv(rating_sheet, file.path(output_dir, "ratings.csv"))
readr::write_csv(
  verification_sheet,
  file.path(output_dir, "verifications.csv")
)

message(
  "Prepared ",
  nrow(review_tags),
  " review rows; ",
  nrow(missing_tags),
  " row(s) had missing media URLs."
)
