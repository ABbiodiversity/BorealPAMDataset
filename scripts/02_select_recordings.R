# Spatially thin eligible recordings and retain their annotation tags.
#
# Usage:
#   Rscript scripts/02_select_recordings.R <prepared.rds> <output.rds>
#
# The output RDS is a named list containing the selected recordings and tags.

suppressPackageStartupMessages({
  library(dggridR)
  library(dplyr)
})

arguments <- commandArgs(trailingOnly = TRUE)
command_line <- commandArgs(trailingOnly = FALSE)
script_argument <- grep("^--file=", command_line, value = TRUE)
script_dir <- dirname(normalizePath(
  sub("^--file=", "", script_argument),
  mustWork = TRUE
))
source(file.path(script_dir, "lib", "pipeline_helpers.R"))

require_arguments(
  arguments,
  2,
  "Rscript scripts/02_select_recordings.R <prepared.rds> <output.rds>"
)

prepared <- read_named_bundle(
  arguments[[1]],
  c("recordings", "tags", "recording_report")
)
recordings <- prepared$recordings
tags <- prepared$tags |>
  dplyr::select(
    "recording_id", "task_id", "species_code", "species_common_name",
    "species_scientific_name", "individual_order", "tag_id", "abundance",
    "vocalization", "detection_time", "tag_duration", "rms_peak_dbfs",
    "tag_is_verified", "tag_rating", "observer_id"
  )

assert_unique(recordings, "recording_id", "eligible recordings")

selected_tag_pool <- tags |>
  dplyr::semi_join(
    dplyr::select(recordings, "recording_id", "task_id"),
    by = c("recording_id", "task_id")
  )

verified_tasks <- selected_tag_pool |>
  dplyr::filter(.data$tag_is_verified %in% TRUE) |>
  dplyr::distinct(.data$task_id) |>
  dplyr::pull(.data$task_id)

rated_tasks <- selected_tag_pool |>
  dplyr::filter(!is.na(.data$tag_rating)) |>
  dplyr::distinct(.data$task_id) |>
  dplyr::pull(.data$task_id)

# Preserve the original recording order because the seeded sampling depends on it.
selected_pool <- recordings |>
  dplyr::mutate(
    verified = as.integer(.data$task_id %in% verified_tasks),
    rated = as.integer(.data$task_id %in% rated_tasks)
  )

set.seed(1)
verified_first <- selected_pool |>
  dplyr::filter(.data$verified == 1) |>
  dplyr::group_by(.data$location, .data$time_of_day) |>
  dplyr::sample_n(1, replace = FALSE) |>
  dplyr::ungroup()

set.seed(2)
one_per_location_period <- selected_pool |>
  dplyr::filter(.data$verified == 0) |>
  dplyr::bind_rows(verified_first) |>
  dplyr::group_by(.data$location, .data$time_of_day) |>
  dplyr::sample_n(1, replace = FALSE) |>
  dplyr::ungroup()

grid <- dggridR::dgconstruct(spacing = 2, metric = TRUE)

location_counts <- one_per_location_period |>
  dplyr::count(
    .data$bcr,
    .data$organization,
    .data$project_id,
    .data$location_id,
    .data$longitude,
    .data$latitude,
    name = "recordings"
  ) |>
  dplyr::mutate(
    cell = dggridR::dgGEO_to_SEQNUM(
      grid,
      .data$longitude,
      .data$latitude
    )$seqnum
  )

set.seed(3)
selected_locations <- location_counts |>
  dplyr::group_by(.data$bcr, .data$organization, .data$cell) |>
  dplyr::filter(.data$recordings == max(.data$recordings)) |>
  dplyr::sample_n(1) |>
  dplyr::ungroup()

recordings_selected <- one_per_location_period |>
  dplyr::semi_join(
    selected_locations,
    by = c(
      "bcr", "organization", "project_id", "location_id",
      "longitude", "latitude"
    )
  )

tags_selected <- tags |>
  dplyr::semi_join(
    dplyr::select(recordings_selected, "recording_id", "task_id"),
    by = c("recording_id", "task_id")
  ) |>
  dplyr::mutate(detection_end = .data$detection_time + .data$tag_duration) |>
  dplyr::filter(.data$detection_end < 180) |>
  dplyr::distinct()

recording_urls <- prepared$recording_report |>
  dplyr::select("recording_id", "source_file_name", "recording_url") |>
  dplyr::distinct(.data$recording_id, .keep_all = TRUE)

recordings_selected <- recordings_selected |>
  dplyr::left_join(
    recording_urls,
    by = "recording_id",
    relationship = "one-to-one"
  ) |>
  dplyr::distinct()

assert_unique(recordings_selected, "recording_id", "selected recordings")
assert_unique(tags_selected, "tag_id", "selected tags")

write_rds(
  list(recordings = recordings_selected, tags = tags_selected),
  arguments[[2]]
)
