# Clean WildTrax reports and identify eligible boreal recordings.
#
# Usage:
#   Rscript scripts/01_prepare_recordings.R \
#     <wildtrax_main.rds> <wildtrax_recording.rds> \
#     <boreal_boundary> <bcr_boundary> <output.rds>
#
# The output RDS is a named list containing recordings, tags, and the filtered
# recording report used by the next stage.

suppressPackageStartupMessages({
  library(dplyr)
  library(hms)
  library(lubridate)
  library(lutz)
  library(purrr)
  library(sf)
  library(stringr)
  library(suncalc)
  library(wildrtrax)
})

arguments <- commandArgs(trailingOnly = TRUE)
command_line <- commandArgs(trailingOnly = FALSE)
script_argument <- grep("^--file=", command_line, value = TRUE)
script_directory <- dirname(normalizePath(
  sub("^--file=", "", script_argument),
  mustWork = TRUE
))
source(file.path(script_directory, "lib", "pipeline_helpers.R"))
source(file.path(script_directory, "lib", "selection_helpers.R"))

require_arguments(
  arguments,
  5,
  paste(
    "Rscript scripts/01_prepare_recordings.R",
    "<wildtrax_main.rds> <wildtrax_recording.rds>",
    "<boreal_boundary> <bcr_boundary> <output.rds>"
  )
)

main_report <- readRDS(normalizePath(arguments[[1]], mustWork = TRUE))
recording_report <- readRDS(normalizePath(arguments[[2]], mustWork = TRUE))
boreal_boundary <- sf::st_read(arguments[[3]], quiet = TRUE)
bcr_boundary <- sf::st_read(arguments[[4]], quiet = TRUE)

assert_columns(
  main_report,
  c(
    "organization", "project_id", "location", "location_id", "longitude",
    "latitude", "recording_id", "recording_date_time", "task_id",
    "task_duration", "task_method", "species_code"
  ),
  "WildTrax main report"
)
assert_columns(
  recording_report,
  c("recording_id", "task_id", "task_noise", "source_file_name", "recording_url"),
  "WildTrax recording report"
)

# These filters reproduce the thresholds used for the submitted dataset.
# Task-completion status is intentionally not filtered: the historical selected
# dataset contains two tasks marked incomplete, and this code documents that
# released dataset rather than defining a revised selection.
eligible_main <- main_report |>
  dplyr::filter(
    .data$task_duration > 178,
    .data$task_method %in% c("1SPT", "1SPM")
  )

eligible_recording_report <- recording_report |>
  dplyr::filter(
    .data$task_duration > 178,
    .data$task_method %in% c("1SPT", "1SPM")
  )

tidy_tags <- eligible_main |>
  wildrtrax::wt_tidy_species(
    remove = c("abiotic", "insect", "human", "unknown")
  ) |>
  wt_replace_tmtt()

recording_noise <- eligible_recording_report |>
  dplyr::select("recording_id", "task_id", "task_noise") |>
  dplyr::distinct()

excluded_project_ids <- c(
  3524, 2813, 2350, 2115, 2423, 2812,
  1650, 1260, 691, 1136, 3236, 41
)

recordings <- tidy_tags |>
  dplyr::left_join(
    recording_noise,
    by = c("recording_id", "task_id"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(
    .data$species_code != "NONE",
    !stringr::str_detect(.data$task_noise, "ARU Malfunction|Extreme|High"),
    !is.na(.data$latitude),
    !is.na(.data$longitude)
  ) |>
  dplyr::mutate(project_id = as.numeric(.data$project_id)) |>
  dplyr::filter(!.data$project_id %in% excluded_project_ids) |>
  dplyr::select(
    "organization", "project_id", "location", "location_id", "longitude",
    "latitude", "recording_id", "recording_date_time", "task_id",
    "task_duration", "task_method", "task_noise"
  ) |>
  dplyr::distinct(.data$recording_id, .keep_all = TRUE) |>
  add_solar_periods() |>
  dplyr::filter(
    !.data$timezone_group %in% c("Asia/Irkutsk", "Asia/Shanghai", "Europe/Paris")
  )

recording_points <- sf::st_as_sf(
  recordings,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) |>
  sf::st_transform(sf::st_crs(boreal_boundary))

recordings_boreal <- sf::st_join(
  recording_points,
  boreal_boundary,
  left = FALSE
)

bcr_for_join <- sf::st_transform(bcr_boundary, sf::st_crs(recordings_boreal))
assert_columns(bcr_for_join, "name_en", "BCR boundary")

recordings_bcr <- sf::st_join(
  recordings_boreal,
  dplyr::select(bcr_for_join, "name_en"),
  left = FALSE
) |>
  sf::st_drop_geometry() |>
  dplyr::rename(bcr = "name_en") |>
  dplyr::distinct(.data$recording_id, .keep_all = TRUE)

tag_counts <- eligible_main |>
  dplyr::filter(.data$species_code != "NONE") |>
  dplyr::count(.data$task_id, name = "tags")

recordings_out <- recordings_bcr |>
  dplyr::inner_join(tag_counts, by = "task_id", relationship = "one-to-one") |>
  dplyr::mutate(doy = lubridate::yday(.data$recording_date_time)) |>
  dplyr::filter(
    .data$doy > 151,
    .data$doy < 186,
    .data$time_of_day != "day",
    !(.data$tags == 1 & .data$time_of_day %in% c("sunrise", "dawn"))
  )

assert_unique(recordings_out, "recording_id", "eligible recordings")

write_rds(
  list(
    recordings = recordings_out,
    # Keep the eligible main report for selection, matching the historical
    # pipeline; the tidied copy above is used only to identify recordings.
    tags = eligible_main,
    recording_report = eligible_recording_report
  ),
  arguments[[5]]
)
