# Identify bird annotations needing manual verification using eBird and HawkEars.
#
# Usage:
#   Rscript scripts/04_select_verification_tags.R \
#     <selected.rds> <ebird_species.csv> <hawkears_scores.csv> \
#     <ebird_data_directory> <output.csv>
#
# The output CSV contains the annotations selected for manual verification.

suppressPackageStartupMessages({
  library(dplyr)
  library(ebirdst)
  library(lubridate)
  library(readr)
  library(sf)
  library(stringr)
  library(terra)
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
  5,
  paste(
    "Rscript scripts/04_select_verification_tags.R",
    "<selected.rds> <ebird_species.csv> <hawkears_scores.csv>",
    "<ebird_data_directory> <output.csv>"
  )
)

validate_wildtrax_credentials()
wildrtrax::wt_auth()

selected <- read_named_bundle(arguments[[1]], c("recordings", "tags"))
recordings <- selected$recordings
tags <- selected$tags
ebird_species <- readr::read_csv(arguments[[2]], show_col_types = FALSE)
hawkears <- readr::read_csv(arguments[[3]], show_col_types = FALSE)
ebird_data_dir <- normalizePath(arguments[[4]], mustWork = TRUE)

assert_columns(
  recordings,
  c(
    "recording_id", "task_id", "longitude", "latitude", "recording_date_time",
    "source_file_name", "location"
  ),
  "selected recordings"
)
assert_columns(tags, c("tag_id", "recording_id", "task_id", "species_code"), "selected tags")
assert_columns(
  ebird_species,
  c("species", "species_code", "common_name"),
  "eBird species table"
)
assert_columns(hawkears, c("name", "recording", "score"), "HawkEars scores")

tags_clean <- tags |>
  dplyr::mutate(
    species_code = dplyr::recode(
      .data$species_code,
      GRAJ = "CAJA",
      PSFL = "WEFL",
      COFL = "WEFL",
      MYWA = "YRWA",
      COSN = "WISN",
      MEGU = "SBIG",
      NOGO = "AGOS",
      .default = .data$species_code
    )
  ) |>
  dplyr::left_join(
    recordings |>
      dplyr::select(
        "recording_id", "task_id", "longitude", "latitude",
        "recording_date_time", "source_file_name", "location"
      ),
    by = c("recording_id", "task_id"),
    relationship = "many-to-one"
  )

species_lookup <- ebird_species |>
  dplyr::mutate(
    species = dplyr::recode(
      .data$species,
      AGOL = "AMGO",
      AGOS = "NOGO",
      BTYW = "BTGW",
      BADO = "BAOW",
      HOWR = "NHWR",
      CORE = "REDP",
      .default = .data$species
    )
  )

species_to_process <- species_lookup |>
  dplyr::inner_join(
    tibble::tibble(species = unique(tags_clean$species_code)),
    by = "species"
  ) |>
  dplyr::filter(.data$species_code != "yebsap-example")

occurrence_results <- vector("list", nrow(species_to_process))
missed_species <- character()

for (species_index in seq_len(nrow(species_to_process))) {
  tag_species_code <- species_to_process$species[[species_index]]
  ebird_species_code <- species_to_process$species_code[[species_index]]

  occurrence_raster <- try(
    ebirdst::load_raster(
      ebird_species_code,
      product = "occurrence",
      resolution = "9km",
      path = ebird_data_dir
    ),
    silent = TRUE
  )

  if (inherits(occurrence_raster, "try-error")) {
    missed_species <- c(missed_species, tag_species_code)
    next
  }

  raster_weeks <- lubridate::isoweek(lubridate::ymd(names(occurrence_raster)))
  names(occurrence_raster) <- as.character(raster_weeks)

  species_tags <- tags_clean |>
    dplyr::filter(.data$species_code == tag_species_code) |>
    dplyr::mutate(week = lubridate::isoweek(.data$recording_date_time)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(terra::crs(occurrence_raster))

  week_results <- lapply(unique(species_tags$week), function(week) {
    week_name <- as.character(week)
    if (!week_name %in% names(occurrence_raster)) {
      return(NULL)
    }

    week_tags <- dplyr::filter(species_tags, .data$week == week)
    occurrence <- terra::extract(occurrence_raster[[week_name]], week_tags)

    week_tags |>
      sf::st_drop_geometry() |>
      dplyr::mutate(occurrence = occurrence[[2]])
  })

  occurrence_results[[species_index]] <- dplyr::bind_rows(week_results)
  message(
    "Processed ",
    tag_species_code,
    " (",
    species_index,
    "/",
    nrow(species_to_process),
    ")"
  )
}

zero_occurrence <- dplyr::bind_rows(occurrence_results) |>
  dplyr::filter(.data$occurrence == 0) |>
  dplyr::select(-"week")

wildtrax_species <- wildrtrax::wt_get_species()

species_without_products <- species_lookup |>
  dplyr::right_join(
    tibble::tibble(species = unique(tags_clean$species_code)),
    by = "species"
  ) |>
  dplyr::filter(is.na(.data$common_name)) |>
  dplyr::transmute(species_code = .data$species) |>
  dplyr::inner_join(
    dplyr::filter(wildtrax_species, .data$species_class == "AVES"),
    by = "species_code"
  ) |>
  dplyr::distinct(.data$species_code)

missing_product_tags <- tags_clean |>
  dplyr::filter(.data$species_code %in% species_without_products$species_code) |>
  dplyr::mutate(occurrence = NA_real_)

ebird_outliers <- dplyr::bind_rows(zero_occurrence, missing_product_tags) |>
  dplyr::mutate(
    recording = paste(
      .data$location,
      format(as.Date(.data$recording_date_time), "%Y%m%d"),
      format(.data$recording_date_time, "%H%M%S"),
      sep = "_"
    )
  )

hawkears_recording_scores <- hawkears |>
  dplyr::rename(species_code = "name") |>
  dplyr::group_by(.data$species_code, .data$recording) |>
  dplyr::summarise(score = max(.data$score, na.rm = TRUE), .groups = "drop")

recording_extra_columns <- setdiff(names(recordings), names(ebird_outliers))

verification_tags <- ebird_outliers |>
  dplyr::left_join(
    hawkears_recording_scores,
    by = c("species_code", "recording"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(score = dplyr::coalesce(.data$score, 0)) |>
  dplyr::filter(.data$score < 0.2) |>
  dplyr::inner_join(
    wildtrax_species |>
      dplyr::filter(
        .data$species_class == "AVES",
        stringr::str_sub(.data$species_common_name, 1, 12) != "Unidentified"
      ) |>
      dplyr::select("species_code", "species_class"),
    by = "species_code",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    recordings |>
      dplyr::select(
        "recording_id",
        "task_id",
        dplyr::all_of(recording_extra_columns)
      ),
    by = c("recording_id", "task_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::distinct(.data$tag_id, .keep_all = TRUE)

if (length(missed_species) > 0) {
  warning(
    "No local eBird occurrence raster was available for: ",
    paste(unique(missed_species), collapse = ", "),
    call. = FALSE
  )
}

write_csv(verification_tags, arguments[[5]])
