# Select annotations for manual quality rating.
#
# Usage:
#   Rscript scripts/05_select_rating_tags.R <selected.rds> <output.csv>
#
# The output CSV contains the historical manual-rating selection. Known quirks
# in that selection are intentionally preserved to document the released data.

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
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
  "Rscript scripts/05_select_rating_tags.R <selected.rds> <output.csv>"
)

validate_wildtrax_credentials()
wildrtrax::wt_auth()

selected <- read_named_bundle(arguments[[1]], c("recordings", "tags"))
recordings <- selected$recordings
tags <- selected$tags

assert_columns(
  recordings,
  c("recording_id", "latitude", "longitude", "bcr", "rated"),
  "selected recordings"
)
assert_columns(
  tags,
  c("tag_id", "recording_id", "species_code", "tag_rating", "tag_is_verified"),
  "selected tags"
)

rating_goal <- 20L

quotas <- tags |>
  dplyr::count(.data$species_code, name = "total") |>
  dplyr::left_join(
    tags |>
      dplyr::filter(!is.na(.data$tag_rating)) |>
      dplyr::count(.data$species_code, name = "rated"),
    by = "species_code",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    rated = dplyr::coalesce(.data$rated, 0L),
    available = .data$total - .data$rated,
    needed = dplyr::if_else(
      .data$rated > rating_goal,
      0L,
      rating_goal - .data$rated
    ),
    needed = pmin(.data$needed, .data$available)
  )

# This condition reproduces the historical sampler. It is reviewed separately
# because it omits species needing exactly one additional rating.
todo <- dplyr::filter(quotas, .data$needed > 1)

selected_by_species <- vector("list", nrow(todo))
for (index in seq_len(nrow(todo))) {
  species <- todo$species_code[[index]]
  needed <- todo$needed[[index]]

  available_tags <- tags |>
    dplyr::filter(
      .data$species_code == species,
      is.na(.data$tag_rating)
    )
  verified_tags <- dplyr::filter(
    available_tags,
    .data$tag_is_verified %in% TRUE
  )

  if (nrow(verified_tags) > 0) {
    additional_needed <- needed - nrow(verified_tags)

    if (additional_needed < 1) {
      selected_by_species[[index]] <- dplyr::slice_sample(
        verified_tags,
        n = needed,
        replace = FALSE
      )
    } else {
      set.seed(index)
      selected_by_species[[index]] <- available_tags |>
        dplyr::slice_sample(n = additional_needed, replace = FALSE) |>
        dplyr::bind_rows(verified_tags)
    }
  } else {
    set.seed(index)
    selected_by_species[[index]] <- dplyr::slice_sample(
      available_tags,
      n = needed,
      replace = FALSE
    )
  }
}

tags_selected <- dplyr::bind_rows(selected_by_species)

prairie_rated_points <- recordings |>
  dplyr::filter(.data$rated == 1, .data$longitude < -80) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

if (nrow(prairie_rated_points) > 0) {
  exclusion_area <- prairie_rated_points |>
    sf::st_union() |>
    sf::st_concave_hull(ratio = 1) |>
    sf::st_sf()

  recordings_outside_exclusion <- recordings |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_filter(exclusion_area, .predicate = sf::st_disjoint) |>
    sf::st_drop_geometry()
} else {
  warning(
    "No previously rated western recordings were found; the extra-rating sample is empty.",
    call. = FALSE
  )
  recordings_outside_exclusion <- recordings[0, ]
}

extra_goal <- 5L
extra_quotas <- quotas |>
  dplyr::filter(.data$needed == 0, .data$total > .data$rated) |>
  dplyr::mutate(needed = pmin(.data$available, extra_goal))

extra_by_species <- vector("list", nrow(extra_quotas))
for (index in seq_len(nrow(extra_quotas))) {
  species <- extra_quotas$species_code[[index]]
  needed <- extra_quotas$needed[[index]]

  available_tags <- tags |>
    dplyr::filter(
      .data$species_code == species,
      is.na(.data$tag_rating),
      .data$recording_id %in% recordings_outside_exclusion$recording_id
    )

  if (nrow(available_tags) < needed) {
    needed <- nrow(available_tags)
  }
  if (needed == 0) {
    next
  }

  verified_tags <- dplyr::filter(
    available_tags,
    .data$tag_is_verified %in% TRUE
  )

  if (nrow(verified_tags) > 0) {
    additional_needed <- needed - nrow(verified_tags)

    if (additional_needed < 1) {
      extra_by_species[[index]] <- dplyr::slice_sample(
        verified_tags,
        n = needed,
        replace = FALSE
      )
    } else {
      set.seed(index)
      extra_by_species[[index]] <- available_tags |>
        dplyr::slice_sample(n = additional_needed, replace = FALSE) |>
        dplyr::bind_rows(verified_tags)
    }
  } else {
    set.seed(index)
    extra_by_species[[index]] <- dplyr::slice_sample(
      available_tags,
      n = needed,
      replace = FALSE
    )
  }
}

tags_extra <- dplyr::bind_rows(extra_by_species)
wildtrax_species <- wildrtrax::wt_get_species()

rating_tags <- dplyr::bind_rows(tags_selected, tags_extra) |>
  dplyr::left_join(
    recordings |>
      dplyr::select("recording_id", "latitude", "longitude", "bcr") |>
      dplyr::distinct(.data$recording_id, .keep_all = TRUE),
    by = "recording_id",
    relationship = "many-to-one"
  ) |>
  dplyr::inner_join(
    wildtrax_species |>
      dplyr::filter(
        stringr::str_sub(.data$species_common_name, 1, 12) != "Unidentified",
        stringr::str_length(.data$species_code) == 4,
        .data$species_class %in% c("AVES", "AMPHIBIA", "MAMMALIA")
      ) |>
      dplyr::select("species_code", "species_class"),
    by = "species_code",
    relationship = "many-to-one"
  )

write_csv(rating_tags, arguments[[2]])
