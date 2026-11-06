# Apply manual reviews, partner exclusions, and taxonomic harmonization.
#
# Usage:
#   Rscript scripts/07_finalize_dataset.R \
#     <selected.rds> <ratings.csv> <verifications.csv> \
#     <private_location_exclusions.csv> <output.rds>
#
# The output RDS contains a named list with the final tag-level dataset and the
# WildTrax project table used to create manuscript summaries.

suppressPackageStartupMessages({
  library(dplyr)
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
  5,
  paste(
    "Rscript scripts/07_finalize_dataset.R",
    "<selected.rds> <ratings.csv> <verifications.csv>",
    "<private_location_exclusions.csv> <output.rds>"
  )
)

validate_wildtrax_credentials()
wildrtrax::wt_auth()

selected <- read_named_bundle(arguments[[1]], c("recordings", "tags"))
recordings <- selected$recordings
tags <- selected$tags
ratings <- readr::read_csv(arguments[[2]], show_col_types = FALSE)
verifications <- readr::read_csv(arguments[[3]], show_col_types = FALSE)
location_exclusions <- readr::read_csv(arguments[[4]], show_col_types = FALSE)

assert_columns(recordings, c("recording_id", "task_id", "project_id"), "selected recordings")
assert_columns(tags, c("tag_id", "recording_id", "task_id"), "selected tags")
assert_columns(ratings, c("tag_id", "rating"), "ratings")
assert_columns(verifications, c("tag_id", "verification"), "verifications")
assert_columns(location_exclusions, "location_id", "location exclusions")

assert_unique(recordings, "recording_id", "selected recordings")
assert_unique(tags, "tag_id", "selected tags")
assert_unique(ratings, "tag_id", "ratings")
assert_unique(verifications, "tag_id", "verifications")

recordings <- recordings |>
  dplyr::mutate(
    project_id = dplyr::if_else(.data$project_id == 1174, 4952, .data$project_id)
  )

ratings_clean <- ratings |>
  dplyr::transmute(
    tag_id = .data$tag_id,
    reviewed_rating = dplyr::na_if(trimws(as.character(.data$rating)), "")
  )

verifications_clean <- verifications |>
  dplyr::transmute(
    tag_id = .data$tag_id,
    reviewed_verification = toupper(trimws(as.character(.data$verification)))
  ) |>
  dplyr::mutate(
    reviewed_verification = dplyr::na_if(.data$reviewed_verification, "")
  )

unexpected_verification_values <- setdiff(
  unique(stats::na.omit(verifications_clean$reviewed_verification)),
  c("TRUE", "FALSE", "IGNORE")
)
if (length(unexpected_verification_values) > 0) {
  stop(
    "Unexpected verification values: ",
    paste(unexpected_verification_values, collapse = ", "),
    call. = FALSE
  )
}

reviewed_tags <- tags |>
  dplyr::left_join(ratings_clean, by = "tag_id", relationship = "one-to-one") |>
  dplyr::mutate(
    tag_rating = dplyr::coalesce(
      .data$reviewed_rating,
      as.character(.data$tag_rating)
    )
  ) |>
  dplyr::select(-"reviewed_rating") |>
  dplyr::left_join(
    verifications_clean,
    by = "tag_id",
    relationship = "one-to-one"
  ) |>
  dplyr::filter(
    is.na(.data$reviewed_verification) |
      .data$reviewed_verification != "FALSE"
  ) |>
  dplyr::mutate(
    tag_is_verified = dplyr::if_else(
      .data$reviewed_verification == "TRUE" &
        !is.na(.data$reviewed_verification),
      TRUE,
      .data$tag_is_verified
    )
  ) |>
  dplyr::select(-"reviewed_verification")

dataset <- reviewed_tags |>
  dplyr::inner_join(
    recordings,
    by = c("recording_id", "task_id"),
    relationship = "many-to-one"
  )

dataset <- dataset |>
  dplyr::filter(
    !.data$organization %in% c("CWS-ATL", "BSC-SK"),
    !.data$location_id %in% location_exclusions$location_id,
    .data$project_id != 1099
  )

sensitive_recordings <- dataset |>
  dplyr::filter(
    .data$project_id %in% c(15, 1696),
    .data$species_code %in% c("CANG", "COLO", "CONI", "GGOW", "RNGR", "RUGR")
  ) |>
  dplyr::distinct(.data$recording_id) |>
  dplyr::pull(.data$recording_id)

dataset <- dataset |>
  dplyr::filter(!.data$recording_id %in% sensitive_recordings) |>
  dplyr::mutate(
    species_common_name = dplyr::case_when(
      .data$species_common_name == "Gray Jay" ~ "Canada Jay",
      .data$species_common_name == "Myrtle Warbler" ~ "Yellow-rumped Warbler",
      .data$species_common_name == "Common Snipe" ~ "Wilson's Snipe",
      .data$species_common_name == "Canis Species" ~
        "Unidentified Canis Species",
      .data$species_common_name == "Mourning / MacGillivray's Warbler" ~
        "Unidentified Mourning / MacGillivray's Warbler",
      TRUE ~ .data$species_common_name
    ),
    species_code = dplyr::case_when(
      .data$species_common_name == "Canada Jay" ~ "CAJA",
      .data$species_common_name == "Yellow-rumped Warbler" ~ "YRWA",
      .data$species_common_name == "Wilson's Snipe" ~ "WISN",
      TRUE ~ .data$species_code
    ),
    species_scientific_name = dplyr::case_when(
      .data$species_common_name == "Yellow-rumped Warbler" ~
        "SETOPHAGA CORONATA",
      .data$species_common_name == "Wilson's Snipe" ~ "GALLINAGO DELICATA",
      TRUE ~ .data$species_scientific_name
    )
  )

wildtrax_species <- wildrtrax::wt_get_species()
wildtrax_projects <- wildrtrax::wt_get_projects(sensor = "ARU")

dataset_final <- dataset |>
  dplyr::left_join(
    wildtrax_species |>
      dplyr::select("species_code", "species_class") |>
      dplyr::distinct(.data$species_code, .keep_all = TRUE),
    by = "species_code",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    wildtrax_projects |>
      dplyr::select(
        "project_id", "project", "task_count", "project_status", "tasks_completed"
      ) |>
      dplyr::distinct(.data$project_id, .keep_all = TRUE),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::filter(
    !.data$species_class %in% c("ABIOTIC", "HUMAN ACTIVITY", "INSECTA"),
    .data$species_common_name != "NONE"
  )

assert_unique(dataset_final, "tag_id", "final dataset")

if (anyNA(dataset_final$tag_is_verified)) {
  # Preserve unknown values from the released dataset instead of treating an
  # unreviewed tag as either verified or explicitly unverified.
  warning(
    sum(is.na(dataset_final$tag_is_verified)),
    " final tag(s) have unknown verification status.",
    call. = FALSE
  )
}

write_rds(
  list(dataset = dataset_final, projects = wildtrax_projects),
  arguments[[5]]
)
