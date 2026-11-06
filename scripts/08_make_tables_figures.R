# Produce manuscript tables, counts, and the dataset-location figure.
#
# Usage:
#   Rscript scripts/08_make_tables_figures.R \
#     <final_dataset.rds> <project_metadata.xlsx> <sheet_name> \
#     <boreal_boundary> <output_directory>
#
# The output directory receives dataset totals, organization/project summaries,
# manuscript Tables 1-4, and the dataset-location figure.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(sf)
  library(stringr)
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
  5,
  paste(
    "Rscript scripts/08_make_tables_figures.R",
    "<final_dataset.rds> <project_metadata.xlsx> <sheet_name>",
    "<boreal_boundary> <output_directory>"
  )
)

final_bundle <- read_named_bundle(arguments[[1]], c("dataset", "projects"))
dataset <- final_bundle$dataset
wildtrax_projects <- final_bundle$projects
project_metadata <- readxl::read_excel(arguments[[2]], sheet = arguments[[3]])
boreal_boundary <- sf::st_read(arguments[[4]], quiet = TRUE) |>
  sf::st_transform(4326)
output_dir <- normalizePath(arguments[[5]], mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

assert_columns(
  dataset,
  c(
    "tag_id", "recording_id", "location_id", "project_id", "organization",
    "species_common_name", "species_class", "tag_is_verified", "longitude",
    "latitude", "recording_date_time", "time_of_day", "task_method"
  ),
  "final dataset"
)
assert_columns(
  project_metadata,
  c(
    "project_id_lookup", "organization_name", "project",
    "Partner_organizations_and_groups", "Research_goal_of_project",
    "Deployment_description", "Recording_equipment", "Sampling_rate_Hz",
    "Amphibians_annotated", "Mammals_annotated", "Validation_methods",
    "Validation_in_Wildtrax"
  ),
  "project metadata"
)

species_counts <- dataset |>
  dplyr::filter(
    !stringr::str_detect(
      .data$species_common_name,
      stringr::regex("Unidentified|Domestic", ignore_case = TRUE)
    )
  ) |>
  dplyr::count(.data$species_common_name)

domestic_counts <- dataset |>
  dplyr::filter(stringr::str_detect(
    .data$species_common_name,
    stringr::regex("Domestic", ignore_case = TRUE)
  )) |>
  dplyr::count(.data$species_common_name)

unidentified_counts <- dataset |>
  dplyr::filter(stringr::str_detect(
    .data$species_common_name,
    stringr::regex("Unidentified", ignore_case = TRUE)
  )) |>
  dplyr::count(.data$species_common_name)

totals <- tibble::tibble(
  recordings = dplyr::n_distinct(dataset$recording_id),
  tags = nrow(dataset),
  tags_verified = sum(dataset$tag_is_verified %in% TRUE),
  locations = dplyr::n_distinct(dataset$location_id),
  number_species = nrow(species_counts),
  number_species_domestic = nrow(domestic_counts),
  number_species_groups = nrow(unidentified_counts)
)

organization_counts <- dataset |>
  dplyr::group_by(.data$organization) |>
  dplyr::summarise(
    num_projects_used = dplyr::n_distinct(.data$project_id),
    num_recordings_used = dplyr::n_distinct(.data$recording_id),
    num_tags_used = dplyr::n_distinct(.data$tag_id),
    num_locations_used = dplyr::n_distinct(.data$location_id),
    .groups = "drop"
  )

project_counts <- dataset |>
  dplyr::group_by(.data$organization, .data$project_id) |>
  dplyr::summarise(
    num_recordings_used = dplyr::n_distinct(.data$recording_id),
    num_tags_used = dplyr::n_distinct(.data$tag_id),
    num_locations_used = dplyr::n_distinct(.data$location_id),
    .groups = "drop"
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
  dplyr::rename(num_recordings_project = "task_count")

project_calculated_metadata <- dataset |>
  dplyr::mutate(year = lubridate::year(.data$recording_date_time)) |>
  dplyr::group_by(.data$project_id, .data$year) |>
  dplyr::summarise(
    date_range = paste(
      format(min(.data$recording_date_time, na.rm = TRUE), "%Y-%m-%d"),
      format(max(.data$recording_date_time, na.rm = TRUE), "%Y-%m-%d"),
      sep = " to "
    ),
    time_of_day = paste(sort(unique(stats::na.omit(.data$time_of_day))), collapse = ";"),
    task_method = paste(sort(unique(stats::na.omit(.data$task_method))), collapse = ";"),
    .groups = "drop"
  ) |>
  dplyr::group_by(.data$project_id) |>
  dplyr::summarise(
    date_range = paste(.data$date_range, collapse = ";"),
    time_of_day = paste(
      sort(unique(unlist(strsplit(.data$time_of_day, ";")))),
      collapse = ";"
    ),
    task_method = paste(
      sort(unique(unlist(strsplit(.data$task_method, ";")))),
      collapse = ";"
    ),
    .groups = "drop"
  )

project_all <- project_calculated_metadata |>
  dplyr::left_join(
    project_metadata,
    by = c("project_id" = "project_id_lookup"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    project_counts |>
      dplyr::select(
        "project_id", "project_status", "num_recordings_project",
        "num_recordings_used", "num_tags_used", "num_locations_used"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::rename(project_name = "project") |>
  dplyr::mutate(
    wildtrax_recordings_available = dplyr::if_else(
      .data$project_status == "Published - Public",
      "Yes",
      "No"
    ),
    wildtrax_data_available = dplyr::if_else(
      .data$project_status %in% c(
        "Published - Public",
        "Published - Map+Report Only"
      ),
      "Yes",
      "No"
    )
  )

table_1 <- project_all |>
  dplyr::select(
    "organization_name", "project_name", "project_id",
    "Partner_organizations_and_groups", "Research_goal_of_project",
    "num_recordings_project", "num_recordings_used",
    "wildtrax_recordings_available", "wildtrax_data_available"
  )

table_2 <- project_all |>
  dplyr::select(
    "organization_name", "project_name", "project_id", "Deployment_description",
    "Recording_equipment", "Sampling_rate_Hz", "date_range", "time_of_day"
  )

table_3 <- project_all |>
  dplyr::select(
    "organization_name", "project_name", "project_id", "task_method",
    "Amphibians_annotated", "Mammals_annotated", "Validation_methods",
    "Validation_in_Wildtrax"
  )

tags_by_taxon <- dataset |>
  dplyr::group_by(.data$project_id) |>
  dplyr::summarise(
    AVES_unverified = sum(
      .data$species_class == "AVES" & .data$tag_is_verified == FALSE,
      na.rm = TRUE
    ),
    AVES_verified = sum(
      .data$species_class == "AVES" & .data$tag_is_verified == TRUE,
      na.rm = TRUE
    ),
    MAMMALIA_unverified = sum(
      .data$species_class == "MAMMALIA" & .data$tag_is_verified == FALSE,
      na.rm = TRUE
    ),
    MAMMALIA_verified = sum(
      .data$species_class == "MAMMALIA" & .data$tag_is_verified == TRUE,
      na.rm = TRUE
    ),
    AMPHIBIA_unverified = sum(
      .data$species_class == "AMPHIBIA" & .data$tag_is_verified == FALSE,
      na.rm = TRUE
    ),
    AMPHIBIA_verified = sum(
      .data$species_class == "AMPHIBIA" & .data$tag_is_verified == TRUE,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

table_4 <- project_all |>
  dplyr::left_join(tags_by_taxon, by = "project_id", relationship = "one-to-one") |>
  dplyr::mutate(
    MAMMALIA_verified = dplyr::if_else(
      .data$Mammals_annotated == "N",
      "n/a",
      as.character(.data$MAMMALIA_verified)
    ),
    MAMMALIA_unverified = dplyr::if_else(
      .data$Mammals_annotated == "N",
      "n/a",
      as.character(.data$MAMMALIA_unverified)
    ),
    AMPHIBIA_verified = dplyr::if_else(
      .data$Amphibians_annotated == "N",
      "n/a",
      as.character(.data$AMPHIBIA_verified)
    ),
    AMPHIBIA_unverified = dplyr::if_else(
      .data$Amphibians_annotated == "N",
      "n/a",
      as.character(.data$AMPHIBIA_unverified)
    )
  ) |>
  dplyr::select(
    "organization_name", "project_name", "project_id", "AVES_unverified",
    "AVES_verified", "MAMMALIA_unverified", "MAMMALIA_verified",
    "AMPHIBIA_unverified", "AMPHIBIA_verified"
  )

readr::write_csv(totals, file.path(output_dir, "dataset_totals.csv"))
readr::write_csv(organization_counts, file.path(output_dir, "organization_counts.csv"))
readr::write_csv(project_counts, file.path(output_dir, "project_counts.csv"))
readr::write_csv(table_1, file.path(output_dir, "table_1.csv"))
readr::write_csv(table_2, file.path(output_dir, "table_2.csv"))
readr::write_csv(table_3, file.path(output_dir, "table_3.csv"))
readr::write_csv(table_4, file.path(output_dir, "table_4.csv"))

recording_locations <- dataset |>
  dplyr::distinct(.data$recording_id, .keep_all = TRUE)

location_figure <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = boreal_boundary, fill = "white", colour = "grey60") +
  ggplot2::geom_point(
    data = recording_locations,
    ggplot2::aes(
      x = .data$longitude,
      y = .data$latitude,
      colour = .data$organization
    ),
    size = 0.6,
    alpha = 0.7
  ) +
  ggplot2::coord_sf(crs = 4326) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = NULL, y = NULL, colour = "Organization")

ggplot2::ggsave(
  file.path(output_dir, "dataset_locations.png"),
  location_figure,
  width = 10,
  height = 7,
  dpi = 300
)

message("Wrote manuscript tables and figure to ", output_dir)
