# Match the North American bird checklist to eBird Status and Trends products.
#
# Usage:
#   Rscript scripts/03_prepare_ebird_status.R \
#     <aba_checklist.csv> <ebird_data_directory> <species_output.csv>
#
# The species table is written to species_output.csv. Downloaded eBird rasters
# are written to ebird_data_directory; both paths must be outside this repo.
#
# EBIRDST_KEY must be set outside this repository.

suppressPackageStartupMessages({
  library(dplyr)
  library(ebirdst)
  library(readr)
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
  3,
  paste(
    "Rscript scripts/03_prepare_ebird_status.R",
    "<aba_checklist.csv> <ebird_data_directory> <species_output.csv>"
  )
)

if (Sys.getenv("EBIRDST_KEY") == "") {
  stop(
    "EBIRDST_KEY is not set. Set it outside the repository before running this script.",
    call. = FALSE
  )
}

checklist_path <- normalizePath(arguments[[1]], mustWork = TRUE)
ebird_data_dir <- normalizePath(arguments[[2]], mustWork = FALSE)
dir.create(ebird_data_dir, recursive = TRUE, showWarnings = FALSE)

north_american_birds <- utils::read.csv(
  checklist_path,
  skip = 4,
  check.names = FALSE
)

if (ncol(north_american_birds) != 6) {
  stop(
    "The ABA checklist must contain six columns after the first four lines.",
    call. = FALSE
  )
}

names(north_american_birds) <- c(
  "family", "common_name", "french", "scientific_name", "species", "status"
)

north_american_birds <- north_american_birds |>
  dplyr::select(-"family", -"french") |>
  dplyr::filter(as.character(.data$status) %in% c("1", "2")) |>
  dplyr::mutate(
    common_name = dplyr::case_when(
      .data$common_name == "Black-crowned Night Heron (Night-Heron)" ~
        "Black-crowned Night Heron",
      .data$common_name == "Common Redpoll" ~ "Redpoll",
      .data$common_name == "Herring Gull" ~ "American Herring Gull",
      .data$common_name == "House Wren" ~ "Northern/Southern House Wren",
      TRUE ~ .data$common_name
    ),
    scientific_name = dplyr::case_when(
      .data$scientific_name == "Accipiter cooperii" ~ "Astur cooperii",
      .data$scientific_name == "Larus argentatus" ~ "Larus smithsonianus",
      .data$scientific_name == "Accipiter atricapillus" ~ "Astur atricapillus",
      .data$scientific_name == "Troglodytes aedon" ~
        "Troglodytes aedon/musculus",
      TRUE ~ .data$scientific_name
    )
  )

data("ebirdst_runs", package = "ebirdst")

species_products <- north_american_birds |>
  dplyr::left_join(
    ebirdst_runs |>
      dplyr::select(
        "species_code", "scientific_name", "common_name", "status_version_year"
      ) |>
      dplyr::mutate(available = TRUE),
    by = c("scientific_name", "common_name"),
    relationship = "many-to-one"
  ) |>
  dplyr::arrange(.data$species)

write_csv(species_products, arguments[[3]])

download_species <- species_products |>
  dplyr::filter(.data$available %in% TRUE) |>
  dplyr::distinct(.data$species_code) |>
  dplyr::pull(.data$species_code)

failures <- character()
for (index in seq_along(download_species)) {
  species_code <- download_species[[index]]
  message(
    "Downloading eBird occurrence product for ",
    species_code,
    " (",
    index,
    "/",
    length(download_species),
    ")"
  )

  result <- try(
    ebirdst::ebirdst_download_status(
      species = species_code,
      path = ebird_data_dir,
      download_abundance = FALSE,
      download_occurrence = TRUE,
      download_ranges = FALSE,
      pattern = "_9km_",
      force = FALSE
    ),
    silent = TRUE
  )

  if (inherits(result, "try-error")) {
    failures <- c(failures, species_code)
  }
}

if (length(failures) > 0) {
  warning(
    "Failed to download eBird products for: ",
    paste(failures, collapse = ", "),
    call. = FALSE
  )
}
