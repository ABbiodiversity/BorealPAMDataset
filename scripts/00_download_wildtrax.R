# Download the WildTrax reports used to construct the Boreal PAM dataset.
#
# Usage:
#   Rscript scripts/00_download_wildtrax.R \
#     /path/to/project_selection.csv \
#     /path/to/private/output_directory
#
# The project-selection CSV is kept outside this repository. It must contain:
#   organization_name, project_id, use_in_paper
#
# Output directory contents:
#   wildtrax_{main,recording,tag}.rds
#   wildtrax_{main,recording,tag}_download_log.csv
#
# Authentication is read from WT_USERNAME and WT_PASSWORD environment
# variables. This script deliberately has no default input or output path.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(wildrtrax)
})

arguments <- commandArgs(trailingOnly = TRUE)

if (length(arguments) != 2) {
  stop(
    paste(
      "Usage:",
      "Rscript scripts/00_download_wildtrax.R",
      "<project_selection.csv>",
      "<private_output_directory>"
    ),
    call. = FALSE
  )
}

project_selection_path <- normalizePath(
  arguments[[1]],
  mustWork = TRUE
)
output_dir <- normalizePath(
  arguments[[2]],
  mustWork = FALSE
)

command_line <- commandArgs(trailingOnly = FALSE)
script_argument <- grep("^--file=", command_line, value = TRUE)

if (length(script_argument) != 1) {
  stop("Unable to determine the script directory.", call. = FALSE)
}

script_path <- normalizePath(
  sub("^--file=", "", script_argument),
  mustWork = TRUE
)

source(file.path(dirname(script_path), "lib", "wildtrax_helpers.R"))

validate_wildtrax_credentials()
wildrtrax::wt_auth()

project_selection <- readr::read_csv(
  project_selection_path,
  show_col_types = FALSE
) |>
  validate_project_selection()

projects_available <- wildrtrax::wt_get_projects(sensor = "ARU")

projects_to_download <- match_selected_projects(
  projects_available,
  project_selection
)

report_names <- c("main", "recording", "tag")

downloads <- purrr::map(
  report_names,
  function(report) {
    download_report_set(projects_to_download$project_id, report)
  }
) |>
  rlang::set_names(report_names)

write_report_outputs(downloads, output_dir)

failed_downloads <- purrr::map_dfr(downloads, "log") |>
  dplyr::filter(.data$status == "failed")

if (nrow(failed_downloads) > 0) {
  warning(
    nrow(failed_downloads),
    " report downloads failed. Review the logs in ",
    output_dir,
    ".",
    call. = FALSE
  )
} else {
  message("All requested WildTrax reports downloaded successfully.")
}
