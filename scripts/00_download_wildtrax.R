# This script downloads the WildTrax reports used to construct the Boreal PAM
# dataset. It consolidates the former general and CWS-PRA download scripts.
#
# Run from the repository root with:
# Rscript scripts/00_download_wildtrax.R project_selection.csv output_directory
#
# The project-selection CSV is maintained outside this repository and contains
# organization_name, project_id, and use_in_paper. Downloaded reports and logs
# are also written outside the repository.

library(wildrtrax)
library(dplyr)
library(stringr)
library(purrr)

args <- commandArgs(trailingOnly = TRUE)
project_selection_file <- args[1]
output_directory <- args[2]

# Get projects list from partner organizations ----
projects_selected <- read.csv(project_selection_file) %>%
  filter(use_in_paper == "Y")

# Login to WT ----
# WT_USERNAME and WT_PASSWORD are set outside this repository.
wt_auth()

# Get all ARU projects in WildTrax ----
projects_all <- wt_get_projects("ARU")

# Get all projects identified for use by the partner organizations.
# Keeping the organization in the join prevents project IDs from one
# organization being applied to another organization.
projects_use <- projects_all %>%
  inner_join(
    projects_selected %>%
      select(organization_name, project_id),
    by = c("organization_name", "project_id")
  )

################################################################################
# Pull each report for the projects defined in projects_use ----

download_report <- function(report_name) {
  # Create function to pull reports without stopping the complete download when
  # a single project fails.
  safe_wt <- safely(wt_download_report, otherwise = NULL)

  project_ids <- projects_use$project_id
  n <- length(project_ids)

  # Run wt_download_report iteratively and retain the result and error for each
  # project.
  dat.list <- map(seq_along(project_ids), function(i) {
    pid <- project_ids[i]

    res <- safe_wt(
      project_id = pid,
      sensor_id = "ARU",
      reports = report_name
    )

    if (is.null(res$error)) {
      message("Project ", pid, " completed (", i, "/", n, ")")
    } else {
      message(
        "Project ", pid, " failed: ", res$error$message,
        " (", i, "/", n, ")"
      )
    }

    res
  })

  # Name each list element by project_id.
  names(dat.list) <- project_ids

  # Extract successful results, remove empty reports, and combine them into one
  # dataframe. Some older WildTrax tag downloads returned a list containing all
  # reports; in that case the tag report was the sixth element.
  results <- map(dat.list, "result")
  results_success <- compact(results)
  good_items <- keep(results_success, ~ is.data.frame(.x) && nrow(.x) > 0)
  bad_items <- discard(results_success, ~ is.data.frame(.x) && nrow(.x) > 0)
  bad_lists <- keep(bad_items, ~ is.list(.x) && !is.data.frame(.x))

  if (report_name == "tag" && length(bad_lists) > 0) {
    broken_report <- map(bad_lists, ~ .x[[6]])
    good_items <- c(good_items, broken_report)
  }

  combined_df <- bind_rows(good_items, .id = "project_id")

  # Keep a simple record of failed downloads for review.
  download_log <- data.frame(
    project_id = project_ids,
    report = report_name,
    success = map_lgl(dat.list, ~ is.null(.x$error)),
    error = map_chr(
      dat.list,
      ~ if (is.null(.x$error)) NA_character_ else .x$error$message
    )
  )

  list(data = combined_df, log = download_log)
}

################################################################################
# Pull the main reports ----
main_download <- download_report("main")
combined_df <- main_download$data

saveRDS(
  combined_df,
  file.path(output_directory, "combined_wt_download_main.rds")
)

# Filter out recordings shorter than 178 seconds and methods not used in the
# dataset, then save the dataframe.
filter_df <- combined_df %>%
  filter(task_duration > 178) %>%
  filter(task_method %in% c("1SPT", "1SPM"))

filter_df_test <- filter_df %>%
  filter(is_complete = TRUE)

saveRDS(
  filter_df,
  file.path(output_directory, "filtered_wt_download_main.rds")
)
write.csv(
  main_download$log,
  file.path(output_directory, "main_download_log.csv"),
  row.names = FALSE
)

################################################################################
# Pull the recording reports ----
recording_download <- download_report("recording")
combined_df_rec <- recording_download$data

saveRDS(
  combined_df_rec,
  file.path(output_directory, "combined_wt_download_recording.rds")
)

# Filter out recordings shorter than 178 seconds and methods not used in the
# dataset. The named TRUE expression below is retained from the historical code;
# it did not remove incomplete tasks from the released dataset.
filter_df_rec <- combined_df_rec %>%
  filter(task_duration > 178) %>%
  filter(task_method %in% c("1SPT", "1SPM")) %>%
  filter(is_complete = TRUE)

saveRDS(
  filter_df_rec,
  file.path(output_directory, "filtered_wt_download_recording.rds")
)
write.csv(
  recording_download$log,
  file.path(output_directory, "recording_download_log.csv"),
  row.names = FALSE
)

################################################################################
# Pull the tag reports ----
tag_download <- download_report("tag")
combined_df_tag <- tag_download$data

saveRDS(
  combined_df_tag,
  file.path(output_directory, "combined_wt_download_tag.rds")
)

###### Check before running!
# Filter out recordings shorter than 178 seconds and methods not used in the
# dataset, then save the dataframe.
filter_df_tag <- combined_df_tag %>%
  filter(task_duration > 178) %>%
  filter(task_method %in% c("1SPT", "1SPM")) %>%
  filter(is_complete = TRUE)

saveRDS(
  filter_df_tag,
  file.path(output_directory, "filtered_wt_download_tag.rds")
)
write.csv(
  tag_download$log,
  file.path(output_directory, "tag_download_log.csv"),
  row.names = FALSE
)

