# Shared helpers for downloading WildTrax reports.

required_selection_columns <- c(
  "organization_name",
  "project_id",
  "use_in_paper"
)

is_affirmative <- function(x) {
  toupper(trimws(as.character(x))) %in% c("Y", "YES", "TRUE", "1")
}

validate_project_selection <- function(project_selection) {
  missing_columns <- setdiff(required_selection_columns, names(project_selection))

  if (length(missing_columns) > 0) {
    stop(
      "Project selection is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  selected <- project_selection |>
    dplyr::filter(is_affirmative(.data$use_in_paper)) |>
    dplyr::transmute(
      organization_name = trimws(as.character(.data$organization_name)),
      project_id = as.integer(.data$project_id)
    ) |>
    dplyr::distinct()

  if (nrow(selected) == 0) {
    stop("Project selection contains no included projects.", call. = FALSE)
  }

  if (anyNA(selected$organization_name) || anyNA(selected$project_id)) {
    stop(
      "Included projects must have both organization_name and project_id.",
      call. = FALSE
    )
  }

  selected
}

validate_wildtrax_credentials <- function() {
  required_variables <- c("WT_USERNAME", "WT_PASSWORD")
  missing_variables <- required_variables[Sys.getenv(required_variables) == ""]

  if (length(missing_variables) > 0) {
    stop(
      "Missing WildTrax environment variables: ",
      paste(missing_variables, collapse = ", "),
      ". Set them outside the repository before running this script.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

match_selected_projects <- function(projects_available, projects_selected) {
  matched <- projects_available |>
    dplyr::inner_join(
      projects_selected,
      by = c("organization_name", "project_id")
    )

  missing <- projects_selected |>
    dplyr::anti_join(
      projects_available,
      by = c("organization_name", "project_id")
    )

  if (nrow(missing) > 0) {
    missing_labels <- paste0(
      missing$organization_name,
      "/",
      missing$project_id
    )

    stop(
      "Selected projects were not returned by WildTrax: ",
      paste(missing_labels, collapse = ", "),
      call. = FALSE
    )
  }

  matched |>
    dplyr::arrange(.data$organization_name, .data$project_id)
}

download_report_for_project <- function(project_id, report) {
  tryCatch(
    {
      value <- wildrtrax::wt_download_report(
        project_id = project_id,
        sensor_id = "ARU",
        reports = report
      )

      if (!is.data.frame(value)) {
        stop(
          "WildTrax returned an unexpected ",
          paste(class(value), collapse = "/"),
          " object."
        )
      }

      list(
        data = value,
        status = "success",
        message = NA_character_
      )
    },
    error = function(error) {
      list(
        data = NULL,
        status = "failed",
        message = conditionMessage(error)
      )
    }
  )
}

download_report_set <- function(project_ids, report) {
  project_ids <- unique(as.integer(project_ids))

  if (length(project_ids) == 0 || anyNA(project_ids)) {
    stop("Project IDs must be non-missing integers.", call. = FALSE)
  }

  if (!report %in% c("main", "recording", "tag")) {
    stop("Unknown WildTrax report type: ", report, call. = FALSE)
  }

  results <- purrr::map(
    project_ids,
    function(project_id) {
      message(
        "Downloading ", report, " report for project ", project_id, "..."
      )
      download_report_for_project(project_id, report)
    }
  )

  report_data <- purrr::map2(
    results,
    project_ids,
    function(result, project_id) {
      if (is.null(result$data) || nrow(result$data) == 0) {
        return(NULL)
      }

      dplyr::mutate(
        result$data,
        requested_project_id = project_id,
        .before = 1
      )
    }
  ) |>
    purrr::compact() |>
    dplyr::bind_rows()

  download_log <- tibble::tibble(
    project_id = project_ids,
    report = report,
    status = purrr::map_chr(results, "status"),
    message = purrr::map_chr(results, "message"),
    rows = purrr::map_int(
      results,
      function(result) {
        if (is.null(result$data)) 0L else nrow(result$data)
      }
    )
  )

  list(data = report_data, log = download_log)
}

write_report_outputs <- function(downloads, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  purrr::iwalk(
    downloads,
    function(download, report) {
      saveRDS(
        download$data,
        file.path(output_dir, paste0("wildtrax_", report, ".rds"))
      )

      readr::write_csv(
        download$log,
        file.path(output_dir, paste0("wildtrax_", report, "_download_log.csv"))
      )
    }
  )
}
