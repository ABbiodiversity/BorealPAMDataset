# General validation and file helpers used by the publication scripts.

require_arguments <- function(arguments, count, usage) {
  if (length(arguments) != count) {
    stop("Usage: ", usage, call. = FALSE)
  }
}

assert_columns <- function(data, columns, object_name = deparse(substitute(data))) {
  missing_columns <- setdiff(columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      object_name,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(data)
}

assert_unique <- function(data, columns, object_name = deparse(substitute(data))) {
  assert_columns(data, columns, object_name)
  duplicated_keys <- duplicated(data[columns])

  if (any(duplicated_keys)) {
    stop(
      object_name,
      " has ",
      sum(duplicated_keys),
      " duplicated row(s) for key: ",
      paste(columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(data)
}

read_named_bundle <- function(path, required_names) {
  value <- readRDS(normalizePath(path, mustWork = TRUE))

  if (!is.list(value) || is.null(names(value))) {
    stop(path, " must contain a named list.", call. = FALSE)
  }

  missing_objects <- setdiff(required_names, names(value))
  if (length(missing_objects) > 0) {
    stop(
      path,
      " is missing required objects: ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }

  value
}

write_rds <- function(value, path) {
  output_path <- normalizePath(path, mustWork = FALSE)
  output_parent <- dirname(output_path)
  dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)
  saveRDS(value, output_path)
  message("Wrote ", output_path)
  invisible(output_path)
}

write_csv <- function(value, path) {
  output_path <- normalizePath(path, mustWork = FALSE)
  output_parent <- dirname(output_path)
  dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(value, output_path)
  message("Wrote ", output_path)
  invisible(output_path)
}
