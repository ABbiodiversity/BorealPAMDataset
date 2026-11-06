# Scientific data-preparation helpers for the Boreal PAM dataset.

wt_replace_tmtt <- function(data, calculation = "round") {
  assert_columns(
    data,
    c("recording_date_time", "species_code", "abundance", "observer_id"),
    "WildTrax main report"
  )

  species_codes <- unique(data$species_code)
  if (length(species_codes) == 1 && identical(species_codes, "NONE")) {
    stop("There are no species in this report.", call. = FALSE)
  }

  predictions <- readRDS(system.file(
    "extdata",
    "tmtt_predictions.rds",
    package = "wildrtrax"
  ))

  with_ids <- dplyr::mutate(data, .row_id = dplyr::row_number())
  tmtt_rows <- dplyr::filter(
    with_ids,
    .data$abundance %in% c("TMTT", "TNPE")
  )

  if (nrow(tmtt_rows) > 0) {
    calculate_abundance <- switch(
      calculation,
      round = round,
      ceiling = ceiling,
      floor = floor,
      stop(
        "calculation must be one of: round, ceiling, floor.",
        call. = FALSE
      )
    )

    tmtt_rows <- tmtt_rows |>
      dplyr::rename(.original_species_code = "species_code") |>
      dplyr::mutate(
        species_code = dplyr::if_else(
          .data$.original_species_code %in% predictions$species_code,
          .data$.original_species_code,
          "species"
        ),
        observer_id = dplyr::if_else(
          .data$observer_id %in% predictions$observer_id,
          as.integer(.data$observer_id),
          0L
        )
      ) |>
      dplyr::inner_join(
        dplyr::select(predictions, "species_code", "observer_id", "pred"),
        by = c("species_code", "observer_id"),
        relationship = "many-to-one"
      ) |>
      dplyr::mutate(
        abundance = calculate_abundance(.data$pred),
        species_code = .data$.original_species_code
      ) |>
      dplyr::select(-"pred", -".original_species_code")
  }

  suppressWarnings(
    with_ids |>
      dplyr::mutate(
        abundance = dplyr::if_else(
          .data$abundance %in% c("TMTT", "TNPE"),
          NA_real_,
          as.numeric(.data$abundance)
        )
      ) |>
      dplyr::rows_update(tmtt_rows, by = ".row_id", unmatched = "ignore") |>
      dplyr::select(-".row_id")
  )
}

add_solar_periods <- function(recordings) {
  assert_columns(
    recordings,
    c("latitude", "longitude", "recording_date_time"),
    "recordings"
  )

  with_timezones <- recordings |>
    dplyr::mutate(
      timezone_raw = lutz::tz_lookup_coords(
        lat = .data$latitude,
        lon = .data$longitude,
        method = "accurate"
      ),
      timezone = sub(";.*", "", .data$timezone_raw)
    )

  split(with_timezones, with_timezones$timezone) |>
    purrr::map_dfr(function(timezone_data) {
      timezone_values <- unique(as.character(timezone_data$timezone))
      if (length(timezone_values) != 1 || is.na(timezone_values)) {
        stop("Each timezone group must have exactly one timezone.", call. = FALSE)
      }
      timezone <- timezone_values[[1]]

      localized <- timezone_data |>
        dplyr::mutate(
          recording_date_time_char = as.character(.data$recording_date_time),
          recording_date_time_char = dplyr::if_else(
            nchar(.data$recording_date_time_char) == 10,
            paste0(.data$recording_date_time_char, " 00:00:00"),
            .data$recording_date_time_char
          ),
          recording_date_time_posixct = as.POSIXct(
            .data$recording_date_time_char,
            tz = "UTC"
          ),
          recording_date_time_local = lubridate::force_tz(
            .data$recording_date_time_posixct,
            .env$timezone
          ),
          recording_date = as.Date(
            .data$recording_date_time_local,
            tz = .env$timezone
          )
        )

      sun_times <- suncalc::getSunlightTimes(
        data = data.frame(
          date = localized$recording_date,
          lat = localized$latitude,
          lon = localized$longitude
        ),
        keep = c(
          "dawn",
          "sunrise",
          "goldenHourEnd",
          "goldenHour",
          "sunset",
          "dusk",
          "night",
          "nadir"
        ),
        tz = timezone
      )

      localized |>
        dplyr::bind_cols(dplyr::select(
          sun_times,
          "dawn",
          "sunrise",
          "goldenHourEnd",
          "goldenHour",
          "sunset",
          "dusk",
          "night",
          "nadir"
        )) |>
        dplyr::mutate(
          rec_time_local = hms::as_hms(.data$recording_date_time_local),
          dawn_t = hms::as_hms(.data$dawn),
          sunrise_t = hms::as_hms(.data$sunrise),
          goldenHourEnd_t = hms::as_hms(.data$goldenHourEnd),
          goldenHour_t = hms::as_hms(.data$goldenHour),
          sunset_t = hms::as_hms(.data$sunset),
          dusk_t = hms::as_hms(.data$dusk),
          nadir_t = hms::as_hms(.data$nadir),
          time_of_day = dplyr::case_when(
            !is.na(.data$dawn_t) &
              .data$rec_time_local >= .data$dawn_t &
              .data$rec_time_local <= .data$goldenHourEnd_t ~ "sunrise",
            is.na(.data$dawn_t) &
              !is.na(.data$sunrise_t) &
              .data$rec_time_local >= .data$nadir_t &
              .data$rec_time_local <= .data$goldenHourEnd_t ~ "sunrise",
            is.na(.data$sunrise_t) &
              .data$rec_time_local >= .data$nadir_t &
              .data$rec_time_local <= .data$goldenHourEnd_t ~ "sunrise",
            !is.na(.data$sunrise_t) &
              .data$rec_time_local > .data$goldenHourEnd_t &
              .data$rec_time_local <= (.data$sunrise_t + 5 * 3600) ~ "dawn",
            is.na(.data$dawn_t) &
              is.na(.data$sunrise_t) &
              .data$rec_time_local >= .data$nadir_t &
              .data$rec_time_local <= (.data$nadir_t + 5 * 3600) ~ "dawn",
            !is.na(.data$dusk_t) &
              .data$dusk_t > .data$goldenHour_t &
              .data$rec_time_local >= .data$goldenHour_t &
              .data$rec_time_local <= .data$dusk_t ~ "sunset",
            !is.na(.data$dusk_t) &
              .data$dusk_t < .data$goldenHour_t &
              (.data$rec_time_local >= .data$goldenHour_t |
                .data$rec_time_local <= .data$dusk_t) ~ "sunset",
            is.na(.data$dusk_t) &
              .data$nadir_t > .data$goldenHour_t &
              .data$rec_time_local >= .data$goldenHour_t &
              .data$rec_time_local < .data$nadir_t ~ "sunset",
            is.na(.data$dusk_t) &
              .data$nadir_t < .data$goldenHour_t &
              (.data$rec_time_local >= .data$goldenHour_t |
                .data$rec_time_local < .data$nadir_t) ~ "sunset",
            !is.na(.data$dusk_t) & !is.na(.data$dawn_t) &
              dplyr::if_else(
                .data$dusk_t < .data$dawn_t,
                .data$rec_time_local >= .data$dusk_t &
                  .data$rec_time_local <= .data$dawn_t,
                .data$rec_time_local >= .data$dusk_t |
                  .data$rec_time_local <= .data$dawn_t
              ) ~ "night",
            TRUE ~ "day"
          ),
          month = lubridate::month(.data$recording_date, label = TRUE)
        ) |>
        dplyr::select(
          -"timezone_raw",
          -"recording_date_time_char",
          -"recording_date_time_local",
          -"dawn",
          -"sunrise",
          -"goldenHourEnd",
          -"goldenHour",
          -"sunset",
          -"dusk",
          -"night"
        )
    }, .id = "timezone_group")
}
