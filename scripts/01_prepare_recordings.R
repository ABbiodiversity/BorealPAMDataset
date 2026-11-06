# This script takes the filtered main and recording reports and whittles them
# down to a dataframe of recordings. It calculates sunrise and sunset times,
# labels recordings as sunrise, dawn, sunset, night, or day, assigns BCRs, and
# restricts the data to the Canadian boreal zone and the target date window.
#
# Run from the repository root with:
# Rscript scripts/01_prepare_recordings.R filtered_main.rds \
#   filtered_recording.rds boreal_boundary.shp bcr_boundary.shp output.rds

library(tidyverse)
library(wildrtrax)
library(lutz)
library(purrr)
library(suncalc)
library(lubridate)
library(hms)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
filter_df <- readRDS(args[1])
rec_df <- readRDS(args[2])
boreal_canada <- st_read(args[3])
bcr <- read_sf(args[4])
output_file <- args[5]

################################################################################
# Debugged version of the WildTrax TMTT replacement function ----
# This function was retained from 0_wildrtrax_functions.R in the original repo.
wt_replace_tmtt <- function(data, calc = "round") {

  if (!"recording_date_time" %in% colnames(data)) {
    stop("The `wt_replace_tmtt` function only works on data from the ARU sensor")
  }

  check_none <- data %>%
    select(species_code) %>%
    distinct() %>%
    pull()

  if (length(check_none) == 1 && check_none == "NONE") {
    stop("There are no species in this project")
  }

  .tmtt <- readRDS(
    system.file("extdata", "tmtt_predictions.rds", package = "wildrtrax")
  )

  dat.tmtt <- mutate(data, id = row_number())

  # Only TMTT rows for replacement.
  dat.tmt <- dat.tmtt %>% filter(abundance %in% c("TMTT", "TNPE"))

  if (nrow(dat.tmt) > 0) {
    dat.tmt <- dat.tmt %>%
      rename(species_code_og = species_code) %>%
      mutate(
        species_code = ifelse(
          species_code_og %in% .tmtt$species_code,
          species_code_og,
          "species"
        ),
        observer_id = as.integer(
          ifelse(observer_id %in% .tmtt$observer_id, observer_id, 0)
        )
      ) %>%
      inner_join(
        .tmtt %>% select(species_code, observer_id, pred),
        by = c("species_code", "observer_id")
      ) %>%
      mutate(
        abundance = case_when(
          calc == "round" ~ round(pred),
          calc == "ceiling" ~ ceiling(pred),
          calc == "floor" ~ floor(pred),
          TRUE ~ NA_real_
        ),
        species_code = species_code_og
      ) %>%
      select(-pred, -species_code_og)
  }

  # Replace TMTT rows with predictions.
  dat.tmtt <- suppressWarnings(
    dat.tmtt %>%
      mutate(
        abundance = case_when(
          abundance %in% c("TMTT", "TNPE") ~ NA_real_,
          TRUE ~ as.numeric(abundance)
        )
      ) %>%
      rows_update(dat.tmt, by = c("id")) %>%
      select(-id)
  )

  return(dat.tmtt)
}

################################################################################
# Tidy tags and identify usable recordings ----

# Tidy the tags.
tags <- filter_df %>%
  wt_tidy_species(remove = c("abiotic", "insect", "human", "unknown")) %>%
  wt_replace_tmtt()

# Simplify dataframe and reduce it to one row per recording_id.
# Do some filtering, including removing projects known not to be usable.
# Project 41 has only YERA tags.
recordings_all <- tags %>%
  left_join(rec_df, multiple = "all") %>%
  dplyr::filter(
    species_code != "NONE",
    !str_detect(task_noise, "ARU Malfunction"),
    !str_detect(task_noise, "Extreme"),
    !str_detect(task_noise, "High"),
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  mutate(project_id = as.numeric(project_id)) %>%
  dplyr::filter(
    !project_id %in% c(
      3524, 2813, 2350, 2115, 2423, 2812,
      1650, 1260, 691, 1136, 3236, 41
    )
  ) %>%
  select(
    organization, project_id, location, location_id, longitude, latitude,
    recording_id, recording_date_time, task_id, task_duration, task_method,
    task_noise
  ) %>%
  distinct(recording_id, .keep_all = TRUE)

# 1. Add timezone per location ----
recordings_tz <- recordings_all %>%
  mutate(
    timezone_raw = tz_lookup_coords(
      lat = latitude,
      lon = longitude,
      method = "accurate"
    ),
    timezone = sub(";.*", "", timezone_raw)
  )

# 2. Split dataframe into a list by timezone ----
recordings_list <- split(recordings_tz, recordings_tz$timezone)

# 3. Work within each timezone to apply the timezone to the recording time and
# get sunrise, sunset, time-of-day, and month values ----
recordings_list_calculations <- map(recordings_list, function(x) {
  tz <- unique(x$timezone)

  # Step 1: ensure POSIXct and localize, keeping the original as character.
  x <- x %>%
    mutate(
      recording_date_time_char = as.character(recording_date_time),
      recording_date_time_char = if_else(
        nchar(recording_date_time_char) == 10,
        paste0(recording_date_time_char, " 00:00:00"),
        recording_date_time_char
      ),
      # Convert to POSIXct in UTC, then assign local timezone without shifting
      # the clock time.
      recording_date_time_posixct = as.POSIXct(
        recording_date_time_char,
        tz = "UTC"
      ),
      recording_date_time_local = force_tz(recording_date_time_posixct, tz),
      recording_date = as.Date(recording_date_time_local, tz = tz),
      recording_time = as_hms(recording_date_time_local)
    )

  # Step 2: get sunrise and sunset values for each row.
  sun_times <- getSunlightTimes(
    data = data.frame(
      date = x$recording_date,
      lat = x$latitude,
      lon = x$longitude
    ),
    keep = c(
      "nightEnd", "nauticalDawn", "dawn", "sunrise", "goldenHourEnd",
      "goldenHour", "sunset", "dusk", "nauticalDusk", "night", "nadir"
    ),
    tz = tz
  )

  # Step 3: attach sunrise and sunset calculations.
  x <- bind_cols(
    x,
    sun_times %>%
      select(
        nightEnd, nauticalDawn, dawn, sunrise, goldenHourEnd, goldenHour,
        sunset, dusk, nauticalDusk, night, nadir
      )
  )

  # Step 4: convert date-times into time values for comparisons.
  x <- x %>%
    mutate(
      rec_time_local = as_hms(recording_date_time_local),
      dawn_t = as_hms(dawn),
      sunrise_t = as_hms(sunrise),
      goldenHourEnd_t = as_hms(goldenHourEnd),
      goldenHour_t = as_hms(goldenHour),
      sunset_t = as_hms(sunset),
      dusk_t = as_hms(dusk),
      nadir_t = as_hms(nadir)
    )

  # Step 5: classify recordings into time-of-day categories.
  x <- x %>%
    mutate(
      time_of_day = case_when(
        # 1. Sunrise window: within golden hour.
        !is.na(dawn_t) &
          rec_time_local >= dawn_t &
          rec_time_local <= goldenHourEnd_t ~ "sunrise",
        # 2. When there is no dawn, use nadir to the end of golden hour.
        is.na(dawn_t) &
          !is.na(sunrise_t) &
          rec_time_local >= nadir_t &
          rec_time_local <= goldenHourEnd_t ~ "sunrise",
        # 3. When there is no sunrise, use golden hours after nadir.
        is.na(sunrise_t) &
          rec_time_local >= nadir_t &
          rec_time_local <= goldenHourEnd_t ~ "sunrise",
        # 4. Dawn: from the end of golden hour to five hours after sunrise.
        !is.na(sunrise_t) &
          rec_time_local > goldenHourEnd_t &
          rec_time_local <= (sunrise_t + (5 * 3600)) ~ "dawn",
        # 5. Define dawn from nadir when there is no sunrise.
        is.na(dawn_t) &
          is.na(sunrise_t) &
          rec_time_local >= nadir_t &
          rec_time_local <= (nadir_t + (5 * 3600)) ~ "dawn",
        # 6. Sunset: evening golden hour to dusk before midnight.
        !is.na(dusk_t) &
          dusk_t > goldenHour_t &
          rec_time_local >= goldenHour_t &
          rec_time_local <= dusk_t ~ "sunset",
        # 7. Sunset when dusk occurs after midnight.
        !is.na(dusk_t) &
          dusk_t < goldenHour_t &
          (rec_time_local >= goldenHour_t | rec_time_local <= dusk_t) ~
          "sunset",
        # 8. No dusk: golden hour to nadir, where nadir is before midnight.
        is.na(dusk_t) &
          nadir_t > goldenHour_t &
          rec_time_local >= goldenHour_t &
          rec_time_local < nadir_t ~ "sunset",
        # 9. No dusk: golden hour to nadir, where nadir is after midnight.
        is.na(dusk_t) &
          nadir_t < goldenHour_t &
          (rec_time_local >= goldenHour_t | rec_time_local < nadir_t) ~
          "sunset",
        # 10. Night: between dusk and dawn on either side of midnight.
        !is.na(dusk_t) & !is.na(dawn_t) &
          if_else(
            dusk_t < dawn_t,
            rec_time_local >= dusk_t & rec_time_local <= dawn_t,
            rec_time_local >= dusk_t | rec_time_local <= dawn_t
          ) ~ "night",
        # 11. All remaining cases are daytime.
        TRUE ~ "day"
      ),
      month = month(recording_date, label = TRUE)
    )

  return(x)
})

# 4. Combine timezones and remove erroneous Asia and Europe locations ----
recordings_combined <- bind_rows(
  recordings_list_calculations,
  .id = "timezone_group"
) %>%
  filter(!timezone_group %in% c("Asia/Irkutsk", "Asia/Shanghai", "Europe/Paris")) %>%
  dplyr::select(-c(timezone_raw, recording_date_time_char, recording_date_time_local:night))

# 5. Clip to the Canadian boreal zone ----
# Convert recordings to sf points.
recordings_locations <- st_as_sf(
  recordings_combined,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# Make sure the shapefile and points have the same CRS.
recordings_locations <- st_transform(
  recordings_locations,
  crs = st_crs(boreal_canada)
)

# Join spatial layers to retain locations inside the boreal shapefile.
recordings_boreal <- st_join(recordings_locations, boreal_canada) %>%
  filter(!is.na(TYPE))

# Check the spatial filtering.
ggplot() +
  geom_point(
    data = recordings_locations,
    aes(x = longitude, y = latitude),
    colour = "grey"
  ) +
  geom_point(
    data = recordings_boreal,
    aes(x = longitude, y = latitude),
    colour = "green"
  )

# 6. Assign Bird Conservation Regions ----
bcr <- bcr %>%
  st_transform(terra::crs(recordings_boreal))

locations_boreal <- recordings_boreal %>%
  dplyr::select(location_id, latitude, longitude, geometry) %>%
  unique()

locations_bcr <- locations_boreal %>%
  st_intersection(bcr) %>%
  st_drop_geometry() %>%
  dplyr::select(location_id, name_en) %>%
  rename(bcr = name_en)

recordings_bcr <- recordings_boreal %>%
  st_drop_geometry() %>%
  inner_join(locations_bcr)

# 7. Final temporal filtering ----
# Apply the target dates and time-of-day limits. Recordings with only one tag at
# sunrise or dawn were removed. Tasks from project 41 only had YERA annotated
# and were already removed above.
recordings_out <- recordings_bcr %>%
  left_join(filter_df %>% mutate(project_id = as.numeric(project_id))) %>%
  dplyr::filter(species_code != "NONE") %>%
  group_by(task_id) %>%
  summarize(tags = n()) %>%
  ungroup() %>%
  left_join(recordings_bcr) %>%
  mutate(doy = yday(recording_date_time)) %>%
  dplyr::filter(
    doy > 151,
    doy < 186,
    time_of_day != "day",
    !(tags == 1 & time_of_day %in% c("sunrise", "dawn"))
  )

saveRDS(recordings_out, output_file)

