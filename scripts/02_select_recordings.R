# This script selects recordings for the dataset. It first prioritizes
# recordings with existing verified or rated tags, samples one recording per
# location and time-of-day stratum, spatially thins locations with a 2 km grid,
# clips tags to the first 180 seconds, and attaches recording URLs.
#
# This consolidates the former general and CWS-PRA selection scripts; the same
# code can be run with either set of external inputs.
#
# Run from the repository root with:
# Rscript scripts/02_select_recordings.R recordings.rds filtered_main.rds \
#   filtered_recording.rds selected_dataset.Rdata

library(tidyverse)
library(dggridR)

args <- commandArgs(trailingOnly = TRUE)

# Get the things ----
recordings <- readRDS(args[1])

tags <- readRDS(args[2]) %>%
  dplyr::select(
    recording_id, task_id, species_code:tag_rating, observer_id
  )

# Merge the things ----
all <- inner_join(recordings, tags)

# ID recordings with validation and tag rating ----
verified <- all %>%
  dplyr::filter(tag_is_verified == TRUE) %>%
  group_by(task_id) %>%
  summarize(total_tags = n()) %>%
  unique()

rated <- all %>%
  dplyr::filter(!is.na(tag_rating)) %>%
  group_by(task_id) %>%
  summarize(total_tags = n()) %>%
  unique()

recordings_verified <- recordings %>%
  mutate(
    verified = ifelse(task_id %in% verified$task_id, 1, 0),
    rated = ifelse(task_id %in% rated$task_id, 1, 0)
  )

# Select the things ----
# Take verified ones first.
set.seed(1)
every1 <- recordings_verified %>%
  dplyr::filter(verified == 1) %>%
  group_by(location, time_of_day) %>%
  sample_n(1, replace = FALSE) %>%
  ungroup()

set.seed(2)
every2 <- recordings_verified %>%
  dplyr::filter(verified == 0) %>%
  rbind(every1) %>%
  group_by(location, time_of_day) %>%
  sample_n(1, replace = FALSE) %>%
  ungroup()

# Thin the things ----
grid <- dgconstruct(spacing = 2, metric = TRUE)

# First calculate recording counts per location.
location_counts <- every2 %>%
  group_by(
    bcr,
    organization,
    project_id,
    location_id,
    longitude,
    latitude
  ) %>%
  summarize(
    recordings = n(),
    .groups = "drop"
  ) %>%
  # Assign each location to a spatial cell.
  mutate(
    cell = dgGEO_to_SEQNUM(
      grid,
      longitude,
      latitude
    )$seqnum
  )

# Within each cell:
# 1. Keep locations with the maximum number of recordings.
# 2. Randomly select one if ties exist.
set.seed(3)

selected_locations <- location_counts %>%
  group_by(bcr, organization, cell) %>%
  filter(recordings == max(recordings)) %>%
  sample_n(1) %>%
  ungroup()

# Rejoin selected locations back to recordings.
every3 <- every2 %>%
  semi_join(
    selected_locations,
    by = c(
      "bcr",
      "organization",
      "project_id",
      "location_id",
      "longitude",
      "latitude"
    )
  ) %>%
  inner_join(recordings)

# Wrangle for saving: clip tags to under three minutes ----
tags_use <- tags %>%
  dplyr::filter(task_id %in% every3$task_id) %>%
  mutate(detection_end = detection_time + tag_duration) %>%
  dplyr::filter(detection_end < 180) %>%
  unique()

# Look at some things ----
ggplot(every3) +
  geom_hex(aes(x = longitude, y = latitude))

ggplot(every3) +
  geom_hex(aes(x = longitude, y = latitude)) +
  facet_wrap(~time_of_day)

################################################################################
# Add recording_URL to dataset ----
filter_df_rec <- readRDS(args[3])

recordings_use <- every3 %>%
  left_join(
    filter_df_rec %>%
      select(recording_id, source_file_name, recording_url),
    by = "recording_id"
  ) %>%
  unique()

# Save datasets ----
save(recordings_use, tags_use, file = args[4])

