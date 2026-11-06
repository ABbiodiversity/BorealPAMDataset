# This script creates the summary statistics and manuscript tables from the
# final dataset, then produces the spatial distribution figure.
#
# It contains the reporting portion of 7_DatasetSummaryForMS.R together with
# the original 8_Figures.R map code.
#
# Run from the repository root with:
# Rscript scripts/07_make_tables_figures.R final_dataset.Rdata \
#   project_metadata.xlsx sheet_name boreal_boundary.shp output_directory

library(tidyverse)
library(readxl)
library(sf)

args <- commandArgs(trailingOnly = TRUE)

################################################################################
#### 1. Load final dataset and project metadata
load(args[1])
project_meta_manual <- read_excel(args[2], sheet = args[3])
boreal_canada <- st_read(args[4]) %>%
  st_transform(crs = 4326)
output_directory <- args[5]

################################################################################
#### 2. Create summary statistics for recordings, locations, tags, and species

## 2.1 Get totals for manuscript text ----
# Total locations.
ms_locations_final <- dataset_filter %>%
  distinct(location_id, organization, .keep_all = TRUE) %>%
  select(
    organization, project_id, location, location_id, longitude, latitude
  )

# Total recordings.
ms_recordings_final <- dataset_filter %>%
  distinct(recording_id, organization, .keep_all = TRUE) %>%
  select(
    organization, project_id, location, location_id, longitude, latitude,
    recording_id
  )

# Total verified tags.
ms_tags_verified <- dataset_filter %>%
  filter(tag_is_verified == TRUE)

# Total species counts.
ms_species_counts <- dataset_filter %>%
  filter(!grepl("Unidentified|Domestic", species_common_name, ignore.case = TRUE)) %>%
  count(species_common_name, sort = TRUE)

ms_species_counts_domestic <- dataset_filter %>%
  filter(grepl("Domestic", species_common_name, ignore.case = TRUE)) %>%
  count(species_common_name, sort = TRUE)

ms_species_counts_unidentified <- dataset_filter %>%
  filter(grepl("Unidentified", species_common_name, ignore.case = TRUE)) %>%
  count(species_common_name, sort = TRUE)

# Totals table.
ms_totals_table <- data.frame(
  recordings = nrow(ms_recordings_final),
  tags = nrow(dataset_filter),
  tags_verified = nrow(ms_tags_verified),
  locations = nrow(ms_locations_final),
  number_species = nrow(ms_species_counts),
  number_species_domestic = nrow(ms_species_counts_domestic),
  number_species_groups = nrow(ms_species_counts_unidentified)
)

# Remove interim dataframes.
rm(
  ms_recordings_final, ms_tags_verified, ms_locations_final,
  ms_species_counts, ms_species_counts_domestic,
  ms_species_counts_unidentified
)

## 2.2 Organization counts for reporting to partners ----
# Count recordings by organization.
counts_org_rec <- dataset_filter %>%
  group_by(organization) %>%
  summarise(
    num_recordings_used = n_distinct(recording_id),
    .groups = "drop"
  )

# Count tags by organization.
counts_org_tag <- dataset_filter %>%
  group_by(organization) %>%
  summarise(
    num_tags_used = n_distinct(tag_id),
    .groups = "drop"
  )

# Count locations by organization.
counts_org_locations <- dataset_filter %>%
  group_by(organization) %>%
  summarise(
    num_locations_used = n_distinct(location_id),
    .groups = "drop"
  )

# Count projects by organization.
counts_org_projects <- dataset_filter %>%
  group_by(organization) %>%
  summarise(
    num_projects_used = n_distinct(project_id),
    .groups = "drop"
  )

# Combine into a single organization table.
ms_summary_counts_org <- counts_org_projects %>%
  left_join(counts_org_rec, by = "organization") %>%
  left_join(counts_org_tag, by = "organization") %>%
  left_join(counts_org_locations, by = "organization")

rm(
  counts_org_rec, counts_org_tag, counts_org_locations, counts_org_projects
)

## 2.3 Project counts ----
# Count recordings by organization and project.
counts_proj_rec <- dataset_filter %>%
  group_by(organization, project_id) %>%
  summarise(
    num_recordings_used = n_distinct(recording_id),
    .groups = "drop"
  )

# Count tags by organization and project.
counts_proj_tag <- dataset_filter %>%
  group_by(organization, project_id) %>%
  summarise(
    num_tags_used = n_distinct(tag_id),
    .groups = "drop"
  )

# Count locations by organization and project.
counts_proj_locations <- dataset_filter %>%
  group_by(organization, project_id) %>%
  summarise(
    num_locations_used = n_distinct(location_id),
    .groups = "drop"
  )

# Combine into a single project table.
ms_summary_counts_proj <- counts_proj_rec %>%
  left_join(counts_proj_tag, by = c("organization", "project_id")) %>%
  left_join(counts_proj_locations, by = c("organization", "project_id")) %>%
  left_join(
    wt_projects %>%
      select(project_id, project, task_count, project_status, tasks_completed),
    by = "project_id"
  ) %>%
  rename(num_recordings_project = task_count) %>%
  select(
    organization, project, project_id, project_status,
    num_recordings_project, tasks_completed, num_recordings_used,
    num_tags_used, num_locations_used
  )

rm(counts_proj_rec, counts_proj_tag, counts_proj_locations)

################################################################################
#### 3. Generate project metadata from the dataset

project_meta_calc <- dataset_filter %>%
  mutate(year = lubridate::year(recording_date_time)) %>%
  group_by(project_id, year) %>%
  summarise(
    date_range = paste(
      format(min(recording_date_time, na.rm = TRUE), "%Y-%m-%d"),
      format(max(recording_date_time, na.rm = TRUE), "%Y-%m-%d"),
      sep = " to "
    ),
    time_of_day = paste(
      sort(unique(na.omit(time_of_day))),
      collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  group_by(project_id) %>%
  summarise(
    date_range = paste(date_range, collapse = ";"),
    time_of_day = paste(
      sort(unique(unlist(strsplit(time_of_day, ";")))),
      collapse = ";"
    ),
    task_method = paste(
      sort(unique(na.omit(dataset_filter$task_method[
        dataset_filter$project_id == first(project_id)
      ]))),
      collapse = ";"
    ),
    .groups = "drop"
  )

################################################################################
#### 4. Combine manual and calculated project metadata

project_all <- project_meta_calc %>%
  left_join(
    project_meta_manual,
    by = join_by("project_id" == "project_id_lookup")
  ) %>%
  left_join(
    wt_projects %>% select(project_id, project_status),
    by = "project_id"
  ) %>%
  left_join(
    ms_summary_counts_proj %>%
      select(
        project_id, num_recordings_project, num_recordings_used,
        num_tags_used, num_locations_used
      ),
    by = "project_id"
  ) %>%
  rename(project_name = project) %>%
  mutate(
    wildtrax_recordings_available = if_else(
      project_status == "Published - Public",
      "Yes",
      "No"
    ),
    wildtrax_data_available = if_else(
      project_status %in% c(
        "Published - Public",
        "Published - Map+Report Only"
      ),
      "Yes",
      "No"
    )
  ) %>%
  select(
    organization_name, project_name, project_id,
    Partner_organizations_and_groups, Research_goal_of_project,
    Deployment_description, Recording_equipment, Sampling_rate_Hz,
    date_range, time_of_day, task_method, Amphibians_annotated,
    Mammals_annotated, Validation_methods, Validation_in_Wildtrax,
    num_recordings_project, num_recordings_used, num_tags_used,
    num_locations_used, wildtrax_recordings_available,
    wildtrax_data_available
  )

rm(project_meta_calc, project_meta_manual, wt_projects)

################################################################################
#### 5. Create tables for manuscript

## 5.1 Generate tables ----
# Table 1.
table_1 <- project_all %>%
  select(
    organization_name, project_name, project_id,
    Partner_organizations_and_groups, Research_goal_of_project,
    num_recordings_project, num_recordings_used,
    wildtrax_recordings_available, wildtrax_data_available
  )

# Table 2.
table_2 <- project_all %>%
  select(
    organization_name, project_name, project_id, Deployment_description,
    Recording_equipment, Sampling_rate_Hz, date_range, time_of_day
  )

# Table 3.
table_3 <- project_all %>%
  select(
    organization_name, project_name, project_id, task_method,
    Amphibians_annotated, Mammals_annotated, Validation_methods,
    Validation_in_Wildtrax
  )

# Summarize number of tags per taxon in the dataset by project.
tags_taxa <- dataset_filter %>%
  group_by(project_id) %>%
  summarise(
    AVES_unverified = sum(
      species_class == "AVES" & tag_is_verified == FALSE,
      na.rm = TRUE
    ),
    AVES_verified = sum(
      species_class == "AVES" & tag_is_verified == TRUE,
      na.rm = TRUE
    ),
    MAMMALIA_unverified = sum(
      species_class == "MAMMALIA" & tag_is_verified == FALSE,
      na.rm = TRUE
    ),
    MAMMALIA_verified = sum(
      species_class == "MAMMALIA" & tag_is_verified == TRUE,
      na.rm = TRUE
    ),
    AMPHIBIA_unverified = sum(
      species_class == "AMPHIBIA" & tag_is_verified == FALSE,
      na.rm = TRUE
    ),
    AMPHIBIA_verified = sum(
      species_class == "AMPHIBIA" & tag_is_verified == TRUE,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Table 4.
table_4 <- project_all %>%
  left_join(tags_taxa, by = "project_id") %>%
  mutate(
    MAMMALIA_verified = if_else(
      Mammals_annotated == "N",
      "n/a",
      as.character(MAMMALIA_verified)
    ),
    MAMMALIA_unverified = if_else(
      Mammals_annotated == "N",
      "n/a",
      as.character(MAMMALIA_unverified)
    ),
    AMPHIBIA_verified = if_else(
      Amphibians_annotated == "N",
      "n/a",
      as.character(AMPHIBIA_verified)
    ),
    AMPHIBIA_unverified = if_else(
      Amphibians_annotated == "N",
      "n/a",
      as.character(AMPHIBIA_unverified)
    )
  ) %>%
  select(
    organization_name, project_name, project_id, AVES_unverified,
    AVES_verified, MAMMALIA_unverified, MAMMALIA_verified,
    AMPHIBIA_unverified, AMPHIBIA_verified
  )

rm(tags_taxa)

## 5.2 Write tables to CSV files ----
write_csv(ms_totals_table, file.path(output_directory, "dataset_totals.csv"))
write_csv(
  ms_summary_counts_org,
  file.path(output_directory, "organization_counts.csv")
)
write_csv(
  ms_summary_counts_proj,
  file.path(output_directory, "project_counts.csv")
)
write_csv(table_1, file.path(output_directory, "table_1.csv"))
write_csv(table_2, file.path(output_directory, "table_2.csv"))
write_csv(table_3, file.path(output_directory, "table_3.csv"))
write_csv(table_4, file.path(output_directory, "table_4.csv"))

################################################################################
#### 6. Dataset spatial distribution figure

# Retain one row per recording so tags do not overplot the same point.
recordings_final <- dataset_filter %>%
  distinct(recording_id, .keep_all = TRUE)

dataset_map <- ggplot(recordings_final) +
  geom_sf(data = boreal_canada, fill = "white") +
  geom_point(aes(x = longitude, y = latitude, colour = organization))

ggsave(
  file.path(output_directory, "dataset_locations.png"),
  dataset_map,
  width = 10,
  height = 7,
  dpi = 300
)

