# 1. This script combines the downloaded selected datasets (the first dataset
#    is all organizations except CWS-PRA; the second is CWS-PRA only).
# 2. It updates tag verifications and ratings based on secondary work completed
#    by the authors.
# 3. It removes recordings from organizations that did not give final approval,
#    and removes sensitive locations or species identified by partners.
# 4. It combines species that were tagged differently in different datasets
#    (Gray Jay/Canada Jay, Myrtle/Yellow-rumped Warbler, Common/Wilson's Snipe).
# 5. It adds WildTrax species and project metadata and saves the final dataset.
#
# The same consolidated selection script (02_select_recordings.R) was used to
# create both selected inputs; separate inputs are retained here because that is
# how the released dataset was assembled historically.
#
# Run from the repository root with:
# Rscript scripts/06_finalize_dataset.R selected_main.Rdata \
#   selected_cwspra.Rdata ratings.csv verifications.csv \
#   private_location_exclusions.csv final_dataset.Rdata

library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(readxl)
library(wildrtrax)

args <- commandArgs(trailingOnly = TRUE)

# Credentials are set outside this repository.
wt_auth()

################################################################################
#### 1. Load and organize selected dataset

## 1.1 Load datasets ----
# Load dataset of everything besides CWS-PRA and rename.
selected_main <- new.env()
load(args[1], envir = selected_main)
recordings_select_most <- selected_main$recordings_use
tags_select_most <- selected_main$tags_use

# Load CWS-PRA dataset and combine everything.
selected_cwspra <- new.env()
load(args[2], envir = selected_cwspra)

recordings_select_all <- rbind(
  recordings_select_most,
  selected_cwspra$recordings_use
)
tags_select_all <- rbind(tags_select_most, selected_cwspra$tags_use)

# Adjust project_id for the ABMI OSM 2022 project because it changed in
# WildTrax after the initial download.
recordings_select_all <- recordings_select_all %>%
  mutate(
    project_id = if_else(
      project_id == 1174,
      4952,
      project_id
    )
  )

################################################################################
#### 2. Update tag verifications and ratings based on secondary author work

# Import verification and rating CSVs.
ratings <- read.csv(args[3])
verifications <- read.csv(args[4])

# Update dataset tag ratings from secondary rating work.
tags_update_rating <- tags_select_all %>%
  left_join(
    ratings %>% select(tag_id, rating),
    by = "tag_id"
  ) %>%
  mutate(tag_rating = coalesce(as.character(rating), tag_rating)) %>%
  select(-rating)

# Update dataset verifications and remove tags rejected during secondary
# verification work.
tags_final <- tags_update_rating %>%
  left_join(
    verifications %>% select(tag_id, verification),
    by = "tag_id"
  ) %>%
  filter(is.na(verification) | verification != "FALSE") %>%
  mutate(
    tag_is_verified = if_else(
      verification == "TRUE" & !is.na(verification),
      TRUE,
      tag_is_verified
    )
  ) %>%
  select(-verification)

## 2.1 Get verification totals for manuscript text ----
verifications_confirmed <- verifications %>%
  filter(verification == TRUE)
verifications_denied <- verifications %>%
  filter(verification == FALSE)
verifications_unknown <- verifications %>%
  filter(verification == "IGNORE")

## 2.2 Combine recordings and tags into one master selected dataset ----
dataset <- tags_final %>%
  left_join(recordings_select_all, by = "task_id") %>%
  select(-"recording_id.y") %>%
  rename(recording_id = recording_id.x)

# Remove intermediate dataframes.
rm(
  selected_main, selected_cwspra, recordings_select_most,
  recordings_select_all, tags_select_most, tags_select_all, tags_final,
  ratings, verifications, verifications_confirmed, verifications_denied,
  verifications_unknown, tags_update_rating
)

################################################################################
#### 3. Remove organizations, locations, recordings, and projects excluded
####    after dataset selection

## 3.1 Remove sensitive locations identified by CWS-NOR partners ----
locations_remove_CWS_NOR <- read.csv(args[5])

# Remove CWS-ATL and BSC-SK because final permission was not confirmed, remove
# CWS-NOR partner-excluded locations, and remove the BU project requested by
# Erin Bayne.
dataset_remove_1 <- dataset %>%
  filter(!organization %in% c("CWS-ATL", "BSC-SK")) %>%
  filter(!location_id %in% locations_remove_CWS_NOR$location_id) %>%
  filter(!project_id == 1099)

## 3.2 Remove recordings containing culturally sensitive species from selected
##     CWS-NOR projects ----
recording_ids_to_remove <- dataset_remove_1 %>%
  filter(
    project_id %in% c(15, 1696),
    species_code %in% c("CANG", "COLO", "CONI", "GGOW", "RNGR", "RUGR")
  ) %>%
  pull(recording_id) %>%
  unique()

dataset_remove_2 <- dataset_remove_1 %>%
  dplyr::filter(!recording_id %in% recording_ids_to_remove)

# Remove intermediate dataframes.
rm(dataset, locations_remove_CWS_NOR, dataset_remove_1)

################################################################################
#### 4. Clean up species list in tags

## 4.1 Convert some species codes for consistency ----
dataset_update <- dataset_remove_2 %>%
  # Convert GRAJ to CAJA.
  mutate(
    species_common_name = if_else(
      species_common_name == "Gray Jay",
      "Canada Jay",
      species_common_name
    ),
    species_code = if_else(
      species_common_name == "Canada Jay",
      "CAJA",
      species_code
    )
  ) %>%
  # Convert MYWA to YRWA.
  mutate(
    species_common_name = if_else(
      species_common_name == "Myrtle Warbler",
      "Yellow-rumped Warbler",
      species_common_name
    ),
    species_code = if_else(
      species_common_name == "Yellow-rumped Warbler",
      "YRWA",
      species_code
    ),
    species_scientific_name = if_else(
      species_common_name == "Yellow-rumped Warbler",
      "SETOPHAGA CORONATA",
      species_scientific_name
    )
  ) %>%
  # Convert COSN to WISN.
  mutate(
    species_common_name = if_else(
      species_common_name == "Common Snipe",
      "Wilson's Snipe",
      species_common_name
    ),
    species_code = if_else(
      species_common_name == "Wilson's Snipe",
      "WISN",
      species_code
    ),
    species_scientific_name = if_else(
      species_common_name == "Wilson's Snipe",
      "GALLINAGO DELICATA",
      species_scientific_name
    )
  ) %>%
  # Convert Canis Species to Unidentified Canis Species.
  mutate(
    species_common_name = if_else(
      species_common_name == "Canis Species",
      "Unidentified Canis Species",
      species_common_name
    )
  ) %>%
  # Add Unidentified to the Mourning/MacGillivray's Warbler group.
  mutate(
    species_common_name = if_else(
      species_common_name == "Mourning / MacGillivray's Warbler",
      "Unidentified Mourning / MacGillivray's Warbler",
      species_common_name
    )
  )

## 4.2 Add species and project metadata ----
wt_species <- wt_get_species()
wt_projects <- wt_get_projects(sensor = "ARU")

dataset_metadata <- dataset_update %>%
  left_join(wt_species, by = "species_code") %>%
  left_join(
    wt_projects %>%
      select(project_id, project, task_count, project_status, tasks_completed),
    by = "project_id"
  )

## 4.3 Remove non-animal tags ----
dataset_filter <- dataset_metadata %>%
  filter(
    species_class != "ABIOTIC" &
      species_class != "HUMAN ACTIVITY" &
      species_class != "INSECTA" &
      species_common_name.x != "NONE"
  ) %>%
  select(!c(species_id, species_scientific_name.y, species_common_name.y)) %>%
  rename(
    species_common_name = species_common_name.x,
    species_scientific_name = species_scientific_name.x
  )

# Remove excess datasets.
rm(dataset_remove_2, dataset_update, dataset_metadata, wt_species)

## 4.4 Save final filtered dataset ----
save(dataset_filter, wt_projects, file = args[6])

