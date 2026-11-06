# This script identifies tags for manual verification. It first prepares eBird
# Status and Trends occurrence products, identifies annotations outside expected
# occurrence, and then uses HawkEars scores to restrict the verification list.
#
# It consolidates 3a_eBirdVerificationData.R and the two former verification
# selection scripts. The original numbered sections and exploratory checks are
# retained below.
#
# Run from the repository root with:
# Rscript scripts/03_select_verification_tags.R selected_dataset.Rdata \
#   ABA_checklist.csv ebird_data_directory hawkears_scores.csv output.csv

library(tidyverse)
library(ebirdst)
library(auk)
library(terra)
library(sf)
library(wildrtrax)

args <- commandArgs(trailingOnly = TRUE)

# Load selected dataset ----
load(args[1])

# Login to WildTrax. Credentials are set outside this repository.
wt_auth()

aba_checklist_file <- args[2]
ebird_data_directory <- args[3]
hawkears_scores_file <- args[4]
verification_output_file <- args[5]

################################################################################
# 1. Get eBird occurrence rasters ####

## 1a. List of North American species ----
nabirds <- read.csv(aba_checklist_file, skip = 4) %>%
  data.table::setnames(
    c("family", "common_name", "french", "scientific_name", "species", "status")
  ) %>%
  dplyr::select(-family, -french) %>%
  dplyr::filter(status %in% c(1, 2)) %>%
  mutate(
    common_name = case_when(
      common_name == "Black-crowned Night Heron (Night-Heron)" ~
        "Black-crowned Night Heron",
      common_name == "Common Redpoll" ~ "Redpoll",
      common_name == "Herring Gull" ~ "American Herring Gull",
      # common_name == "Whimbrel" ~ "Hudsonian/Eurasian Whimbrel",
      # common_name == "Yellow Warbler" ~ "Mangrove Yellow Warbler",
      common_name == "House Wren" ~ "Northern/Southern House Wren",
      !is.na(common_name) ~ common_name
    ),
    scientific_name = case_when(
      scientific_name == "Accipiter cooperii" ~ "Astur cooperii",
      # scientific_name == "Coccothraustes vespertinus" ~
      #   "Hesperiphona vespertina",
      # scientific_name == "Dryobates villosus" ~ "Leuconotopicus villosus",
      scientific_name == "Larus argentatus" ~ "Larus smithsonianus",
      scientific_name == "Accipiter atricapillus" ~ "Astur atricapillus",
      # scientific_name == "Numenius phaeopus" ~
      #   "Numenius hudsonicus/phaeopus",
      scientific_name == "Troglodytes aedon" ~
        "Troglodytes aedon/musculus",
      !is.na(scientific_name) ~ scientific_name
    )
  )

## 1b. Get the eBird codes ----
codes <- get_ebird_taxonomy()

## 1c. Make to-do list ----
all <- nabirds %>%
  left_join(ebirdst_runs %>% mutate(available = 1)) %>%
  arrange(species)

# Keep the species list beside the verification output for provenance.
write.csv(
  all,
  file.path(dirname(verification_output_file), "eBirdSpeciesList.csv"),
  row.names = FALSE
)

# Check species without available Status and Trends products.
nrow(dplyr::filter(all, is.na(available)))

todo <- dplyr::filter(all, !is.na(available))

## 1d. Download the occurrence files ----
for (i in 1:nrow(todo)) {
  try(
    ebirdst_download_status(
      species = todo$species_code[i],
      path = ebird_data_directory,
      download_abundance = FALSE,
      download_occurrence = TRUE,
      download_ranges = FALSE,
      pattern = "_9km_",
      force = TRUE
    )
  )
}

################################################################################
# 2. Identify outliers with eBird ####

## 2a. Some tidying ----
tags_clean <- tags_use %>%
  mutate(
    species_code = case_when(
      species_code == "GRAJ" ~ "CAJA",
      species_code == "PSFL" ~ "WEFL",
      species_code == "COFL" ~ "WEFL",
      species_code == "MYWA" ~ "YRWA",
      species_code == "COSN" ~ "WISN",
      species_code == "MEGU" ~ "SBIG",
      species_code == "NOGO" ~ "AGOS",
      !is.na(species_code) ~ species_code
    )
  ) %>%
  left_join(
    recordings_use %>%
      dplyr::select(task_id, longitude, latitude, recording_date_time) %>%
      unique(),
    by = "task_id"
  )

## 2b. Make a species list ----
spp <- all %>%
  mutate(
    species = case_when(
      species == "AGOL" ~ "AMGO",
      species == "AGOS" ~ "NOGO",
      species == "BTYW" ~ "BTGW",
      species == "BADO" ~ "BAOW",
      species == "HOWR" ~ "NHWR",
      species == "CORE" ~ "REDP",
      !is.na(species) ~ species
    )
  ) %>%
  inner_join(data.frame(species = unique(tags_clean$species_code), tag = TRUE)) %>%
  filter(species_code != "yebsap-example")

## 2c. Set up a species loop ----
out_list <- list()
missed <- c()

for (i in 1:nrow(spp)) {
  spp_i <- spp$species[i]

  ## 2d. Load the eBird raster ----
  r_i <- try(
    ebirdst::load_raster(
      spp$species_code[i],
      product = "occurrence",
      resolution = "9km",
      path = ebird_data_directory
    )
  )

  # If it failed, try the download again in case the data were not present.
  if (inherits(r_i, "try-error")) {
    try(
      ebirdst_download_status(
        spp$species_code[i],
        path = ebird_data_directory,
        download_occurrence = TRUE,
        download_abundance = FALSE,
        pattern = "_9km_"
      )
    )
    r_i <- try(
      ebirdst::load_raster(
        spp$species_code[i],
        product = "occurrence",
        resolution = "9km",
        path = ebird_data_directory
      )
    )
  }

  if (inherits(r_i, "try-error")) {
    missed <- c(missed, spp_i)
    next
  }

  ## 2e. Match the weeks ----
  wks <- isoweek(ymd(names(r_i)))
  names(r_i) <- wks

  ## 2f. Filter to species of interest ----
  tags_i <- tags_clean %>%
    dplyr::filter(species_code == spp_i) %>%
    mutate(week = isoweek(recording_date_time)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs(r_i))

  ## 2g. Week loop ----
  wks_i <- unique(tags_i$week)

  out_i <- list()
  for (j in 1:length(wks_i)) {
    tags_j <- tags_i %>%
      dplyr::filter(week == wks_i[j]) %>%
      st_as_sf(coords = c("longitude", "latitude"))

    ## 2h. Get occurrence values ----
    occur_j <- terra::extract(r_i[[wks_i[[j]]]], tags_j)

    out_i[[j]] <- tags_j %>%
      st_drop_geometry() %>%
      cbind(data.frame(occurrence = occur_j[, 2]))
  }

  ## 2i. Package and report ----
  out_list[[i]] <- do.call(rbind, out_i)
  cat(i, " - ", spp_i, "\n")
}

## 2j. Package ----
out <- data.table::rbindlist(out_list)

## 2k. Identify ones for verification ----
ebird_r <- out %>%
  dplyr::filter(occurrence == 0) %>%
  left_join(
    recordings_use %>%
      dplyr::select(task_id, longitude, latitude) %>%
      unique(),
    by = "task_id"
  ) %>%
  dplyr::select(-week)

## 2l. Add species that do not have rasters and are therefore likely mistakes ----
ebird_na_spp <- all %>%
  mutate(
    species = case_when(
      species == "AGOL" ~ "AMGO",
      species == "AGOS" ~ "NOGO",
      species == "BTYW" ~ "BTGW",
      species == "BADO" ~ "BAOW",
      species == "HOWR" ~ "NHWR",
      species == "CORE" ~ "REDP",
      !is.na(species) ~ species
    )
  ) %>%
  right_join(data.frame(species = unique(tags_clean$species_code), tag = TRUE)) %>%
  dplyr::filter(is.na(common_name)) %>%
  dplyr::select(species) %>%
  rename(species_code = species) %>%
  inner_join(wildrtrax::wt_get_species()) %>%
  dplyr::filter(species_class == "AVES")

ebird_na <- tags_clean %>%
  dplyr::filter(species_code %in% ebird_na_spp$species_code) %>%
  mutate(occurrence = NA)

## 2m. Put eBird results together ----
ebird <- rbind(ebird_r, ebird_na) %>%
  left_join(
    recordings_use %>%
      dplyr::select(
        recording_id, source_file_name, location, recording_date_time
      ) %>%
      unique()
  ) %>%
  mutate(
    recording = tools::file_path_sans_ext(source_file_name)
  ) # Some source files were not WAV files.

################################################################################
# 3. Identify outliers with HawkEars ####

## 3a. Load output ----
he <- read.csv(hawkears_scores_file) %>%
  rename(species_code = name)

## 3b. Summarize to maximum score ----
he_rec <- he %>%
  group_by(species_code, recording) %>%
  summarize(score = max(score)) %>%
  ungroup()

# Visualize.
sum(is.na(he_rec$score))
hist(he_rec$score)

sum(ebird$recording %in% he_rec$recording)

# Make location_date_time column. This fixed incomplete filename matching and
# produced complete matching with the HawkEars output.
ebird$ldt <- paste(
  ebird$location,
  format(as.Date(ebird$recording_date_time), "%Y%m%d"),
  format(ebird$recording_date_time, "%H%M%S"),
  sep = "_"
)

all(ebird$ldt %in% he_rec$recording)

# Replace recording column so everything below can remain the same.
ebird$recording <- ebird$ldt

################################################################################
# 4. Combine HawkEars and eBird information ####

## 4a. Put together ----
# Restrict to birds, remove unidentified taxa, and retain those with HawkEars
# score below 0.2.
verif_todo <- ebird %>%
  left_join(he_rec) %>%
  mutate(score = ifelse(is.na(score), 0, score)) %>%
  dplyr::filter(score < 0.2) %>%
  left_join(recordings_use) %>%
  inner_join(
    wildrtrax::wt_get_species() %>%
      dplyr::filter(
        species_class == "AVES",
        str_sub(species_common_name, 1, 12) != "Unidentified"
      ) %>%
      dplyr::select(species_code, species_class)
  )

all(verif_todo$recording %in% he_rec$recording)
sum(verif_todo$score == 0)

## 4b. Group into verified recordings ----
verif_rec <- verif_todo %>%
  dplyr::select(all_of(colnames(recordings_use))) %>%
  mutate(verified = 1)

################################################################################
# 5. Check if additional verification is required ####

# Threshold for verification per BCR.
thresh <- 0.03

## 5a. Put together with already verified data ----
verif_all <- recordings_use %>%
  dplyr::filter(!recording_id %in% verif_rec$recording_id) %>%
  rbind(verif_rec)

## 5b. Calculate proportion with at least one verified tag per BCR ----
verif_bcr <- verif_all %>%
  group_by(bcr) %>%
  summarize(
    verified = sum(verified),
    total = n()
  ) %>%
  ungroup() %>%
  mutate(
    percent = verified / total,
    needed = thresh - percent,
    needed = ifelse(needed < 0, 0, needed),
    recordings = round(needed * total)
  )

# This calculation was explored but was not used to add records.
verif_bcr

# 6. Save for WildTrax/manual review ----
write.csv(verif_todo, verification_output_file, row.names = FALSE)

