# This script selects tags for manual quality rating. It retains the historical
# sampling code used to create the released dataset, including its original
# exploratory plots and sampling behavior.
#
# Run from the repository root with:
# Rscript scripts/04_select_rating_tags.R selected_dataset.Rdata output.csv

library(tidyverse)
library(terra)
library(sf)
library(GGally)
library(wildrtrax)

args <- commandArgs(trailingOnly = TRUE)

# Load dataset ----
load(args[1])

# Login. Credentials are set outside this repository.
wt_auth()

################################################################################
# 1. Select existing rated tags ----
rated <- tags_use %>%
  dplyr::filter(!is.na(tag_rating)) %>%
  mutate(tag_rating = as.integer(tag_rating)) %>%
  left_join(
    recordings_use %>%
      dplyr::select(recording_id, latitude, longitude, bcr)
  )

################################################################################
# 2. Look at species distribution ----
table(rated$species_code, rated$vocalization)

################################################################################
# 3. Look at spatial distribution ----
ggplot(rated) +
  geom_point(
    data = recordings_use,
    aes(x = longitude, y = latitude),
    size = 0.5
  ) +
  geom_point(aes(x = longitude, y = latitude, colour = factor(tag_rating)))

# Most existing ratings were in the prairies.

################################################################################
# 4. Attributes of ratings ----
# This was used to determine whether additional stratification was worthwhile.
rated_attributes <- rated %>%
  dplyr::select(
    individual_order, vocalization, detection_time, tag_duration,
    rms_peak_dbfs, tag_rating
  ) %>%
  dplyr::filter(rms_peak_dbfs < 0)

ggpairs(rated_attributes)

# There may be some relationship with rms_peak_dbfs.
ggplot(rated_attributes) +
  geom_jitter(aes(x = rms_peak_dbfs, y = tag_rating))

ggplot(rated_attributes) +
  geom_violin(aes(x = as.factor(tag_rating), y = rms_peak_dbfs))

# The relationship was not strong enough to stratify on this variable.

################################################################################
# 5. Calculate number of ratings needed per species ----

# Set target.
goal1 <- 20

quotas <- tags_use %>%
  group_by(species_code) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  left_join(
    tags_use %>%
      dplyr::filter(!is.na(tag_rating)) %>%
      group_by(species_code) %>%
      summarize(rated = n()) %>%
      ungroup()
  ) %>%
  mutate(
    rated = ifelse(is.na(rated), 0, rated),
    available = total - rated,
    needed = ifelse(rated > goal1, 0, goal1 - rated),
    needed = ifelse(needed > available, available, needed)
  )

sum(quotas$needed)

################################################################################
# 6. Select tags for rating ----
# Use any location for these ones.

# The historical code used needed > 1. This is retained because the repository
# documents the selection that was actually completed.
todo <- quotas %>%
  dplyr::filter(needed > 1)

select_list <- list()
for (i in 1:nrow(todo)) {
  spp_i <- todo$species_code[i]
  n_i <- todo$needed[i]

  tags_i <- tags_use %>%
    dplyr::filter(
      species_code == spp_i,
      is.na(tag_rating)
    )

  tags_ver <- tags_i %>%
    dplyr::filter(tag_is_verified == TRUE)

  if (nrow(tags_ver) > 0) {
    n2_i <- n_i - nrow(tags_ver)

    if (n2_i < 1) {
      select_list[[i]] <- tags_ver %>%
        sample_n(n_i, replace = FALSE)
    } else {
      set.seed(i)
      select_list[[i]] <- tags_i %>%
        sample_n(n2_i, replace = FALSE) %>%
        rbind(tags_ver)
    }
  } else {
    set.seed(i)
    select_list[[i]] <- tags_i %>%
      sample_n(n_i, replace = FALSE)
  }
}

tags_select <- do.call(rbind, select_list)

################################################################################
# 7. Select additional tags for species with high numbers in the prairies ----

# Make an exclusion area.
exclude <- recordings_use %>%
  dplyr::filter(
    rated == 1,
    longitude < -80
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_union() %>%
  st_concave_hull(ratio = 1) %>%
  st_sf()

# Identify recordings to include.
recordings_include <- recordings_use %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_filter(exclude, .predicate = st_disjoint)

# Set target.
goal2 <- 5

# Wrangle quotas.
extra <- quotas %>%
  dplyr::filter(
    needed == 0,
    total > rated
  ) %>%
  mutate(needed = ifelse(available < goal2, available, goal2))

# Select.
extra_list <- list()
for (i in 1:nrow(extra)) {
  spp_i <- extra$species_code[i]
  n_i <- extra$needed[i]

  tags_i <- tags_use %>%
    dplyr::filter(
      species_code == spp_i,
      is.na(tag_rating),
      recording_id %in% recordings_include$recording_id
    )

  tags_ver <- tags_i %>%
    dplyr::filter(tag_is_verified == TRUE)

  if (nrow(tags_ver) > 0) {
    n2_i <- n_i - nrow(tags_ver)

    if (n2_i < 1) {
      extra_list[[i]] <- tags_ver %>%
        sample_n(n_i, replace = FALSE)
    } else {
      set.seed(i)
      extra_list[[i]] <- tags_i %>%
        sample_n(n2_i, replace = FALSE) %>%
        rbind(tags_ver)
    }
  } else {
    set.seed(i)
    extra_list[[i]] <- tags_i %>%
      sample_n(n_i, replace = FALSE)
  }
}

tags_extra <- do.call(rbind, extra_list)

################################################################################
# 8. Put together ----
# Remove unknowns, non-target taxa, and species with more than a four-letter
# code.
tags_rating <- rbind(tags_select, tags_extra) %>%
  left_join(
    recordings_use %>%
      dplyr::select(recording_id, latitude, longitude, bcr)
  ) %>%
  inner_join(
    wildrtrax::wt_get_species() %>%
      dplyr::filter(
        str_sub(species_common_name, 1, 12) != "Unidentified",
        str_length(species_code) == 4,
        species_class %in% c("AVES", "AMPHIBIA", "MAMMALIA")
      ) %>%
      dplyr::select(species_code, species_class)
  )

################################################################################
# 9. Look at selection ----
ggplot() +
  geom_point(
    data = tags_rating,
    aes(x = longitude, y = latitude, colour = rms_peak_dbfs)
  ) +
  geom_point(
    data = rated,
    aes(x = longitude, y = latitude),
    colour = "grey",
    size = 0.5
  )

hist(tags_rating$rms_peak_dbfs)

################################################################################
# 10. Save ----
write.csv(tags_rating, args[2], row.names = FALSE)

