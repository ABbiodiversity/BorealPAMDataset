# Boreal PAM Dataset

This repository contains the scripts used to download, select, quality-check,
and tidy the passive acoustic monitoring data described in the associated
[dataset manuscript](https://docs.google.com/document/d/1ZG4OdennJSf9ZrQmj0AhPJ9cC0zT-FSMCJ6fw5fz9RQ/edit?tab=t.0#heading=h.bxn1zgca8i1y).

The workflow assembles recordings and annotations from WildTrax, applies the
temporal and spatial selection criteria used for the dataset, selects tags for
manual verification and rating, applies the resulting QA/QC decisions, prepares
the final metadata, and processes the selected audio for release.

## Workflow

The scripts are numbered in their intended order:

| Step | Script | Purpose |
|---:|---|---|
| 1 | `00_download_wildtrax.R` | Download and combine the required WildTrax reports. |
| 2 | `01_prepare_recordings.R` | Prepare recording metadata and apply temporal and spatial eligibility rules. |
| 3 | `02_select_recordings.R` | Sample and spatially thin recordings for the dataset. |
| 4 | `03_select_verification_tags.R` | Use eBird and HawkEars information to select tags for manual verification. |
| 5 | `04_select_rating_tags.R` | Select tags for manual quality rating. |
| 6 | `05_prepare_manual_review.R` | Prepare media and tracking tables for manual review. |
| 7 | `06_finalize_dataset.R` | Apply reviews, exclusions, and taxonomic harmonization to create the final dataset. |
| 8 | `07_make_tables_figures.R` | Produce manuscript summaries, tables, and the dataset map. |
| 9 | `08_download_audio.R` | Download the selected recordings. |
| 10 | `09_process_audio.py` | Clip recordings and create the FLAC files used for release. |

The required command-line arguments are documented at the top of each script.
The two recording-selection inputs used historically (the main collection and
CWS-PRA collection) are processed by the same consolidated selection script.

## External inputs and outputs

This is a code-only repository. It does not contain credentials, private
database connections, source data, manual-review files, downloaded recordings,
or generated outputs. These are supplied from external locations when the
scripts are run, and output locations are passed explicitly to the scripts.

Running the complete workflow requires access to the relevant WildTrax
projects and the external project-selection, spatial, eBird, HawkEars,
manual-review, exclusion, and manuscript-metadata files. Audio processing also
requires Python, FFmpeg, and FFprobe.

The scripts preserve the analytical and sampling decisions used to construct
the published dataset. They are retained primarily as a transparent record of
the data-processing workflow rather than as a turnkey software package.

## Citation

Please cite the associated dataset manuscript and data publication when using
this workflow or the resulting dataset. Full citation details will be added
when available.
