# Data Directory

This directory contains raw feeding and drinking data files collected from precision livestock farming systems (e.g., Insentec), as well as metadata files used for analysis.

## Raw Data Files

### Feeder Files

- **File pattern:** `VR*.DAT`
- **Description:** Raw feeder visit data files from Insentec feeding systems
- **Location:** `data/insentec/` subdirectory
- **Usage:** These files are processed by `scripts/1_data_cleaning.r` to create cleaned feeding data

### Drinker Files

- **File pattern:** `VW*.DAT`
- **Description:** Raw drinker visit data files from Insentec drinking systems
- **Location:** `data/insentec/` subdirectory
- **Usage:** These files are processed by `scripts/1_data_cleaning.r` to create cleaned drinking data

## Metadata Files (.rdata)

**Note:** All metadata files (`.rdata` files) in this directory are processed data from the repository: [competition_dominance_analysis](https://github.com/skysheng7/competition_dominance_analysis.git). These files were generated as part of the published research:

- **Title:** Redefining dominance calculation: Increased competition flattens the dominance hierarchy in dairy cows
- **Paper DOI:** [https://doi.org/10.3168/jds.2023-24587](https://doi.org/10.3168/jds.2023-24587)
- **Dataset DOI:** [https://doi.org/10.5683/SP3/HT9EHX](https://doi.org/10.5683/SP3/HT9EHX)

### `regrouping.rdata`

- **Description:** Records of regrouping events when cows were moved between pens
- **Structure:** Data frame with columns:
  - `date`: Date of regrouping event (POSIXct)
  - `cow_num`: Number of cows in the group
  - `cow_list`: Semicolon-separated list of cow IDs
  - `excluded_num`: Number of cows excluded
  - `excluded_cow`: Semicolon-separated list of excluded cow IDs
  - `enroll_num`: Number of cows enrolled
  - `enroll_cow`: Semicolon-separated list of enrolled cow IDs
  - `re_enrolled`: Cows that were re-enrolled
  - `Red_warning`: Red warning messages (e.g., "Insentec break down")
  - `orange_warning`: Orange warning messages
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to identify and filter regrouping days

### `warning_days.rdata`

- **Description:** Days with warnings indicating data quality issues or system problems
- **Structure:** Data frame (tibble) with columns:
  - `date`: Date of warning (character, format: "YYYY-MM-DD")
  - `Red_warning`: Red warning messages (e.g., "Trial not started", "Insentec break down")
  - `orange_warning`: Orange warning messages
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to filter out problematic days from analysis

### `parlor.rdata`

- **Description:** Parlor/milking data including milk production, DIM (Days In Milk), parity, and reproduction status
- **Structure:** Data frame containing milking records with cow identification, dates, and production metrics
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to:
  - Calculate DIM (Days In Milk) and parity information
  - Extract milk production data
  - Determine reproduction status (pregnant, bred, fresh, etc.)

### `cows_in_heat.rdata`

- **Description:** Records of cows detected in estrus/heat
- **Structure:** Data frame with cow identification and dates when cows were in heat
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to filter out days when cows were in heat, as this can affect feeding behavior

### `lameness_database.rdata`

- **Description:** Gait scoring and lameness records for cows
- **Structure:** Data frame containing gait scores (GS) and lameness assessments
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to:
  - Update gait scores based on notes
  - Identify lame cows (GS > 2.5 for extended periods)
  - Label days before cows become lame
  - Filter out lame cows and 7 days before lameness onset

### `thi.rdata`

- **Description:** Temperature-Humidity Index (THI) data for environmental stress assessment
- **Structure:** Data frame with columns:
  - `date`: Date of measurement (Date format)
  - `temperature(C)_mean`: Mean temperature in Celsius
  - `temperature(C)_standard_deviation`: Standard deviation of temperature
  - `temperature(C)_min`: Minimum temperature
  - `temperature(C)_max`: Maximum temperature
  - `relative_humidity(%)_mean`: Mean relative humidity percentage
  - `relative_humidity(%)_standard_deviation`: Standard deviation of relative humidity
  - `relative_humidity(%)_min`: Minimum relative humidity
  - `relative_humidity(%)_max`: Maximum relative humidity
  - `THI_mean`: Mean Temperature-Humidity Index
  - `THI_standard_deviation`: Standard deviation of THI
  - `THI_min`: Minimum THI
  - `THI_max`: Maximum THI
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to assess environmental stress conditions that may affect feeding behavior

### `sick_cow_no_lame.rdata`

- **Description:** Records of sick cows excluding lameness cases
- **Structure:** Data frame with cow identification and dates of sickness
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to identify sickness periods (7 days before and after recorded sickness) and filter out these periods from analysis

### `enroll_exclude_track.rdata`

- **Description:** Tracking of cow enrollment and exclusion dates
- **Structure:** Data frame with columns:
  - `Cow`: Cow ID (numeric)
  - `date`: Date of enrollment or exclusion (POSIXct)
  - `entry_exit_status`: Status indicator ("enroll" or "exclude")
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to filter out entry and exit days for each cow

### `master_feed_replacement_all_with_feeder_occupancy.rdata`

- **Description:** Feed replacement events with feeder occupancy information
- **Structure:** Data frame containing feed replacement records with columns including:
  - `date`: Date of replacement event (Date format)
  - `feeder_occupancy`: Feeder occupancy metric (renamed from `resource_occupancy`)
  - Additional columns related to feed replacement events (time, bout_interval, etc.)
- **Usage:** Used in `scripts/9_filter_problematic_days.r` to:
  - Calculate dominance hierarchy based on replacements
  - Filter replacements by feeder occupancy (≤0.75 for low-medium occupancy)
  - Exclude red warning days and specific orange warning days from dominance calculations

## Directory Structure

```
data/
├── README.md                    # This file
├── insentec/                    # Raw data files directory
│   ├── VR*.DAT                  # Feeder files
│   └── VW*.DAT                  # Drinker files
├── regrouping.rdata             # Regrouping events
├── warning_days.rdata           # Data quality warnings
├── parlor.rdata                 # Milking and production data
├── cows_in_heat.rdata           # Estrus detection records
├── lameness_database.rdata      # Gait scoring and lameness data
├── thi.rdata                    # Temperature-Humidity Index data
├── sick_cow_no_lame.rdata       # Sickness records (non-lameness)
├── enroll_exclude_track.rdata   # Cow enrollment/exclusion tracking
└── master_feed_replacement_all_with_feeder_occupancy.rdata  # Feed replacement events with feeder occupancy
```

## Data Processing Pipeline

1. **Raw Data Cleaning** (`scripts/1_data_cleaning.r`):
   - Processes `VR*.DAT` and `VW*.DAT` files from `insentec/` directory
   - Outputs cleaned data to `results/1_data_cleaning/`

2. **Data Filtering** (`scripts/9_filter_problematic_days.r`):
   - Uses all `.rdata` files in this directory to filter out problematic days
   - Filters based on:
     - Regrouping events
     - Warning days (red/orange warnings)
     - Cow entry/exit dates
     - Lameness periods
     - Sickness periods
     - Cows in heat
     - Reproduction status
     - Environmental conditions (THI)
   - Outputs filtered dataset to `results/9_filter_problematic_days/`

## Notes

- Raw data files (`VR*.DAT`, `VW*.DAT`) are typically not included in version control (see `.gitignore`)
- Processed `.rdata` files are tracked in version control
- Ensure data files follow the expected naming convention (`VR*.DAT` for feeders, `VW*.DAT` for drinkers)
- Data files should be organized by date or in a flat structure as required by the `moo4feed` package
- All dates in `.rdata` files use timezone "America/Los_Angeles" or "America/Vancouver" as specified in the scripts
