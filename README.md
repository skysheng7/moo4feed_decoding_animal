# Decoding Cow Behaviours using `moo4feed` Package

**Authors**: Kehan (Sky) Sheng, Borbala Foris, Daniel Weary, Marina von Keyserlingk

## Overview

This repository contains a comprehensive data analysis workflow for extracting individual-level behavioral traits from raw feeding and drinking data collected through precision livestock farming systems. The project uses the [`moo4feed`](https://skysheng7.github.io/moo4feed/) R package to process and analyze feeding behavior data from dairy cattle.

The analysis pipeline transforms raw visit-level data into meaningful behavioral metrics including meal patterns, social interactions, feed availability preferences, and dominance relationships. This workflow supports animal welfare research and data-driven monitoring by enabling reproducible, scalable analysis of precision livestock farming data.

## Project Structure

```
moo4feed_decoding_animal/
├── scripts/                    # Analysis scripts (run sequentially)
│   ├── 1_data_cleaning.r       # Data cleaning and quality control
│   ├── 2_meal_clustering.r     # Meal identification and clustering
│   ├── 3_bin_visit_analysis.r # Bin exploration patterns
│   ├── 4_replacement_detection.r # Social replacement/displacement events
│   ├── 5_synchronicity_analysis.r # Pair-wise feeding synchronicity
│   ├── 6_non_nutritive_visit_analysis.r # Non-nutritive and empty bin visits
│   ├── 7_feed_availability_analysis.r # Feed availability calculations
│   └── 8_meal_level_behavior_analysis.r # Meal-level behavioral metrics
├── results/                    # Output data files and visualizations
│   ├── 1_data_cleaning/        # Output files from data cleaning
│   ├── 2_meal_clustering/      # Output files from meal clustering
│   ├── 3_bin_visit_analysis/   # Output files from bin visit analysis
│   ├── 4_replacement_detection/ # Output files from replacement detection
│   ├── 5_synchronicity_analysis/ # Output files from synchronicity analysis
│   ├── 6_non_nutritive_visit_analysis/ # Output files from non-nutritive visit analysis
│   ├── 7_feed_availability_analysis/ # Output files from feed availability analysis
│   └── 8_meal_level_behavior_analysis/ # Output files from meal-level behavior analysis
│   └── README.md               # Detailed documentation of all results
├── data/                       # Raw data files (not included in repository)
|   └── insentec/               # Subdirectory for Insentec data files
|       ├── VR*.DAT            # Feeder files
|       └── VW*.DAT            # Drinker files
│   └── README.md               # Data directory documentation
├── renv/                       # R environment management (renv package)
├── renv.lock                   # Lock file for reproducible R package versions
├── .Rprofile                   # R profile that activates renv on startup
├── .gitignore                  # Git ignore rules
├── LICENSE                     # MIT License
└── README.md                   # This file
```

## Installation

### Prerequisites

1. **R** (version 4.0 or higher)
2. **RStudio** (recommended) or another R IDE

### Required R Packages

This project uses [`renv`](https://rstudio.github.io/renv/) for reproducible package management. When you first open this project:

1. **Restore the R environment**:
   ```r
   # If renv is not installed
   install.packages("renv")
   
   # Restore all packages from renv.lock
   renv::restore()
   ```

This will install all required packages (including `moo4feed` and its dependencies) at the exact versions specified in `renv.lock`, ensuring reproducible results.

**Note:** The `.Rprofile` file will automatically activate renv when you open R in this project directory.

## Usage

### Running the Analysis

Run the scripts sequentially in order (1 through 8). Each script builds upon outputs from previous steps:

1. **Data Cleaning** (`1_data_cleaning.r`)
   - Cleans raw visit data
   - Performs quality control checks
   - Generates daily summaries
   - Outputs: `clean_feed.rda`, `clean_water.rda`, `summary_df.csv`, `warnings.csv`

2. **Meal Clustering** (`2_meal_clustering.r`)
   - Identifies meals using Gaussian Mixture Model (GMM) gap analysis
   - Groups individual visits into meal events
   - Outputs: `meal_summaries.csv`, `labeled_visits.rda`, meal visualizations

3. **Bin Visit Analysis** (`3_bin_visit_analysis.r`)
   - Analyzes unique bin exploration patterns
   - Outputs: `bin_visits.csv`

4. **Replacement Detection** (`4_replacement_detection.r`)
   - Detects social replacement/displacement events
   - Identifies actor-reactor relationships
   - Outputs: `all_replacements.csv`, `replacements.rda`

5. **Synchronicity Analysis** (`5_synchronicity_analysis.r`)
   - Calculates pair-wise feeding/drinking synchronicity
   - Analyzes neighbor proximity preferences
   - Outputs: Pair-wise synchronicity matrices and summaries

6. **Non-Nutritive Visit Analysis** (`6_non_nutritive_visit_analysis.r`)
   - Identifies visits with no or minimal intake
   - Detects empty bin visits
   - Outputs: Non-nutritive and empty bin visit summaries

7. **Feed Availability Analysis** (`7_feed_availability_analysis.r`)
   - Detects feed addition events
   - Calculates feed availability percentages at each visit
   - Outputs: Feed addition events and availability metrics

8. **Meal-Level Behavior Analysis** (`8_meal_level_behavior_analysis.r`)
   - Aggregates behavioral metrics at the meal level
   - Calculates actor/reactor role percentages
   - Generates dominance summaries
   - Outputs: Meal-level behavioral summaries and dominance metrics

## Results Documentation

Detailed documentation of all output files is available in [`results/README.md`](results/README.md). This includes:

- File descriptions and structures
- Column definitions
- Data formats and units
- Usage examples

## Development

This project was developed using AI-assisted tools including Cursor and Claude Code. System prompts and development configurations can be found in the `.cursor` and `.claude` directories.

The project uses `renv` for reproducible R package management. To contribute:

1. Ensure renv is activated: `renv::activate()`
2. Install any new dependencies: `renv::install("package_name")`
3. Update the lock file: `renv::snapshot()`

## Documentation

For detailed tutorials and package documentation, visit the [`moo4feed` package website](https://skysheng7.github.io/moo4feed/).

## Contributors

- **Principal Investigator:** Marina von Keyserlingk  
  - ORCID: 0000-0002-1427-3152  
  - Affiliation: University of British Columbia  
  - Email: <nina@mail.ubc.ca>

- **Co-Investigator:** Daniel Weary  
  - ORCID: 0000-0002-0917-3982  
  - Affiliation: University of British Columbia  
  - Email: <dan.weary@ubc.ca>

- **Contributor:** Kehan Sheng  
  - ORCID: 0000-0001-6442-5284  
  - Affiliation: University of British Columbia  
  - Email: <skysheng7@gmail.com>

- **Contributor:** Borbala Foris  
  - ORCID: 0000-0002-0901-3057  
  - Affiliation while working on this project: University of British Columbia
  - Current affiliation: University of Veterinary Medicine, Vienna
  - Email: <forisbori@gmail.com>

## Acknowledgements

This analysis workflow was developed following the instructions and recommended workflows outlined in several key resources: 

- [*R Packages*](https://r-pkgs.org/) by Hadley Wickham and Jenny Bryan  
- [*Reproducible and Trustworthy Workflows for Data Science*](https://ubc-dsci.github.io/reproducible-and-trustworthy-workflows-for-data-science/) by Tiffany Timbers, Joel Ostblom, and Florencia D'Andrea  
- Courses: [DSCI 522 Data Science Workflows](https://ubc-mds.github.io/course-descriptions/DSCI_522_dsci-workflows/), [DSCI 524 Collaborative Software Development](https://ubc-mds.github.io/DSCI_524_collab-sw-dev/README.html) by Dr. Tiffany Timbers

## Project Information

- **Funding:** This project is funded by a Natural Sciences and Engineering Research Council (NSERC) Discovery Grant (RGPIN-2021-02848; Ottawa, ON, Canada) awarded to MvK. KS also received funding from the Pei-Huang Tung and Tan-Wen Tung Graduate Fellowship (Vancouver, BC, Canada), Elizabeth R. Howland Fellowship (Vancouver, BC, Canada), Wilson Henderson Fellowship (Vancouver, BC, Canada), Hugo E Meilicke Memorial Fellowship (Vancouver, BC, Canada), and Mary and David Macaree Fellowship (Vancouver, BC, Canada).
