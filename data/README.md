# Data Directory

This directory contains raw feeding and drinking data files collected from precision livestock farming systems (e.g., Insentec).

## Data File Formats

### Feeder Files

- **File pattern:** `VR*.DAT`
- **Description:** Raw feeder visit data files from Insentec feeding systems
- **Location:** Place feeder files in a subdirectory (e.g., `data/insentec/`)

### Drinker Files

- **File pattern:** `VW*.DAT`
- **Description:** Raw drinker visit data files from Insentec drinking systems
- **Location:** Place drinker files in the same subdirectory as feeder files, or in a separate directory if needed

## Directory Structure

```
data/
└── insentec/          # Example subdirectory (create your own)
    ├── VR*.DAT        # Feeder files
    └── VW*.DAT        # Drinker files
```

## Notes

- Raw data files are typically not included in version control (see `.gitignore`)
- Ensure data files follow the expected naming convention (`VR*.DAT` for feeders, `VW*.DAT` for drinkers)
- Data files should be organized by date or in a flat structure as required by the `moo4feed` package
