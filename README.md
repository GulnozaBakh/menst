# Town-Level Data Analysis – Nepal 🇳🇵

This repository contains R scripts for preprocessing, spatial harmonization, rainfall data integration, and analysis of town-level datasets in Nepal.

---

## 🚦 Script Execution Order

Please follow the sequence below for accurate data processing and analysis.

### 1️⃣ `data_preprocess.R`
- Prepares the **initial town data**.
- Performs basic **data cleaning** and formatting.

### 2️⃣ `study_data_spatial_mappers.R` *(Run together with `data_preprocess.R`)*
> ⚠️ **Important:** Run this script **alongside** `data_preprocess.R` before proceeding. It standardizes names used in all downstream processes.

- Ensures **consistent naming** of **districts, provinces, and municipalities** across all datasets.
- Harmonizes spatial and tabular data.

---

### 3️⃣ `rainfall_preprocess.R`
- Processes and cleans **rainfall data**.
- Merges rainfall data with the **main dataset** generated earlier.

---

### 4️⃣ `data_analysis.R`
- Performs **data analysis** including:
  - **Distance calculations** (in km)
  - **Statistical summaries** (mean, standard deviation, coefficient of variation)

---

## 📂 File Descriptions

| File                         | Description                                                  |
|-----------------------------|--------------------------------------------------------------|
| `data_preprocess.R`         | Initial town data cleaning and preparation                   |
| `study_data_spatial_mappers.R` | Standardizes spatial names across datasets               |
| `rainfall_preprocess.R`     | Processes rainfall data and merges it with town data         |
| `data_analysis.R`           | Conducts analysis (distances, stats, summaries)              |

---

## 🔧 Requirements
- **R** (version >= 4.x)
- Key R packages: `sf`, `geosphere`, `terra`, `dplyr`, `readr`, `ggplot2` `readxl`, `purrr`



