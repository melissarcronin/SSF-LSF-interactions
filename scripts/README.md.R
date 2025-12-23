# # SSF–LSF Interactions & SIFI Index
# 
# This repository contains the data processing, index construction, and analysis code for the **Small-scale and Industrial Fisheries Interaction (SIFI) Index**, which quantifies country-level vulnerability to interactions between small-scale fisheries (SSFs) and large-scale / industrial fisheries (LSFs).
# 
# The SIFI framework integrates indicators of **exposure**, **sensitivity**, and **adaptive capacity** to examine how industrial fishing pressure intersects with social and governance conditions across countries.
# 
# ---
#   
#   ## Repository Structure
#   
#   SSF-LSF_interactions/
#   ├── SSF-LSF_interactions.Rproj
# ├── README.md
# ├── .gitignore
# │
# ├── data/
#   │ ├── raw/ # Raw, unprocessed input data
#   │ ├── processed/ # Cleaned and standardized datasets
#   │
# ├── scripts/
#   │ ├── 01-data_cleaning_sensitivity_analyses.R
# │ ├── 02_Construct_SIFI_Index_and_visualization.R
# │ └── 03-Bright_spots_analysis.R
# │
# ├── outputs/
#   │ ├── figures/ # Generated figures
#   │ └── data_tables/ # Summary tables and derived outputs
#   │
# 
# ---
#   
#   ## Workflow Overview
#   
#   The analysis is designed to be run **sequentially**, using numbered scripts:
#   
#   1. **`01-data_cleaning_sensitivity_analyses.R`**  
#   Cleans and standardizes all input data used in the SIFI Index, including exposure, sensitivity, and adaptive capacity indicators.
# 
# 2. **`02_Construct_SIFI_Index_and_visualization.R`**  
#   Constructs the SIFI Index, performs scaling and aggregation, and generates core visualizations.
# 
# 3. **`03-Bright_spots_analysis.R`**  
#   Identifies and analyzes “bright spots” and patterns in vulnerability, exposure, and governance capacity.
# 
# Scripts assume the working directory is set to the project root (via the `.Rproj` file) and use relative paths throughout.
# 
# ---
#   
#   ## Reproducibility Notes
#   
# - All scripts rely on relative paths.
# - SSF catch data is not shareable per data privacy specifications (see Supplementary Information). Thus the SIFI Index cannot be calculated from raw data.
# - Raw data are stored separately from processed outputs.
# - Large or reproducible outputs (e.g., figures) may be excluded from version control depending on `.gitignore` settings.
# 
# ---
#   
#   ## Author
#   
#   **Melissa Cronin**  
#   University of Massachusetts Dartmouth  
#   Shared Seas Lab
# 
# ---
#   
#   ## License & Use
#   
#   This repository is intended for research and academic use.  
# Please contact the author before reuse or redistribution of data products.
