****# # SSF–LSF Interactions & SIFI Index 
# SSF–LSF Interactions / SIFI Index

Code used to construct and analyze the Small-scale and Industrial Fisheries Interaction (SIFI) Index, which quantifies country-level vulnerability to interactions between small-scale fisheries (SSFs) and large-scale / industrial fisheries (LSFs).

The index integrates indicators of exposure, sensitivity, and adaptive capacity.

## Repository structure


SSF-LSF_interactions/
├── SSF-LSF_interactions.Rproj
├── README.md
├── .gitignore
│
├── data/
│ ├── raw/ # Raw input data (restricted)
│ └── processed/ # Processed, shareable datasets
│
├── scripts/
│ ├── 01-data_cleaning_sensitivity_analyses.R
│ ├── 02_Construct_SIFI_Index_and_visualization.R
│ └── 03-Bright_spots_analysis.R
│
└── outputs/
├── figures/
└── data_tables/

## Workflow

Scripts are designed to be run sequentially:

1. `01-data_cleaning_sensitivity_analyses.R`  
   Cleans and standardizes all input data used in the SIFI Index.

2. `02_Construct_SIFI_Index_and_visualization.R`  
   Constructs the SIFI Index and generates core visualizations.

3. `03-Bright_spots_analysis.R`  
   Identifies and analyzes patterns in vulnerability and governance capacity.

Scripts assume the working directory is set to the project root (via the `.Rproj` file) and use relative paths throughout.

## Data notes

Raw small-scale fisheries catch data are not publicly shareable due to data privacy agreements (see Supplementary Information).  
The repository includes a processed, country-level SSF catch dataset sufficient to reproduce all analyses.

## Author

Melissa R. Cronin  
University of Massachusetts Dartmouth  
Shared Seas Lab

## License

This repository is intended for research and academic use.  
Please contact melissa.cronin@umassd.edu before reuse or redistribution of data products.
