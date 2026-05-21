---
output: github_document
editor_options: 
  markdown: 
    wrap: 72
---

# README
This repo contains code used for data cleaning, analysing and reporting levels of self-reported leisure-time physical activity
in the IRONMAN study overall and by race and country region groups (see Solberg et al. 2026 for for details). 

Github repo contains no actual study data or output due to data privacy. Repo is intended for demonstrating data management and analytical abilities. Rest of the readme lists instructions for use when shared with colleagues working on the same project. 

The project contains scripts for:
1. Data cleaning of all relevant study variables
2. Descriptive data for baseline variables and physical activity levels at different timepoints
3. Logistic mixed effects models analysing odds of meeting guidelines in different subgroups
4. Sensitivity analyses testing the robustness of the findings

Justification of methods is described in article. 

## 📁 Project Setup

To view and run the analyses, follow these steps:

1.  Open the `IM_PA_study.Rproj` project from the project folder in RStudio. The environment is set up using the renv package to manage dependencies and relative file paths within the project root folder using the here package to enable code sharing. 

2.  Install the
    [`renv`](https://cran.r-project.org/web/packages/renv/index.html)
    package and restore the environment using the following code:

``` r
install.packages("renv")
library(renv)
renv::restore()
```
    This will automatically install all relevant packages used in the          analyses within the project folder.

3.  Manually move the following files into the `data/raw` directory within the main project folder:

-   `2025-02-24_proms.csv`
-   `2025-02-24_proms_dictionary.xlsx`
-   `subject.csv`
-   `psa.csv`
-   `vs.csv`
-   `cohort.csv`
-   `dm.csv`
-   `ca_cm.csv`

## 📜 Running the Scripts

All scripts use relative file paths via the
[`here`](https://here.r-lib.org/) package. As long as the files are
correctly placed in `data/raw`, the scripts should run without
modification. Processed files will be saved to `data/processed`.

To run the analysis:
1.  Execute the script `scripts/0_master_script.R` to run all scripts from 
    cleaning to analyses. See individual scripts for details about each        step. 
    The `scripts/0_master_script.R` will run all scripts sequentially, from     data cleaning and preprocessing to final analyses. The output will be      saved in the `data/processed` folder.


## ⚠️ Important Notes

-   The analyses are based on IRONMAN data received on **26.02.2025**.
    If the scripts are run on updated datasets, results may differ.
-   In case of updated data, some processing steps may not capture all         possible categorical responses (e.g., treatment types).
    These should be manually reviewed to ensure completeness.

## 📬 Contact

If you encounter any issues with running the code, or have any questions about the analysis steps, please feel free to reach out to:

**Aleksander Solberg**\
📧 [aleksandso\@nih.no](mailto:aleksandso@nih.no)
