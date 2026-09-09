# Cervical joint position error and neck pain in hospital nurses

Analysis code for

> Quesada D, Ernst MJ, Degenfellner J, Pfeiffer F.
> *Association between cervical joint position error and neck pain in hospital nurses: An exploratory cross-sectional study.*
> Zurich University of Applied Sciences, Institute of Physiotherapy, and Spital Schwyz. Manuscript, 2026.

## Contents

| File | Purpose |
|---|---|
| `0_analysis.R` | Main analysis: conversion of joint position error (JPE) from cm to degrees, descriptive statistics (Table 1), correlogram, best-subset model selection with BIC (`leaps`/`bestglm`), linear regression of neck pain intensity (NRS) and logistic regression of neck pain (yes/no), regression tables as Word documents. |
| `1_reproduce_manuscript_numbers.R` | Self-contained check that recomputes every number printed in the manuscript from the data file and compares it at the printed precision. |
| `sessionInfo.txt` | R version and package versions used for the analysis. |
| `DATA/` | Placeholder for the participant-level data file, which is not distributed (see `DATA/README.md`). |

## Running the code

1. Put `schwyz_data_Doris2026.csv` into `DATA/`.
2. Open a script in RStudio and source it. The working directory is set to the
   script location automatically. From the command line, `cd` into the
   repository first and run `Rscript 0_analysis.R`.

Package installation is handled through `pacman::p_load()` at the top of
`0_analysis.R`. The analysis was run with R 4.6.0; exact package versions are
listed in `sessionInfo.txt`.

## Outputs

`0_analysis.R` writes `Table1_Output.docx`, `Table_NRS_linear_model.docx`,
`Table_neckpain_logistic_model.docx` and `Correlogram_JPE.pdf` into the
repository folder. These files are ignored by git.

## Data

The data cannot be shared publicly. Requests should be sent to the
corresponding author, Fabian Pfeiffer (fabian.pfeiffer@zhaw.ch).

## License

MIT, see `LICENSE`.
