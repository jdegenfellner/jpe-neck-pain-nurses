# Cervical joint position error and neck pain in hospital nurses

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22674559.svg)](https://doi.org/10.5281/zenodo.22674559)

Analysis code for

> Quesada D, Ernst MJ, Degenfellner J, Pfeiffer F.
> *Association between cervical joint position error and neck pain in hospital nurses: An exploratory cross-sectional study.*
> Zurich University of Applied Sciences, Institute of Physiotherapy, and Spital Schwyz. Manuscript, 2026.

## Contents

| File | Purpose |
|---|---|
| `run_all.R` | Runs everything below in one go and stops if any number in the manuscript is not reproduced. |
| `0_analysis.R` | Main analysis: sample size calculation, conversion of joint position error (JPE) from cm to degrees, Table 1 (participant characteristics), Table 2 (JPE measurements), correlogram, best-subset model selection with BIC (`leaps`/`bestglm`), linear regression of neck pain intensity (NRS, Table 3) and logistic regression of neck pain (yes/no, Table 4), all tables as Word documents. |
| `1_reproduce_manuscript_numbers.R` | Self-contained check that recomputes every number printed in the manuscript (text and Tables 1-4) from the data file and compares it at the printed precision. |
| `sessionInfo.txt` | R version and package versions used for the analysis. |
| `DATA/` | Placeholder for the participant-level data file, which is not distributed (see `DATA/README.md`). |

## Running the code

1. Put `schwyz_data_Doris2026.csv` into `DATA/`.
2. From the command line, `cd` into the repository and run `Rscript run_all.R`.
   Alternatively open `run_all.R` in RStudio and source it; the working
   directory is set to the script location automatically.

Package installation is handled through `pacman::p_load()` at the top of
`0_analysis.R`. The analysis was run with R 4.6.0; exact package versions are
listed in `sessionInfo.txt`.

## Outputs

`run_all.R` writes `Table1_Output.docx`, `Table2_Output.docx`,
`Table_NRS_linear_model.docx`, `Table_neckpain_logistic_model.docx`,
`Correlogram_JPE.pdf`, `sessionInfo.txt` and `reproduction_check.txt` into the
repository folder. Except for `sessionInfo.txt` these files are ignored by git.

## Data

The data cannot be shared publicly. Requests should be sent to the
corresponding author, Fabian Pfeiffer (fabian.pfeiffer@zhaw.ch).

## Citation

Archived on Zenodo. Concept DOI (always resolves to the latest version):
[10.5281/zenodo.22674559](https://doi.org/10.5281/zenodo.22674559).
Version v1.0.0: [10.5281/zenodo.22674560](https://doi.org/10.5281/zenodo.22674560).

## License

MIT, see `LICENSE`.
