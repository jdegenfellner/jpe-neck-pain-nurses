# ============================================================================
# run_all.R: reproduces every result of the manuscript in one go.
#
# Requires the (not public) data file ./DATA/schwyz_data_Doris2026.csv.
# Run from this folder:
#   Rscript run_all.R
# or open in RStudio and source it.
#
# Steps
#   1. 0_analysis.R                    sample size, Table 1, Table 2, Table 3
#                                      (linear model), Table 4 (logistic model),
#                                      correlogram, sessionInfo.txt
#   2. 1_reproduce_manuscript_numbers.R compares every number printed in the
#                                      manuscript with a recomputation and
#                                      stops if any number differs
#
# Outputs (written to this folder)
#   Table1_Output.docx, Table2_Output.docx, Table_NRS_linear_model.docx,
#   Table_neckpain_logistic_model.docx, Correlogram_JPE.pdf, sessionInfo.txt,
#   reproduction_check.txt
# ============================================================================

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!file.exists("./DATA/schwyz_data_Doris2026.csv"))
  stop("Data file ./DATA/schwyz_data_Doris2026.csv not found. ",
       "The data are not public (see Availability of data and materials).")

analysis_script <- if (file.exists("0_analysis.R")) "0_analysis.R" else "0_NEW_R_CODE.R"
check_script    <- if (file.exists("1_reproduce_manuscript_numbers.R"))
  "1_reproduce_manuscript_numbers.R" else "1_REPRODUCE_MANUSCRIPT_NUMBERS.R"

cat("== Step 1:", analysis_script, "==\n")
pdf(NULL)  # exploratory plots of the analysis script are not saved
source(analysis_script, echo = FALSE)
dev.off()

cat("== Step 2:", check_script, "==\n")
check_env <- new.env()
sink("reproduction_check.txt", split = TRUE)
source(check_script, local = check_env, echo = FALSE)
sink()

n_bad <- sum(check_env$out$OK == "NEIN")
if (n_bad > 0)
  stop(n_bad, " manuscript number(s) differ from the recomputation, ",
       "see reproduction_check.txt")
cat("\nAll", nrow(check_env$out), "manuscript numbers reproduced.\n")
