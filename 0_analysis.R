# outcome: NRS, neck pain yes/no
# explanatory variables: AE, VE, CE, headache, age, sex, bmi, stress

# Set working directory to source file location (in RStudio). When run via
# run_all.R or Rscript, the working directory is already R_Codes_and_DATA/.
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable() && sys.nframe() == 0)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pacman) 
pacman::p_load(data.table,
               tidyverse,
               GGally,
               readxl,
               writexl,
               pdftools,
               magick,
               tidyverse,
               naniar,
               GGally,
               performance,
               BiocManager,
               corrgram,
               DataExplorer,
               aspace,
               visdat,
               leaps,
               table1,
               Hmisc,
               bestglm,
               tictoc,
               car,
               flextable,
               officer,
               pwr)
#BiocManager::install("EBImage")

# SAMPLE SIZE----------
# Manuscript (Methods, "Sample size"): multiple linear regression for NRS with
# five predictors, R^2 = 0.30 (Cohen's f^2 = R^2 / (1 - R^2)), alpha = 0.05,
# power = 0.8 -> 36 participants.
ss <- pwr.f2.test(u = 5, f2 = 0.30 / (1 - 0.30), sig.level = 0.05, power = 0.8)
(n_required <- ceiling(ss$v) + 5 + 1)  # n = v + u + 1 = 36

# READ----------
df <- read.csv("./DATA/schwyz_data_Doris2026.csv",
               dec = ".", 
               sep = ";", 
               header = TRUE, stringsAsFactors = TRUE)

# JPE: cm -> DEGREES ------------------------------------------------------
# Documents how the AE/CE/VE columns in schwyz_data_Doris2026.csv were derived
# from the raw target coordinates. The manuscript (Methods, "Statistical
# analysis") describes exactly this step; it was originally done in a
# spreadsheet, so it is reproduced here in code.
#
# Raw input: for each direction and each of the 3 repetitions, the deviation
# from the neutral head position was read off the target in cm (signed,
# according to the Cartesian coordinate system, see Figure 2).
#   right/left rotation -> horizontal movement -> x-axis
#   flexion/extension   -> sagittal movement   -> y-axis
# Only the primary moving plane is used (see manuscript).
#
# Per direction, over the 3 repetitions:
#   AE = mean(|deviation|)                     absolute error
#   CE = mean(deviation)                       constant error (signed: over-/undershoot)
#   VE = sqrt(mean((deviation - CE)^2))        variable error (SD, divisor n = 3)
#
# The aggregate is formed in cm and converted afterwards, using the fixed
# 100 cm participant-target distance:
#   degrees = atan(cm / 100) * 180 / pi
# (Roren et al., 2009). NOTE: atan() is non-linear, so aggregating in cm and
# converting afterwards is not identical to converting each trial first; the
# order used here is the one behind the published numbers. The difference is
# < 0.1 degrees for the observed deviations.
#
# The raw coordinate file contains individual participant data and is NOT part
# of the Supplementary Material (see "Availability of data and materials").
# This block therefore only runs when the file is present locally.

cm_to_deg <- function(cm) atan(cm / 100) * 180 / pi

jpe_errors_cm <- function(trials) {           # trials: matrix, 3 columns
  ce <- rowMeans(trials)
  data.frame(AE = rowMeans(abs(trials)),
             CE = ce,
             VE = sqrt(rowMeans((trials - ce)^2)))   # divisor n = 3
}

raw_jpe_file <- "./DATA/z_old/jpe_doris2026.xlsx"

if (file.exists(raw_jpe_file)) {

  raw <- as.data.frame(readxl::read_excel(raw_jpe_file, sheet = "jpe_doris2026"))

  # direction -> (column prefix, axis of the primary moving plane, target names)
  directions <- list(
    rrot = list(prefix = "jpe_r", axis = "x"),
    lrot = list(prefix = "jpe_l", axis = "x"),
    flex = list(prefix = "jpe_f", axis = "y"),
    ext  = list(prefix = "jpe_e", axis = "y"))

  jpe_deg <- data.frame(participant_id = raw$ID)

  for (d in names(directions)) {
    cols   <- paste0(directions[[d]]$prefix, 1:3, directions[[d]]$axis)
    trials <- as.matrix(raw[, cols])
    err    <- jpe_errors_cm(trials)
    jpe_deg[[paste0("ae_", d)]] <- cm_to_deg(err$AE)
    jpe_deg[[paste0("ce_", d)]] <- cm_to_deg(err$CE)
    jpe_deg[[paste0("ve_", d)]] <- cm_to_deg(err$VE)
  }

  # CHECK: reproduce the AE/CE/VE columns that ship in schwyz_data_Doris2026.csv
  chk <- merge(df[, c("participant_id",
                      paste0(rep(c("ae_", "ce_", "ve_"), each = 4),
                             c("rrot", "lrot", "flex", "ext")))],
               jpe_deg, by = "participant_id", suffixes = c("", "_recomputed"))
  max_abs_dev <- max(abs(chk[, paste0(rep(c("ae_", "ce_", "ve_"), each = 4),
                                      c("rrot", "lrot", "flex", "ext"))] -
                         chk[, paste0(rep(c("ae_", "ce_", "ve_"), each = 4),
                                      c("rrot", "lrot", "flex", "ext"), "_recomputed")]))
  cat("JPE cm->degree check: max |deviation| =",
      format(max_abs_dev, digits = 2), "degrees (rounding only)\n")
  stopifnot(max_abs_dev < 0.01)

} else {
  cat("Raw JPE coordinate file not available -",
      "AE/CE/VE are used as supplied in schwyz_data_Doris2026.csv.\n")
}

# PREPARE----------
str(df)
# many variables are not factors yet!
# e.g., profession, age, sex,....

bin_cols <- c("neck_pain", "headache", "stress")
df[bin_cols] <- lapply(df[bin_cols], function(x)
  factor(x, levels = c(0, 1), labels = c("no", "yes")))
df$sex <- factor(df$sex, levels = c(1, 2), labels = c("male", "female"))
df$profession  <- factor(df$profession)
df$headachetype <- factor(df$headachetype)

# Check that everything is now a factor as intended
str(df[, c("profession", "sex", "neck_pain", "headache", "stress",
           "headachetype")])


# Missing values?----
vis_miss(df) # ndiscore and psqoverall have missings.
sum(is.na(df$ndiscore)) # 10 Personen haben NDI-Fragebogen (Nackenschmerz) nicht ausgefüllt, da sie angegeben haben, dass sie keine Nackenschmerzen haben
sum(is.na(df$psqoverall)) # 14 Personen haben PSQ-Fragebogen (Stress) nicht ausgefüllt, da sie angegeben haben, dass sie keinen Stress haben
summary(df$psqoverall) # range 13-48 

table(df$neck_pain) # 12 Personen haben keine Nackenschmerzen angegeben. 2 davon wurden jedoch auf den NDI-Fragebogen weitergeleitet, da sie aktuell 1/10 auf der NRS-Skala angegeben haben
table(df$stress) 
# D.h. das sind echte fehlende Werte. Wenn die Personen keinen Stress haben,
# könnte man tatsächlich 0 imputieren, weil man ja weiß, dass es eine 0 ist?

# Die Frage ist, welche Scores Personen mit "keinem Stress" hätten.
hist(df$psqoverall, breaks = 10) #Histogram Stress
plot(ecdf(df$psqoverall))

# Impute using knowledge (aktuell nicht in Anwendung)------------
# https://www.researchgate.net/publication/365678181_Assessing_medical_students%27_perceived_stress_levels_by_comparing_a_chatbot-based_approach_to_the_Perceived_Stress_Questionnaire_PSQ20_in_a_mixed-methods_study
# cutoff >50 clinically relevant stress

# EDA (Exploratory Data Analysis)------

# Correlogram---------
# Pairwise correlations / scatterplots of the 12 JPE measures (AE/CE/VE x
# right rot / left rot / extension / flexion).
jpe_vars <- c("ae_rrot", "ce_rrot", "ve_rrot",
              "ae_lrot", "ce_lrot", "ve_lrot",
              "ae_ext",  "ce_ext",  "ve_ext",
              "ae_flex", "ce_flex", "ve_flex")

corr_jpe <- ggpairs(df[, jpe_vars],
        title = "Correlogram of JPE measures (AE / CE / VE)",
        lower = list(continuous = wrap("points", alpha = 0.5)), # Scatterplots in lower triangle
        upper = list(continuous = wrap("cor", size = 3)),       # Correlation in upper triangle
        diag  = list(continuous = wrap("densityDiag"))) +       # Density plots on diagonal
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
corr_jpe
# iCloud Drive lässt das inkrementelle Schreiben grosser PDF-Devices scheitern
# ("Error in grDevices::dev.off() : write failed"). Deshalb erst in einen lokalen
# Temp-Ordner rendern und das fertige PDF anschliessend am Stueck herkopieren.
.tmp_pdf <- file.path(tempdir(), "Correlogram_JPE.pdf")
ggsave(.tmp_pdf, plot = corr_jpe, width = 12, height = 12)
file.copy(.tmp_pdf, "Correlogram_JPE.pdf", overwrite = TRUE)



# _Neck Pain----
df %>%
ggplot(aes(x = as.factor(neck_pain))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Subjects with Neck Pain", 
       x = "", 
       y = "Number of Subjects") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# _Neck Pain and NRS-----
summary(df$neck_pain_nrs) # 0-10, max 6.
df %>% 
  ggplot(aes(x = neck_pain, y = neck_pain_nrs)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, height = 0.1, 
              alpha = 0.5, color = "black") +
  labs(title = "Neck Pain and NRS Scores", 
       x = "", 
       y = "NRS Score") +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "red") +  # Hinzufügen einer Linie bei NRS = 0
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))  # center plot title
# -> It seems to me that this is perfectly separable at, say NRS=1.5? 
# i.e., everyone with NRS>1.5 has neck pain, and everyone with NRS<1.5 does not.
# i.e., using neck_pain in a logistic regression model will result in perfect classification.

#boxplot(neck_pain_nrs~neck_pain, data = df) #1 Ausreisser mit NRS 1/10 (statt 0/10) trotz "kein Nackenschmerz"

# _Neck Pain and Stress----
df %>%
  ggplot(aes(x = neck_pain, y = psqoverall)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
  labs(title = "Stress Levels by Neck Pain Status", 
       x = "", 
       y = "PSQ Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  

# It seems the variances are different. Exploratory test (do not overinterpret!):
var.test(psqoverall ~ neck_pain, data = df) # explainable by chance. (explorative...)

# Neck Pain and Stress
#table(df[df$neck_pain == "Neck Pain",]$stress)/sum(df$neck_pain == "Neck Pain")
#table(df[df$neck_pain == "No Neck Pain",]$stress)/sum(df$neck_pain == "No Neck Pain")

df %>%
  ggplot(aes(x = neck_pain, fill = as.factor(stress))) +
   geom_bar(position = "fill") +
   labs(title = "Stress Levels by Neck Pain Status",
       x = "",
       y = "Proportion",
       fill = "Stress") +
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5))  
# -> identical!

# _Neck Pain and Headache----
df %>%
  ggplot(aes(x = neck_pain, fill = factor(headachetype))) +
   geom_bar(position = "fill") +  # "fill" normalisiert auf 100% je Kategorie
   labs(title = "Distribution of Headache Type by Neck Pain Status",
       x = "",
       y = "Proportion",
       fill = "Headache Type") +
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5))  

fisher.test(table(df$headache, df$neck_pain)) # exploratory!!; explainable by chance.


# _NRS-----
table(df$neck_pain_nrs)

df %>%
  ggplot(aes(x = factor(1), y = neck_pain_nrs)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, color = "black") +
  labs(title = "Neck Pain NRS", x = "", y = "NRS Score") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

df %>%
  ggplot(aes(x = neck_pain, y = neck_pain_nrs)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, color = "black") +
  labs(title = "NRS by Neck Pain Status", x = "", y = "NRS Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  ggplot(aes(x = stress, y = neck_pain_nrs)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, color = "black") +
  labs(title = "NRS by Stress Status", x = "", y = "NRS Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  ggplot(aes(x = headache, y = neck_pain_nrs)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, color = "black") +
  labs(title = "NRS by Headache Status", x = "", y = "NRS Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Pain pre/post JPE-Test
# Vergleich, wie Neck pain (ja/nein) zu Nackenschmerzen vor dem JPE-Test
df %>%
  ggplot(aes(x = neck_pain, y = nrs_pre_jpe)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, color = "black") +
  labs(title = "NRS Before JPE-Test by Neck Pain Status", x = "", y = "NRS Pain Score Before") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# nach JPE-Test
df %>%
  ggplot(aes(x = neck_pain, y = nrs_post_jpe)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, color = "black") +
  labs(title = "NRS After JPE-Test by Neck Pain Status", x = "", y = "NRS Pain Score After") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Vergleich Neck Pain zu Schmerzen nach dem JPE-Test => 2 Personen haben nach dem Test etwas
#Schmerzen gehabt, obwohl sie grundsätzlich keine Nackenschmerzen hatten.

# Schmerz (NRS) vor und nach dem Joint-Position-Error (JPE) Assessment
df %>% 
  ggplot(aes(x = nrs_pre_jpe, y = nrs_post_jpe)) +
   geom_point(color = "blue", alpha = 0.6) +
   geom_smooth(method = "lm", color = "red", se = TRUE) +  
   labs(title = "Pain Scores Before vs. After the Test",
       x = "NRS Pain Score Before",
       y = "NRS Pain Score After") +
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5))  

# Nackenschmerz (ja/nein) und NRS (0-10)
df %>% 
  ggplot(aes(x = neck_pain, y = neck_pain_nrs)) +
   geom_violin(fill = "skyblue", alpha = 0.5) +  
   geom_jitter(width = 0.1, height = 0.1,
               alpha = 0.5, color = "black") +  
   labs(title = "Distribution of Neck Pain NRS by Neck Pain Status",
       x = "Neck Pain (0 = No, 1 = Yes)",
       y = "Neck Pain NRS (0-10)") +
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5))  
# Makes sense: NRS is bound by 0 at the lower end. 
# Values cramp together there for people without neck pain.
# People with neck pain are above a certain NRS 
# threshold and show nice variability.


# _NDI----
# Nur Personen mit Nackenschmerzen "ja" und NRS ungleich 0 haben NDI-Fragebogen ausgefüllt
df %>%
  ggplot(aes(x = factor(1), y = ndiscore)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, color = "black") +
  labs(title = "NDI Score", x = "", y = "NDI Score") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))



# Table 1------
# https://blog.djnavarro.net/posts/2024-06-21_table1/
# Produces Table 1 of the manuscript ("Characteristics of participants"):
# sex, the four pooled HSQ headache groups, and for NDI/PSQ the number of
# completed questionnaires per column plus the missing rows.

df_t1 <- df
df_t1$group <- factor(df_t1$neck_pain, levels = c("no", "yes"),
                      labels = c("No Neck Pain", "Neck Pain"))
df_t1$sex_t1 <- factor(df_t1$sex, levels = c("female", "male"))

# HSQ: probable diagnoses are pooled with the definite ones
hsq_levels <- c("migraine and probable migraine", "TTH and probable TTH",
                "other headache conditions", "no headache")
df_t1$hsq_group <- factor(
  dplyr::case_when(
    df_t1$headachetype %in% c("migraine", "probable migraine") ~ hsq_levels[1],
    df_t1$headachetype %in% c("TTH", "probable TTH")           ~ hsq_levels[2],
    df_t1$headachetype == "other headache"                     ~ hsq_levels[3],
    df_t1$headachetype == "no"                                 ~ hsq_levels[4]),
  levels = hsq_levels)
stopifnot(!anyNA(df_t1$hsq_group))  # every headache type is mapped

Hmisc::label(df_t1$sex_t1)        <- "Sex"
Hmisc::label(df_t1$age)           <- "Age"
Hmisc::label(df_t1$work_percent)  <- "Percentage of employment"
Hmisc::label(df_t1$neck_pain_nrs) <- "Numeric Rating Scale (NRS) (0-10)"
Hmisc::label(df_t1$hsq_group)     <- "Headache Screening Questionnaire (HSQ) (n=42)"
Hmisc::label(df_t1$ndiscore)      <- "Neck Disability Index (NDI) in % (n=32)"
Hmisc::label(df_t1$psqoverall)    <- "Perceived Stress Questionnaire (PSQ) (n=28)"

# Continuous: Mean (SD) only. For the questionnaires filled in by a subgroup
# (NDI, PSQ) the label row shows the number of completed questionnaires.
# NRS is reported with two decimals, everything else with 3 significant digits.
render_cont_t1 <- function(x, name, ...) {
  out <- if (name == "neck_pain_nrs")
    c("Mean (SD)" = sprintf("%.2f (%.2f)", mean(x), sd(x))) else
    table1::render.continuous.default(x, ...)["Mean (SD)"]
  first <- if (name %in% c("ndiscore", "psqoverall"))
    sprintf("(n=%d)", sum(!is.na(x))) else ""
  c(first, out)
}
render_t1 <- function(x, name, ...) {
  if (is.numeric(x)) {
    r <- render_cont_t1(x[!is.na(x)], name)
    if (anyNA(x) || name %in% c("ndiscore", "psqoverall"))
      r <- c(r, table1::render.missing.default(x))
    r
  } else {
    table1::render.default(x, name, ...)
  }
}

table1_obj <- table1::table1(
  ~ sex_t1 + age + work_percent + neck_pain_nrs + hsq_group +
    ndiscore + psqoverall | group,
  data = df_t1, render = render_t1,
  render.strat = function(strata, ...) {  # column headers with "(n=..)"
    n <- sapply(strata, nrow)
    setNames(sprintf("%s<br/><span class='stratn'>(n=%d)</span>", names(strata), n),
             sprintf("%s\n(n=%d)", names(strata), n))
  },
  caption = "Table 1: Characteristics of participants",
  footnote = paste("Abbreviations: SD = standard deviation, TTH = tension type",
                   "headache, NDI = neck disability index,",
                   "PSQ = perceived stress questionnaire"))
table1_obj

ft <- t1flex(table1_obj)
doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = "Table1_Output.docx")




# Table 2------------
# Measurements of joint position error (in degree), Mean (SD) with two
# decimals, grouped by movement direction as in the manuscript.
t2_dirs <- c(rrot = "Right Rotation", lrot = "Left Rotation",
             ext = "Extension", flex = "Flexion")
fmt_ms <- function(x) sprintf("%.2f (%.2f)", mean(x), sd(x))
t2_rows <- list()
for (d in names(t2_dirs)) {
  t2_rows[[length(t2_rows) + 1]] <- data.frame(
    Measure = t2_dirs[[d]], noNP = "", NP = "", all = "")
  for (m in c("AE", "CE", "VE")) {
    v <- paste0(tolower(m), "_", d)
    t2_rows[[length(t2_rows) + 1]] <- data.frame(
      Measure = sprintf("%s %s Mean (SD)", m, t2_dirs[[d]]),
      noNP = fmt_ms(df[[v]][df$neck_pain == "no"]),
      NP   = fmt_ms(df[[v]][df$neck_pain == "yes"]),
      all  = fmt_ms(df[[v]]))
  }
}
table2 <- do.call(rbind, t2_rows)
table2

n_grp <- table(df$neck_pain)
ft_t2 <- flextable(table2) %>%
  set_header_labels(Measure = "",
                    noNP = sprintf("No Neck Pain\n(N=%d)", n_grp[["no"]]),
                    NP   = sprintf("Neck Pain\n(N=%d)", n_grp[["yes"]]),
                    all  = sprintf("Overall\n(N=%d)", nrow(df))) %>%
  bold(i = which(table2$noNP == ""), j = 1) %>%
  add_footer_lines(paste("Abbreviations: AE = absolute error, CE = constant error,",
                         "VE = variable error, SD = standard deviation")) %>%
  autofit()
doc_t2 <- read_docx() %>%
  body_add_par("Table 2: Measurements of joint position error (in degree)") %>%
  body_add_flextable(ft_t2)
print(doc_t2, target = "Table2_Output.docx")




# Regressionsmodelle ------------

# Die Studie von AlDahas et al. (2023) und Robert Hill et al. (2009) empfehlen die Verwendung von AE und CE
# Analog zu der Studie von Quartey et al. (2019) habe ich mich für die Verwendung der Resultants entschieden
# AlDahas et al. (2023) zeigen die beste Reliabilität in rechtsrotation bei sitzender Position (bei asymptomatischen Personen)
# und eine gute Reliabilität bei Flexion (AE) sowie Flexion/Linksrotation/Rechtsrotation bei CE.


# _1) NRS-------------

colnames(df)

# --- Shared selection setup (used by BOTH the NRS and the neck-pain model) -----------

# The continuous (NRS) and the binary (neck pain) analyses use the SAME candidate
# predictors and the SAME maximum model size, so they are directly comparable:

#   - age is FORCED into both models (never subject to selection),
#   - bmi is left out of the candidate pool so it stays within bestglm's 15-predictor
#     ceiling for GLMs (it keeps an exhaustive 2^p search feasible),
#   - max_predictors caps the size of the FINAL model (including the forced-in age) and
#     is shared between regsubsets (continuous) and bestglm (discrete).
candidate_predictors <- c("ae_rrot", "ce_rrot", "ve_rrot",
                          "ae_lrot", "ce_lrot", "ve_lrot",
                          "ae_ext",  "ce_ext",  "ve_ext",
                          "ae_flex", "ce_flex", "ve_flex",
                          "sex", "headache", "stress")   # 15 candidates (age forced separately)
max_predictors <- 4   # max number of predictors in the FINAL model, INCLUDING age

best_subset_JPE <- regsubsets(
  reformulate(c("age", candidate_predictors), response = "neck_pain_nrs"),
  force.in = "age",
  nvmax    = max_predictors,
  data     = df)
summary(best_subset_JPE)
plot(best_subset_JPE, scale = "adjr2")
plot(best_subset_JPE, scale = "bic")

# Let's use BIC as criterium for model selection. 
# https://en.wikipedia.org/wiki/Bayesian_information_criterion
# BIC tries to weigh model fit against model complexity (number of predictors).
# BIC = -2 * log-likelihood + k * log(n), where k is the number of predictors
# and n the number of observations.
# When picking from several models, ones with lower BIC values are generally preferred.
# NOTE: BIC does not say anything about the CAUSAL relationships between the 
# explanatory variables and the outcome.
# It is a selection criterion that can be used to improve predictive quality of models.

best_subset_summary <- summary(best_subset_JPE)
num_predictors <- rowSums(best_subset_summary$which) - 1  # Subtract intercept
bic_values <- best_subset_summary$bic
bic_values
(bic_4cov <- bic_values[num_predictors == 4])  # Filter for models with 4 predictors
# 4.313959
# -> 3rd model
names(best_subset_summary$which[2, ][best_subset_summary$which[2, ]==TRUE])
# "(Intercept)" "age"         "ce_flex"     "headacheyes"   

model_NRS_BIC <- lm(neck_pain_nrs ~ age + ce_flex + headache, data = df)

performance::check_model(model_NRS_BIC)
performance::check_model(model_NRS_BIC, check = "pp_check") # ok, a little bump at NRS=0, but not too bad.

qqPlot(model_NRS_BIC) # not bad
# model fit is not bad.

summary(model_NRS_BIC) # Adjusted R-squared:  0.1753
# n=42, 38 dof.

# Publication-ready table: effect sizes (B and standardized beta) and 95% CIs, NO p-values.
# B    = unstandardized regression coefficient (effect on NRS in original units)
# beta = standardized coefficient (effect size in SD units; only for numeric predictors)
est   <- coef(model_NRS_BIC)
ci    <- confint(model_NRS_BIC)         

# Standardized coefficients: refit on z-scored numeric variables.
df_std <- df
num_vars <- c("neck_pain_nrs", "age", "ce_flex")
df_std[num_vars] <- lapply(df_std[num_vars], function(x) as.numeric(scale(x)))
std_est <- coef(lm(neck_pain_nrs ~ age + ce_flex + headache, data = df_std))

nice_labels <- c("(Intercept)" = "Intercept",
                 "age"         = "Age (years)",
                 "ce_flex"     = "CE Flexion",
                 "headacheyes" = "Headache (yes vs. no)")

model_NRS_BIC_table <- data.frame(
  Predictor    = nice_labels[names(est)],
  B            = round(est, 2),
  CI_95        = sprintf("[%.2f to %.2f]", ci[, 1], ci[, 2]),
  beta_std     = round(std_est[names(est)], 2),
  check.names  = FALSE,
  row.names    = NULL
)
# Standardized beta is not meaningful for the intercept
model_NRS_BIC_table$beta_std[model_NRS_BIC_table$Predictor == "Intercept"] <- NA
model_NRS_BIC_table

ft_model <- flextable(model_NRS_BIC_table) %>%
  set_header_labels(B = "B", CI_95 = "95% CI", beta_std = "Standardized β") %>%
  add_header_lines(
    sprintf("Linear model: NRS ~ Age + CE Flexion + Headache (adj. R² = %.3f)",
            summary(model_NRS_BIC)$adj.r.squared)) %>%
  autofit()
ft_model

doc_model <- read_docx() %>%
  body_add_flextable(ft_model) %>%
  body_add_par("Table: Linear regression of Neck Pain NRS (effect sizes with 95% CIs)",
               style = "heading 2")
print(doc_model, target = "Table_NRS_linear_model.docx")


# _2) _Neck pain cat------------
# Analogous to 1), but the outcome is now categorical (neck_pain: no/yes) -> logistic
# regression. Uses the SAME candidate_predictors and the SAME max_predictors as the NRS
# model above. The GLM analog of regsubsets is bestglm(..., family = binomial); we select
# by BIC and force age into the model (excluded from the search, added back manually),
# analogous to force.in = "age" in 1).

# bestglm expects a data frame "Xy" with all predictors first and the response in the
# LAST column. Built from candidate_predictors; the binary factors (sex/headache/stress)
# are passed as 0/1 numerics to avoid factor-handling quirks in the search. The FINAL
# model below is refit on df with the real factors, so the odds-ratio table gets
# readable labels.

Xy <- df[, candidate_predictors]
factor_cands <- names(Xy)[sapply(Xy, is.factor)]
Xy[factor_cands] <- lapply(Xy[factor_cands], function(x) as.integer(x) - 1L)
Xy$y <- df$neck_pain   # response MUST be the last column
dim(Xy)                # 42 x 16 (15 predictors + y)
Xy <- na.omit(Xy)      # bestglm needs complete cases
dim(Xy) # 42

# bestglm selects from the 15 candidates; age is forced in afterwards. To keep the FINAL
# model size equal to the NRS model (max_predictors, incl. age), the search may pick at
# most max_predictors - 1 predictors.
best_model_BIC <- bestglm(Xy, family = binomial, IC = "BIC", nvmax = max_predictors - 1L)
best_model_BIC
summary(best_model_BIC$BestModel)

# Forced-in age + BIC-selected predictors -> final logistic model on df (real factors).
selected_terms <- attr(terms(best_model_BIC$BestModel), "term.labels")
form_neckpain   <- reformulate(unique(c("age", selected_terms)), response = "neck_pain")
form_neckpain
model_neckpain_BIC <- glm(form_neckpain, data = df, family = binomial)
summary(model_neckpain_BIC)

performance::r2_mcfadden(model_neckpain_BIC)
#      R2: 0.129
# adj. R2: 0.089

check_model(model_neckpain_BIC) # model diagnostics
# -> the chosen model fits the data well.

# Classification check (confusion table at the prevalence threshold)
table(df$neck_pain)                          # no = 12, yes = 30
(prevalence <- mean(df$neck_pain == "yes"))    # 30 / 42
pred_prob  <- predict(model_neckpain_BIC, type = "response")
model_predicted_neck_pain <- pred_prob > 0.5
(confusion_table <- table(model_predicted_neck_pain, 
                     df$neck_pain[as.integer(names(pred_prob))])) 
(accuracy <- sum(diag(confusion_table))/sum(confusion_table)) # not well suited for prediction.
# 71.4% prevalence vs. 76.2% 

# Publication-ready table: effect sizes (odds ratios) and 95% CIs, NO p-values.
# OR > 1 -> higher odds of neck pain per unit increase (or vs. reference level).
or_est <- exp(coef(model_neckpain_BIC))
or_ci  <- exp(confint(model_neckpain_BIC))   # 95% CI (profile likelihood)

nice_labels <- c(
  "(Intercept)" = "Intercept",
  "age"         = "Age (years)",
  "bmi"         = "BMI",
  "sexfemale"   = "Sex (female vs. male)",
  "headacheyes" = "Headache (yes vs. no)",
  "stressyes"   = "Stress (yes vs. no)",
  "ae_rrot" = "AE Right Rotation", "ce_rrot" = "CE Right Rotation", "ve_rrot" = "VE Right Rotation",
  "ae_lrot" = "AE Left Rotation",  "ce_lrot" = "CE Left Rotation",  "ve_lrot" = "VE Left Rotation",
  "ae_ext"  = "AE Extension",      "ce_ext"  = "CE Extension",      "ve_ext"  = "VE Extension",
  "ae_flex" = "AE Flexion",        "ce_flex" = "CE Flexion",        "ve_flex" = "VE Flexion"
)
pred_label <- nice_labels[names(or_est)]
pred_label[is.na(pred_label)] <- names(or_est)[is.na(pred_label)]  # fallback to raw name

model_neckpain_table <- data.frame(
  Predictor   = pred_label,
  OR          = round(or_est, 2),
  CI_95       = sprintf("[%.2f to %.2f]", or_ci[, 1], or_ci[, 2]),
  check.names = FALSE,
  row.names   = NULL
)
model_neckpain_table

# Model statistics reported in the table footnote of the manuscript
r2_mf <- performance::r2_mcfadden(model_neckpain_BIC)
neckpain_footnote <- sprintf(paste0(
  "McFadden's adjusted pseudo-R² = %.3f. Akaike's information criterion = %.2f, ",
  "null deviance = %.2f (df = %d); Residual deviance = %.2f (df = %d)"),
  r2_mf$R2_adjusted, AIC(model_neckpain_BIC),
  model_neckpain_BIC$null.deviance, model_neckpain_BIC$df.null,
  model_neckpain_BIC$deviance, model_neckpain_BIC$df.residual)
neckpain_footnote

ft_neckpain <- flextable(model_neckpain_table) %>%
  set_header_labels(OR = "Odds ratio", CI_95 = "95% CI") %>%
  add_header_lines(
    paste0("Logistic model: Neck pain (yes/no) ~ ",
           paste(all.vars(form_neckpain)[-1], collapse = " + "))) %>%
  add_footer_lines(neckpain_footnote) %>%
  autofit()
ft_neckpain

doc_neckpain <- read_docx() %>%
  body_add_flextable(ft_neckpain) %>%
  body_add_par("Table: Logistic regression of Neck Pain (odds ratios with 95% CIs)",
               style = "heading 2")
print(doc_neckpain, target = "Table_neckpain_logistic_model.docx")


# Sessionsinformationen - loaded packages
sink("sessionInfo.txt")
print(sessionInfo())
sink()

