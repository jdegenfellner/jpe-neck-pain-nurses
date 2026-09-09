# ============================================================================
# Reproduction check for every number reported in Manuscript_Quesada_v5.docx
#
# Each figure printed in the manuscript (running text, Tables 1-4) is compared
# against a recomputation from schwyz_data_Doris2026.csv. The comparison is
# made at the decimal precision actually printed in the manuscript, using
# commercial rounding (half away from zero) as produced by table1/Word.
#
# Companion to 0_analysis.R (the analysis script); self-contained.
# ============================================================================
suppressMessages({library(readxl)})
# Run from R_Codes_and_DATA/ (in RStudio the working directory is set to the
# location of this file; from the command line, cd to R_Codes_and_DATA first).
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("./DATA/schwyz_data_Doris2026.csv", dec=".", sep=";", header=TRUE, stringsAsFactors=TRUE)
for (b in c("neck_pain","headache","stress")) df[[b]] <- factor(df[[b]], levels=c(0,1), labels=c("no","yes"))
df$sex <- factor(df$sex, levels=c(1,2), labels=c("male","female"))

res <- list()
chk <- function(label, manuscript, computed, tol=NULL) {
  m <- as.numeric(manuscript); k <- as.numeric(computed)
  dec <- sapply(as.character(manuscript), function(z) {
           z <- sub("-", "", z)
           if (grepl("[.]", z)) nchar(sub(".*[.]", "", z)) else 0 })
  # kaufmaennisches Runden (half away from zero), wie in table1/Word, nicht R-Default
  round_half_up <- function(x, d) sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d
  ok <- all(abs(round_half_up(k, dec) - m) <= 1e-9)
  res[[length(res)+1]] <<- data.frame(Stelle=label,
      Manuskript=paste(manuscript, collapse=" / "),
      Berechnet=paste(round(as.numeric(computed),3), collapse=" / "),
      OK=ifelse(ok,"ja","NEIN"), stringsAsFactors=FALSE)
}
ms <- function(x) c(mean(x,na.rm=TRUE), sd(x,na.rm=TRUE))

cat("=========== PARTICIPANTS / DESCRIPTIVES ===========\n")
chk("n final sample", 42, nrow(df), 0)
chk("weiblich", 38, sum(df$sex=="female"), 0)
chk("Neck pain n / %", c(30,71), c(sum(df$neck_pain=="yes"), round(100*mean(df$neck_pain=="yes"))), 0.5)
chk("NRS overall Mean/SD", c(2.50,1.85), ms(df$neck_pain_nrs))
chk("NDI (n=32) Mean/SD", c(14.0,7.43), ms(df$ndiscore))
chk("Headache n / %", c(35,83), c(sum(df$headache=="yes"), round(100*mean(df$headache=="yes"))), 0.5)
chk("PSQ (n=28) Mean/SD", c(31.1,9.94), ms(df$psqoverall))
chk("Daydifference Mean/SD", c(10.3,6.68), ms(df$Daydifference), 0.02)

cat("=========== TABLE 1 ===========\n")
np <- split(df, df$neck_pain)
chk("T1 Age no NP", c(30.7,10.8), ms(np$no$age), 0.05)
chk("T1 Age NP",    c(36.3,12.7), ms(np$yes$age), 0.05)
chk("T1 Age overall", c(34.7,12.4), ms(df$age), 0.05)
chk("T1 Employment no NP", c(80.8,30.0), ms(np$no$work_percent), 0.05)
chk("T1 Employment NP",    c(88.6,16.7), ms(np$yes$work_percent), 0.05)
chk("T1 Employment overall", c(86.4,21.2), ms(df$work_percent), 0.05)
chk("T1 NRS no NP", c(0.17,0.39), ms(np$no$neck_pain_nrs))
chk("T1 NRS NP",    c(3.43,1.28), ms(np$yes$neck_pain_nrs))
ht <- table(df$headachetype, df$neck_pain)
cat("  headachetype x neck_pain:\n"); print(ht)
chk("T1 NDI no NP (n=2)", c(10.0,8.49), ms(np$no$ndiscore), 0.01)
chk("T1 NDI NP (n=30)",   c(14.3,7.44), ms(np$yes$ndiscore), 0.01)
chk("T1 NDI missing no NP / NP / total", c(10,0,10), c(sum(is.na(np$no$ndiscore)), sum(is.na(np$yes$ndiscore)), sum(is.na(df$ndiscore))), 0)
chk("T1 PSQ no NP (n=8)", c(30.6,8.31), ms(np$no$psqoverall), 0.02)
chk("T1 PSQ NP (n=20)",   c(31.3,10.7), ms(np$yes$psqoverall), 0.05)
chk("T1 PSQ missing no NP / NP / total", c(4,10,14), c(sum(is.na(np$no$psqoverall)), sum(is.na(np$yes$psqoverall)), sum(is.na(df$psqoverall))), 0)

cat("=========== TABLE 2 ===========\n")
t2 <- list(
 c("AE Right Rotation","ae_rrot", 3.94,1.67, 3.32,1.91, 3.50,1.84),
 c("CE Right Rotation","ce_rrot", 2.38,3.15, 1.70,3.19, 1.89,3.15),
 c("VE Right Rotation","ve_rrot", 2.82,1.56, 1.89,1.16, 2.15,1.33),
 c("AE Left Rotation","ae_lrot",  2.76,1.29, 3.31,1.82, 3.15,1.69),
 c("CE Left Rotation","ce_lrot",  1.04,2.26, 0.67,3.20, 0.78,2.94),
 c("VE Left Rotation","ve_lrot",  2.22,1.44, 2.28,1.31, 2.26,1.33),
 c("AE Extension","ae_ext",       3.23,1.64, 2.27,1.31, 2.54,1.46),
 c("CE Extension","ce_ext",       0.75,3.30,-0.30,2.34,-0.00,2.65),
 c("VE Extension","ve_ext",       1.91,1.12, 1.58,0.95, 1.68,1.00),
 c("AE Flexion","ae_flex",        2.92,1.53, 2.56,1.52, 2.66,1.51),
 c("CE Flexion","ce_flex",        1.69,2.58,-0.18,2.37, 0.35,2.55),
 c("VE Flexion","ve_flex",        1.96,0.83, 1.91,1.31, 1.92,1.19))
for (r in t2) {
  v <- r[2]; m <- as.numeric(r[3:8])
  chk(paste0("T2 ", r[1], " (noNP/NP/all)"), m,
      c(ms(np$no[[v]]), ms(np$yes[[v]]), ms(df[[v]])), 0.006)
}

cat("=========== TABLE 3 (linear) ===========\n")
m3 <- lm(neck_pain_nrs ~ age + ce_flex + headache, data=df)
ci3 <- confint(m3)
chk("T3 Intercept B / CI", c(0.65,-1.08,2.38), c(coef(m3)[1], ci3[1,]))
chk("T3 Age B / CI",       c(0.03,-0.02,0.08), c(coef(m3)[2], ci3[2,]))
chk("T3 CE Flexion B / CI",c(-0.25,-0.47,-0.04), c(coef(m3)[3], ci3[3,]))
chk("T3 Headache B / CI",  c(1.10,-0.42,2.61), c(coef(m3)[4], ci3[4,]))
ds <- df; for (v in c("neck_pain_nrs","age","ce_flex")) ds[[v]] <- as.numeric(scale(ds[[v]]))
sb <- coef(lm(neck_pain_nrs ~ age + ce_flex + headache, data=ds))
chk("T3 std beta Age / CEflex / Headache", c(0.20,-0.35,0.59), sb[2:4])
chk("T3 adjusted R2", 0.175, summary(m3)$adj.r.squared, 0.0006)

cat("=========== TABLE 4 (logistic) ===========\n")
m4 <- glm(neck_pain ~ age + ce_flex, data=df, family=binomial)
or <- exp(cbind(coef(m4), suppressMessages(confint(m4))))
chk("T4 Intercept OR / CI", c(0.80,0.07,7.49), or[1,])
chk("T4 Age OR / CI",       c(1.04,0.98,1.12), or[2,])
chk("T4 CE Flexion OR / CI",c(0.73,0.52,0.98), or[3,])
chk("T4 AIC", 49.76, AIC(m4), 0.006)
chk("T4 null deviance / df", c(50.26,41), c(m4$null.deviance, m4$df.null), 0.006)
chk("T4 resid deviance / df", c(43.76,39), c(m4$deviance, m4$df.residual), 0.006)
suppressMessages(library(performance))
chk("T4 'Adjusted R2' (= McFadden adj.)", 0.089, performance::r2_mcfadden(m4)$R2_adjusted)
chk("T4 McFadden R2 (nicht im Manuskript)", 0.129, performance::r2_mcfadden(m4)$R2)
chk("gestrichener Text: Tjur's R2 = 0.18", 0.18, performance::r2_tjur(m4))

cat("=========== FISHER ===========\n")
ft <- fisher.test(table(df$headache, df$neck_pain))
chk("Fisher OR / CI", c(2.12,0.26,15.44), c(ft$estimate, ft$conf.int), 0.006)

cat("=========== TEXT-ZAHLEN ===========\n")
chk("Text: AE RR / LR / Ext / Flex (Outcome data)",
    c(4.39,4.13,3.70,4.06),
    c(mean(df$ae_rrot), mean(df$ae_lrot), mean(df$ae_ext), mean(df$ae_flex)))
chk("Text: Daydifference range 0-32", c(0,32), range(df$Daydifference,na.rm=TRUE), 0)
cat("  CE Flexion Gruppendifferenz:", round(abs(mean(np$no$ce_flex)-mean(np$yes$ce_flex)),2),
    "Grad (Manuskript: \"slightly less than 2\")\n")
cat("  max AE Gruppendifferenz (Extension):", round(abs(mean(np$no$ae_ext)-mean(np$yes$ae_ext)),2),
    "Grad (Manuskript: \"approximately 1\")\n")
cat("  VE Gruppenvergleich (noNP vs NP): rrot", round(mean(np$no$ve_rrot),2), "vs", round(mean(np$yes$ve_rrot),2),
    "| lrot", round(mean(np$no$ve_lrot),2), "vs", round(mean(np$yes$ve_lrot),2),
    "| ext", round(mean(np$no$ve_ext),2), "vs", round(mean(np$yes$ve_ext),2),
    "| flex", round(mean(np$no$ve_flex),2), "vs", round(mean(np$yes$ve_flex),2), "\n")

out <- do.call(rbind, res)
cat("\n\n================== ERGEBNIS ==================\n")
print(out, row.names=FALSE, right=FALSE)
cat("\nGeprüft:", nrow(out), " | OK:", sum(out$OK=="ja"), " | Abweichung:", sum(out$OK=="NEIN"), "\n")
