# =============================================================================
# EBS Pollock: SCA Assessment -> OM Conditioning via openMSE
# =============================================================================
# Parses pm_24.dat, runs SAMtool SCA, and conditions an operating model
# using Assess2OM for management strategy evaluation.
#
# Requirements: MSEtool, SAMtool (openMSE 2.0)
# =============================================================================

library(MSEtool)
library(SAMtool)

# --- Configurable parameters ------------------------------------------------
#dat_file  <- file.path("runs", "data", "pm_24.dat")
dat_rds   <- file.path(here::here("openMSE", "data", "pm_24.rds"))
M_value   <- 0.3        # natural mortality (ages 3+; see age-specific below)
SR        <- "BH"       # stock-recruit relationship ("BH" or "Ricker")
proyears  <- 50         # projection years for MSE
nsim_om   <- 48         # number of simulations for OM (must be >= 2)
fix_h     <- TRUE       # fix steepness in SCA?
steep     <- 0.9        # steepness if fixed

# Von Bertalanffy parameters (from pm.tpl lines 6464-6467)
vbLinf <- 68.43045172
vbK    <- 0.139339914
L0     <- 25.23382299   # length at age 1 (recage)

# =============================================================================
# Step 1: Load pm_24.dat (cached as RDS)
# =============================================================================

# Data parser for ADMB-format tab-delimited .dat files
# (inlined from utilities.R:651)
read_data <- function(file) {
  lines <- readLines(file)
  result <- list()
  current_name <- NULL
  buffer <- list()

  for (line in lines) {
    line <- trimws(line)
    if (line == "") next
    if (startsWith(line, "#")) {
      if (!is.null(current_name)) {
        result[[current_name]] <- if (length(buffer) == 1) buffer[[1]] else do.call(rbind, buffer)
      }
      current_name <- sub("^#", "", line)
      buffer <- list()
    } else {
      nums <- as.numeric(unlist(strsplit(line, "\\s+")))
      buffer[[length(buffer) + 1]] <- nums
    }
  }

  if (!is.null(current_name)) {
    result[[current_name]] <- if (length(buffer) == 1) buffer[[1]] else do.call(rbind, buffer)
  }

  return(result)
}

if (file.exists(dat_rds)) {
  dat <- readRDS(dat_rds)
  cat("Loaded cached data from:", dat_rds, "\n")
} else {
  if (!file.exists(dat_file)) {
    stop("Missing data file: ", dat_file, "\n",
         "Run once with pm_24.dat present to create the cached RDS: ", dat_rds)
  }
  dat <- read_data(dat_file)
  saveRDS(dat, dat_rds)
  cat("Parsed and cached data to:", dat_rds, "\n")
}

# Extract scalars
styr   <- as.integer(dat$styr)
endyr  <- as.integer(dat$endyr)
recage <- as.integer(dat$recage)
nages  <- as.integer(dat$nages)
nyears <- endyr - styr + 1
ages   <- recage:(recage + nages - 1)

cat("Model years:", styr, "-", endyr, "(", nyears, "years),",
    nages, "ages (", recage, "-", recage + nages - 1, ")\n")

# Extract vectors and matrices
obs_catch  <- as.numeric(dat$obs_catch)
p_mature   <- as.numeric(dat$p_mature)
wt_fsh     <- dat$wt_fsh   # nyears x nages matrix

# Survey indices
yrs_bts <- as.integer(dat$yrs_bts_data)
ob_bts  <- as.numeric(dat$ob_bts)
ob_bts_std <- as.numeric(dat$ob_bts_std)
n_bts   <- as.integer(dat$n_bts)

yrs_ats <- as.integer(dat$yrs_ats_data)
ob_ats  <- as.numeric(dat$ob_ats)
ob_ats_std <- as.numeric(dat$ob_ats_std)
n_ats   <- as.integer(dat$n_ats)

yrs_avo <- as.integer(dat$yrs_avo)
ob_avo  <- as.numeric(dat$ob_avo)
ob_avo_std <- as.numeric(dat$ob_avo_std)
n_avo   <- as.integer(dat$n_avo)

# Fishery catch-at-age
yrs_fsh <- as.integer(dat$yrs_fsh_data)
n_fsh   <- as.integer(dat$n_fsh)
sam_fsh <- as.numeric(dat$sam_fsh)
oac_fsh <- dat$oac_fsh_data  # n_fsh x nages

# Normalize catch-at-age rows to proportions (some years are in numbers)
oac_fsh_prop <- oac_fsh / rowSums(oac_fsh)

cat("Data parsed successfully.\n")
cat("  Catch range:", range(obs_catch), "\n")
cat("  BTS:", n_bts, "obs,", "ATS:", n_ats, "obs,", "AVO:", n_avo, "obs\n")
cat("  Fishery CAA:", n_fsh, "years\n")

# =============================================================================
# Step 2: Derive growth & weight-length parameters
# =============================================================================

# Derive t0 from L0 (length at recage=1):
#   L(a) = Linf * (1 - exp(-K * (a - t0)))
#   L0 = Linf * (1 - exp(-K * (recage - t0)))
#   => t0 = recage + log(1 - L0/Linf) / K
vbt0 <- recage + log(1 - L0 / vbLinf) / vbK

# Predicted length-at-age
len_at_age <- vbLinf * (1 - exp(-vbK * (ages - vbt0)))

# Mean weight-at-age from fishery data (use rows that are in proportions, ~first 27 years)
# More robustly, use all years and take column means
mean_wt_age <- colMeans(wt_fsh)

# Fit weight-length: log(W) = log(a) + b * log(L)
wl_fit <- lm(log(mean_wt_age) ~ log(len_at_age))
wla <- exp(coef(wl_fit)[1])
wlb <- coef(wl_fit)[2]

cat("\nGrowth parameters:\n")
cat("  Linf =", vbLinf, " K =", vbK, " t0 =", round(vbt0, 3), "\n")
cat("  W-L: a =", signif(wla, 4), " b =", round(wlb, 4), "\n")

# Derive L50 and L95 from maturity-at-age interpolated to length-at-age
# p_mature is maturity at ages 1:15
L50 <- approx(p_mature, len_at_age, xout = 0.5)$y
L95 <- approx(p_mature, len_at_age, xout = 0.95)$y
cat("  L50 =", round(L50, 1), "cm,  L95 =", round(L95, 1), "cm\n")

# =============================================================================
# Step 3: Build the MSEtool Data object
# =============================================================================

pollock_data <- new("Data")
pollock_data@Name  <- "EBS Pollock"
pollock_data@Year  <- styr:endyr
pollock_data@MaxAge <- max(ages)
pollock_data@Mort  <- M_value
pollock_data@steep <- steep

# Catch time series (1 x nyears matrix)
pollock_data@Cat <- matrix(obs_catch, nrow = 1)

# Primary index: use BTS as Data@Ind (padded with NA for non-survey years)
bts_full <- rep(NA_real_, nyears)
bts_cv_full <- rep(NA_real_, nyears)
bts_full[yrs_bts - styr + 1] <- ob_bts
bts_cv_full[yrs_bts - styr + 1] <- ob_bts_std / ob_bts
pollock_data@Ind <- matrix(bts_full, nrow = 1)
pollock_data@CV_Ind <- matrix(bts_cv_full, nrow = 1)

# Additional indices via AddInd: BTS, ATS, AVO (3 indices)
# AddInd dimensions: [nsim, nindex, nyears]
AddInd    <- array(NA_real_, dim = c(1, 3, nyears))
CV_AddInd <- array(NA_real_, dim = c(1, 3, nyears))

# Index 1: BTS
AddInd[1, 1, yrs_bts - styr + 1] <- ob_bts
CV_AddInd[1, 1, yrs_bts - styr + 1] <- ob_bts_std / ob_bts

# Index 2: ATS
AddInd[1, 2, yrs_ats - styr + 1] <- ob_ats
CV_AddInd[1, 2, yrs_ats - styr + 1] <- ob_ats_std / ob_ats

# Index 3: AVO
AddInd[1, 3, yrs_avo - styr + 1] <- ob_avo
CV_AddInd[1, 3, yrs_avo - styr + 1] <- ob_avo_std / ob_avo

pollock_data@AddInd   <- AddInd
pollock_data@CV_AddInd <- CV_AddInd

# Index units: all biomass-based (1 = biomass)
pollock_data@AddIunits <- c(1L, 1L, 1L)

# Vulnerability schedule for each index
# AddIndV dimensions: [nsim, nindex, MaxAge+1] — all 1 for biomass surveys
# SCA uses n_age = max_age + 1 (ages 0:max_age internally)
pollock_data@AddIndV <- array(1, dim = c(1, 3, max(ages) + 1))

# Catch-at-age: array [nsim=1, nyears, MaxAge+1]
# SCA internally uses n_age = max_age + 1 (ages 0:max_age).
# Pad with age-0 column of zeros so dim 3 = MaxAge + 1 = 16.
caa_full <- matrix(0, nrow = nyears, ncol = nages + 1)  # columns: age 0, 1, ..., 15
caa_full[yrs_fsh - styr + 1, 2:(nages + 1)] <- oac_fsh_prop
pollock_data@CAA <- array(caa_full, dim = c(1, nyears, nages + 1))

# Sample sizes for CAA — store in CV_Cat as a rough proxy;
# SCA uses CAA_multiplier to control effective sample sizes
pollock_data@CV_Cat <- matrix(0.05, nrow = 1, ncol = nyears)

# Life history parameters
pollock_data@vbLinf <- vbLinf
pollock_data@vbK    <- vbK
pollock_data@vbt0   <- vbt0
pollock_data@wla    <- wla
pollock_data@wlb    <- wlb
pollock_data@L50    <- L50
pollock_data@L95    <- L95

# Length-at-age for reference
pollock_data@LenCV  <- 0.06  # from pm.tpl sdage

cat("\nData object built successfully.\n")

# =============================================================================
# Step 4: Run SCA
# =============================================================================

cat("\nRunning SCA assessment...\n")

res <- SCA(
  x = 1,
  Data = pollock_data,
  AddInd = 1:3,
  SR = SR,
  vulnerability = "dome",
  catch_eq = "Baranov",
  fix_h = fix_h,
  max_age = max(ages)
)

cat("SCA convergence:", res@conv, "\n")

if (res@conv) {
  cat("  R0   =", signif(res@R0, 4), "\n")
  cat("  h    =", round(res@h, 3), "\n")
  cat("  FMSY =", round(res@FMSY, 4), "\n")
  cat("  MSY  =", round(res@MSY, 1), "\n")
  cat("  SSB0 =", signif(res@SSB0, 4), "\n")
  cat("  Terminal SSB/SSB0 =", round(tail(res@SSB_SSB0, 1), 3), "\n")
  cat("  Terminal F/FMSY   =", round(tail(res@F_FMSY, 1), 3), "\n")
} else {
  warning("SCA did not converge. Check model specification and starting values.")
}

# Diagnostic report (markdown/html) saved alongside the project
report_dir <- file.path(getwd(), "reports")
report_file <- SAMtool:::report(res, dir = report_dir, open_file = FALSE)
cat("SCA report saved to:", report_file, "\n")

# Save Assessment object for downstream reporting
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
assessment_rds <- file.path(report_dir, "assessment_sca.rds")
saveRDS(res, assessment_rds)
cat("SCA Assessment saved to:", assessment_rds, "\n")

# Save a lightweight assessment summary for cross-platform comparisons
comparison_dir <- file.path(here::here("openMSE", "analysis", "outputs"))
dir.create(comparison_dir, showWarnings = FALSE, recursive = TRUE)

get_slot <- function(x, slot_name) {
  if (slot_name %in% slotNames(x)) {
    return(slot(x, slot_name))
  }
  NULL
}

years_vec <- as.numeric(pollock_data@Year)
align_years <- function(x, years = years_vec) {
  if (is.null(x)) return(NULL)
  x <- as.numeric(x)
  if (length(x) == length(years)) return(x)
  if (length(x) > length(years)) return(x[seq_along(years)])
  if (length(x) < length(years)) return(c(x, rep(NA_real_, length(years) - length(x))))
  x
}

ssb_vals <- get_slot(res, "SSB")
if (is.null(ssb_vals)) {
  ssb_ratio <- get_slot(res, "SSB_SSB0")
  ssb0 <- get_slot(res, "SSB0")
  if (!is.null(ssb_ratio) && !is.null(ssb0)) {
    ssb_vals <- as.numeric(ssb_ratio) * as.numeric(ssb0)
  }
}

rec_vals <- get_slot(res, "R")
if (is.null(rec_vals)) rec_vals <- get_slot(res, "Rec")
if (is.null(rec_vals)) rec_vals <- get_slot(res, "Recruitment")

f_vals <- get_slot(res, "FMort")

openmse_assessment <- list(
  report = list(
    years = years_vec,
    SSB = align_years(ssb_vals),
    Recruitment = align_years(rec_vals),
    F = align_years(f_vals)
  ),
  metadata = list(
    model = "openmse_sca",
    created = Sys.time(),
    source = "openMSE/R/sca_openmse.R"
  )
)

comparison_rds <- file.path(comparison_dir, "base.rds")
saveRDS(openmse_assessment, comparison_rds)
cat("SCA assessment summary saved to:", comparison_rds, "\n")

# Diagnostic plots (interactive only)
if (interactive()) {
  plot(res)
}

# =============================================================================
# Step 5: Condition OM via Assess2OM
# =============================================================================

cat("\nConditioning operating model via Assess2OM...\n")

# Extract arrays from SCA result
# SCA uses n_age = max_age + 1 = 16 (ages 0-15 internally).
# Drop age-0 column (col 1) since Assess2OM with recind=1 creates it.
N_mat <- res@N_at_age[, -1]                              # [nyears+1, 15] ages 1-15
nyrs_assess <- nrow(N_mat)

# F-at-age: FMort * Selectivity, drop age-0 column
# FMort: vector of length nyears; Selectivity: [nyears, 16]
F_at_age_mat <- (as.numeric(res@FMort) * res@Selectivity)[, -1]  # [nyears, 15]

# Weight-at-age: use fishery wt_fsh for all years (nyears x nages)
wt_age_mat <- wt_fsh

# Maturity: replicate p_mature across years
mat_mat <- matrix(p_mature, nrow = nyears, ncol = nages, byrow = TRUE)

# Natural mortality: constant M_value across ages/years
# (could use age-specific M: 0.9 for age 1, 0.45 for age 2, 0.3 for ages 3+)
M_vec <- c(0.9, 0.45, rep(0.3, nages - 2))
M_mat <- matrix(M_vec, nrow = nyears, ncol = nages, byrow = TRUE)

# Length-at-age: replicate across years
laa_mat <- matrix(len_at_age, nrow = nyears, ncol = nages, byrow = TRUE)

# Reshape [nyears, nages] matrices to [nsim=1, nages, nyears] arrays for Assess2OM
# Assess2OM expects arrays with dim [sim, ages, year] where first age = age 0.
# Since recage=1, set recind=1 to tell Assess2OM to create a dummy age 0.
#
# N_at_age from SCA has nyears+1 rows (includes terminal+1 year projection).
# Use rows 1:nyears for the historical period.
#
# Helper: convert [nyears, nages] matrix -> [nsim, nages, nyears] array
# Assess2OM requires nsim >= 2 (R drops dimensions with nsim=1).
# Replicate the single assessment result across nsim simulations.
nsim_om <- 48
mat2arr <- function(m, nsim = nsim_om) {
  # m is [nyears, nages]; need [nsim, nages, nyears]
  arr1 <- array(t(m), dim = c(1, ncol(m), nrow(m)))
  arr1[rep(1, nsim), , , drop = FALSE]
}

naa_array   <- mat2arr(N_mat[1:nyears, ])
faa_array   <- mat2arr(F_at_age_mat)
waa_array   <- mat2arr(wt_age_mat)
Mataa_array <- mat2arr(mat_mat)
Maa_array   <- mat2arr(M_mat)
laa_array   <- mat2arr(laa_mat)

# Steepness from SCA result (or fixed value)
h_val <- if (length(res@h) > 0 && res@h > 0) res@h else steep

OM <- Assess2OM(
  Name = "EBS Pollock",
  naa = naa_array,
  faa = faa_array,
  waa = waa_array,
  Mataa = Mataa_array,
  Maa = Maa_array,
  laa = laa_array,
  CurrentYr = endyr,
  h = h_val,
  proyears = proyears,
  recind = recage,
  plusgroup = TRUE,
  report = TRUE
)

cat("\nOM conditioning complete.\n")
cat("  nsim =", OM@nsim, "\n")
cat("  nyears =", OM@nyears, "\n")
cat("  maxage =", OM@maxage, "\n")
cat("  proyears =", OM@proyears, "\n")

# =============================================================================
# Step 6: Basic MSE demonstration
# =============================================================================

cat("\nRunning basic MSE...\n")

MSE_result <- runMSE(OM, MPs = c("FMSYref", "FMSYref75", "NFref"))

cat("MSE complete.\n")
summary(MSE_result)
