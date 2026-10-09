# Run from the repository root. Paths are supplied explicitly; raw external
# workbooks and assessment inputs remain in their owning projects.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop("Usage: Rscript R/audit_inputs.R pm_25.dat pm_26.dat BTS_workbook.xlsx weight_at_age.csv")
}
library(dplyr)
library(tidyr)
library(readxl)
out_dir <- "site/data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv <- function(x, name) write.csv(x, file.path(out_dir, name), row.names = FALSE, na = "")

read_admb_blocks <- function(path) {
  lines <- trimws(readLines(path, warn = FALSE))
  result <- list()
  key <- NULL
  rows <- list()
  flush <- function() {
    if (!is.null(key)) {
      if (!length(rows)) stop("Empty block: ", key)
      sizes <- lengths(rows)
      if (length(unique(sizes)) != 1L) stop("Unequal row lengths in ", key)
      result[[key]] <<- if (length(rows) == 1L) rows[[1]] else do.call(rbind, rows)
    }
  }
  for (line in lines[nzchar(lines)]) {
    if (startsWith(line, "#")) {
      flush()
      key <- strsplit(trimws(sub("^#", "", line)), "[[:space:]]+")[[1]][1]
      if (key %in% names(result)) stop("Repeated block: ", key)
      rows <- list()
    } else {
      tokens <- strsplit(sub("#.*$", "", line), "[[:space:]]+")[[1]]
      nums <- suppressWarnings(as.numeric(tokens))
      if (anyNA(nums)) stop("Non-numeric value in ", key)
      rows[[length(rows) + 1L]] <- nums
    }
  }
  flush()
  result
}

validate_assessment <- function(d, label) {
  n_years <- d$endyr - d$styr + 1L
  stopifnot(length(d$obs_catch) == n_years,
            length(d$p_mature) == d$nages,
            all(is.finite(d$wt_fsh)), all(d$wt_fsh > 0),
            all(d$p_mature >= 0 & d$p_mature <= 1))
  for (s in c("fsh", "bts", "ats")) {
    years <- d[[paste0("yrs_", s, "_data")]]
    stopifnot(length(years) == d[[paste0("n_", s)]], !anyDuplicated(years),
              all(years >= d$styr & years <= d$endyr))
  }
  stopifnot(nrow(d$oac_fsh_data) == d$n_fsh,
            ncol(d$oac_fsh_data) == d$nages, all(rowSums(d$oac_fsh_data) > 0))
  tibble(source = label, start_year = d$styr, terminal_year = d$endyr,
         ages = paste0(d$recage, "–", d$recage + d$nages - 1L, "+"),
         fishery_caa_last = max(d$yrs_fsh_data), bts_last = max(d$yrs_bts_data),
         ats_last = max(d$yrs_ats_data), avo_last = max(d$yrs_avo),
         fishery_caa_years = d$n_fsh, bts_years = d$n_bts,
         required_weight_rows = n_years, fishery_weight_rows = nrow(d$wt_fsh),
         spawning_weight_rows = nrow(d$wt_ssb),
         weight_dimensions_pass = identical(dim(d$wt_fsh), as.integer(c(n_years, d$nages))) &&
           identical(dim(d$wt_ssb), as.integer(c(n_years, d$nages))))
}

datasets <- list(pm_24_rds = readRDS("data/pm_24.rds"),
                 pm_25_dat = read_admb_blocks(args[1]),
                 pm_26_dat = read_admb_blocks(args[2]))
coverage <- bind_rows(lapply(names(datasets), function(n) validate_assessment(datasets[[n]], n)))
write_csv(coverage, "assessment_coverage.csv")
baseline <- datasets[[1]]
comparison <- bind_rows(lapply(names(datasets)[-1], function(label) {
  d <- datasets[[label]]
  bind_rows(lapply(intersect(names(baseline), names(d)), function(field) {
    old <- baseline[[field]]
    new <- d[[field]]
    same_shape <- length(old) == length(new) && identical(dim(old), dim(new))
    tibble(source = label, field = field, same_shape = same_shape,
           identical_values = same_shape && isTRUE(all.equal(old, new, check.attributes = FALSE, tolerance = 0)),
           max_absolute_difference = if (same_shape) max(abs(old - new)) else NA_real_)
  }))
}))
write_csv(comparison, "assessment_comparison.csv")
write_csv(tibble(source = names(datasets),
                 unit_as_stored = "thousand metric tons (assessment catch scale)",
                 first_catch = vapply(datasets, function(d) d$obs_catch[1], numeric(1)),
                 last_catch = vapply(datasets, function(d) tail(d$obs_catch, 1), numeric(1))), "catch_scale.csv")

bts <- read_excel(args[3], sheet = "bts_abundance_at_age_1982_2026_")
years <- suppressWarnings(as.integer(bts$year))
keep <- !is.na(years) & years >= 1982 & years <= 2026
bts <- bts[keep, ]
bts$year <- years[keep]
stopifnot(!anyDuplicated(bts$year), ncol(bts) == 16L,
          identical(names(bts)[-1], c(paste0("age_", 1:14), "age_15_plus")))
age_cols <- names(bts)[-1]
year_qc <- bts |>
  mutate(cells_observed = rowSums(!is.na(pick(all_of(age_cols)))),
         abundance_total = rowSums(pick(all_of(age_cols))),
         status = case_when(cells_observed == 15 ~ "Complete", cells_observed == 0 ~ "Missing", TRUE ~ "Partial")) |>
  select(year, cells_observed, abundance_total, status)
write_csv(year_qc, "bts_year_qc.csv")
bts_long <- bts |>
  pivot_longer(-year, names_to = "age_bin", values_to = "numbers") |>
  mutate(age = as.integer(sub("_plus", "", sub("age_", "", age_bin))))
write_csv(bts_long |> filter(year >= 2024), "recent_bts_numbers.csv")
workbook_warnings <- list()
sheet_audit <- bind_rows(lapply(excel_sheets(args[3]), function(s) {
  sheet_warnings <- character()
  tryCatch({
    d <- withCallingHandlers(suppressMessages(read_excel(args[3], sheet = s, .name_repair = "unique")),
      warning = function(w) {
        sheet_warnings <<- c(sheet_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    if(length(sheet_warnings)) workbook_warnings[[s]] <<- tibble(sheet = s, warning = sheet_warnings)
    yr_col <- if ("Time" %in% names(d)) "Time" else if (s == "Index") names(d)[2] else "year"
    yr <- if (yr_col %in% names(d)) suppressWarnings(as.integer(d[[yr_col]])) else numeric()
    yr <- yr[is.finite(yr) & yr >= 1982 & yr <= 2026]
    tibble(sheet = s, rows = nrow(d), first_year = if(length(yr)) min(yr) else NA_integer_,
           last_year = if(length(yr)) max(yr) else NA_integer_, status = "Readable",
           warnings_count = length(sheet_warnings))
  }, error = function(e) tibble(sheet = s, rows = NA_integer_, first_year = NA_integer_,
                              last_year = NA_integer_, status = conditionMessage(e), warnings_count = length(sheet_warnings)))
}))
write_csv(sheet_audit, "workbook_sheet_audit.csv")
write_csv(bind_rows(workbook_warnings), "workbook_warnings.csv")

weights <- read.csv(args[4])
stopifnot(all(c("year", "age_bin", "weight_kg", "se_kg") %in% names(weights)))
weight_qc <- weights |>
  group_by(year) |>
  summarise(cells = n(), observed = sum(is.finite(weight_kg) & is.finite(se_kg)),
            .groups = "drop")
stopifnot(!anyDuplicated(paste(weights$year, weights$age_bin)),
          all(is.na(weights$weight_kg) == is.na(weights$se_kg)),
          all(weights$weight_kg > 0, na.rm = TRUE), all(weights$se_kg > 0, na.rm = TRUE))
write_csv(weight_qc, "bts_weight_qc.csv")
write_csv(weights |> filter(year == 2026), "bts_weight_2026.csv")

inputs <- c("data/pm_24.rds", args)
manifest <- tibble(source = c("openMSE_poll/data/pm_24.rds", "ebswp_assessment/2025/runs/data/pm_25.dat",
                              "admb/runs/data/pm_26.dat", "ebs_main/data/bts/BTS_Index_agecomps26.xlsx",
                              "pollock_prep/wiki/reports/bts-weight-2026/output/weight_at_age.csv"),
                   md5 = unname(tools::md5sum(inputs)), bytes = file.info(inputs)$size,
                   status = c("Historical demo", "Candidate; coverage audited", "Candidate; coverage audited",
                              "2026 development input; mixed vintages", "2026 model-derived survey weights; adoption unverified"))
write_csv(manifest, "input_manifest.csv")
writeLines(capture.output(sessionInfo()), "validation/audit-session.txt")
cat("INPUT_AUDIT_PASS\n")
print(coverage)
print(year_qc |> filter(year >= 2024))
print(sheet_audit)
print(weight_qc |> filter(year >= 2024))
print(comparison |> group_by(source) |> summarise(changed_fields = sum(!identical_values), .groups = "drop"))
