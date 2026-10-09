# =============================================================================
# MSE to Slick Conversion Helper
# =============================================================================
# Converts an MSE object from openMSE to a Slick object for visualization
# =============================================================================

library(Slick)
library(MSEtool)

#' Convert MSE object to Slick object
#'
#' @param MSE An MSE object from runMSE()
#' @param title Title for the Slick object
#' @param subtitle Subtitle
#' @param author Author name(s)
#' @return A Slick object
MSE2Slick <- function(MSE,
                      title = "MSE Results",
                      subtitle = "",
                      author = "openMSE") {

  # Extract dimensions
  nsim <- MSE@nsim
  nMP <- MSE@nMPs
  proyears <- MSE@proyears
  MPs <- MSE@MPs

  # Years
  CurrentYr <- MSE@OM$CurrentYr[1]  # Take first element if vector
  hist_years <- (CurrentYr - MSE@nyears + 1):CurrentYr
  proj_years <- (CurrentYr + 1):(CurrentYr + proyears)
  all_years <- c(hist_years, proj_years)

  # --- Build MPs object ---
  mps <- MPs(
    Code = MPs,
    Label = MPs,
    Description = rep("", nMP),
    Color = default_mp_colors(nMP)
  )

  # --- Build OMs object (single OM case) ---
  # Factors: data.frame with Factor, Level, Description columns
  factors_df <- data.frame(
    Factor = "Scenario",
    Level = "Base",
    Description = "SCA-conditioned base case"
  )
  # Design: data.frame where rows = OMs, columns = Factor names
  design_df <- data.frame(Scenario = "Base", row.names = "Base Case")

  oms <- OMs(
    Factors = factors_df,
    Design = design_df
  )

  # --- Build Boxplot data ---
  # Performance metrics from MSE summary
  # Slick Boxplot Value dimensions: [nsim, nOM, nMP, nPI]
  nPI_box <- 3
  nOM <- 1

  # Calculate performance indicators per sim and MP
  # F < FMSY (average over projection years per sim)
  PNOF <- apply(MSE@FM[, , , drop = FALSE] < 1, c(1, 2), mean)  # [nsim, nMP]

  # SSB > 0.5 SSBMSY
  P50 <- apply(MSE@SB_SBMSY[, , , drop = FALSE] > 0.5, c(1, 2), mean)

  # Average yield (last 10 years) relative to MSY
  # RefPoint$MSY may be a vector (per sim) or matrix - handle both
  MSY_ref <- if (is.matrix(MSE@RefPoint$MSY)) {
    rowMeans(MSE@RefPoint$MSY)  # average across years if matrix
  } else if (length(MSE@RefPoint$MSY) == nsim) {
    MSE@RefPoint$MSY
  } else {
    rep(mean(MSE@RefPoint$MSY), nsim)  # fallback
  }
  LTY <- apply(MSE@Catch[, , (proyears-9):proyears, drop = FALSE], c(1, 2), mean) /
         MSY_ref

  # Stack into array [nsim, nOM=1, nMP, nPI=3]
  boxplot_val <- array(NA, dim = c(nsim, nOM, nMP, nPI_box))
  for (mp in 1:nMP) {
    boxplot_val[, 1, mp, 1] <- PNOF[, mp]
    boxplot_val[, 1, mp, 2] <- P50[, mp]
    boxplot_val[, 1, mp, 3] <- LTY[, mp]
  }

  boxplot_obj <- Boxplot(
    Code = c("PNOF", "P50", "LTY"),
    Label = c("P(F<FMSY)", "P(SSB>0.5SSBMSY)", "Long-term Yield"),
    Description = c(
      "Probability of not overfishing",
      "Probability SSB above half SSBMSY",
      "Mean yield in final 10 years relative to MSY"
    ),
    Value = boxplot_val
  )

  # --- Build Timeseries data ---
  # SSB/SSBMSY, F/FMSY, Catch
  # Slick Timeseries Value dimensions: [nsim, nOM, nMP, nPI, ntime]
  nPI_ts <- 3

  ts_val <- array(NA, dim = c(nsim, nOM, nMP, nPI_ts, proyears))

  for (mp in 1:nMP) {
    # SSB/SSBMSY [nsim, proyears] -> [nsim, 1, mp, 1, proyears]
    ts_val[, 1, mp, 1, ] <- MSE@SB_SBMSY[, mp, ]
    # F/FMSY
    ts_val[, 1, mp, 2, ] <- MSE@FM[, mp, ]
    # Catch
    ts_val[, 1, mp, 3, ] <- MSE@Catch[, mp, ]
  }

  timeseries_obj <- Timeseries(
    Code = c("SSB_SSBMSY", "F_FMSY", "Catch"),
    Label = c("SSB/SSBMSY", "F/FMSY", "Catch"),
    Description = c(
      "Spawning biomass relative to SSBMSY",
      "Fishing mortality relative to FMSY",
      "Total catch"
    ),
    Time = proj_years,
    TimeNow = proj_years[1],  # First projection year (must be within Time)
    TimeLab = "Year",
    Value = ts_val,
    Target = c(1, 1, NA),
    Limit = c(0.5, 1, NA)
  )

  # --- Build Kobe data ---
  # Kobe Value array: [nsim, nOM, nMP, nPI=2, ntime]
  # nPI dimension: 1 = B/BMSY, 2 = F/FMSY
  kobe_val <- array(NA, dim = c(nsim, nOM, nMP, 2, proyears))

  for (mp in 1:nMP) {
    # B/BMSY trajectory
    kobe_val[, 1, mp, 1, ] <- MSE@SB_SBMSY[, mp, ]
    # F/FMSY trajectory
    kobe_val[, 1, mp, 2, ] <- MSE@FM[, mp, ]
  }

  kobe_obj <- Kobe(
    Code = c("B_BMSY", "F_FMSY"),
    Label = c("B/BMSY", "F/FMSY"),
    Description = c("Biomass relative to BMSY", "Fishing mortality relative to FMSY"),
    Time = proj_years,
    TimeLab = "Year",
    Value = kobe_val,
    Target = c(1, 1),
    Limit = c(0.5, 1)
  )

  # --- Assemble Slick object ---
  slick <- Slick(
    Title = title,
    Subtitle = subtitle,
    Author = author,
    MPs = mps,
    OMs = oms,
    Boxplot = boxplot_obj,
    Timeseries = timeseries_obj,
    Kobe = kobe_obj
  )

  return(slick)
}

# =============================================================================
# Example usage with EBS Pollock MSE results
# =============================================================================

if (FALSE) {
  # Source the SCA/MSE script first
  source("sca_openmse.R")

  # Convert to Slick
  slick_obj <- MSE2Slick(
    MSE_result,
    title = "EBS Pollock MSE",
    subtitle = "SCA-conditioned operating model",
    author = "NOAA AFSC"
  )

  # Launch Slick app
  App(slick_obj)

  # Or save for later
  saveRDS(slick_obj, "EBS_pollock_slick.rds")
}
