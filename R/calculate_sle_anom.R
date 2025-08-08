#' calculate_sle_anom: calculate sea level changes.
#'
#' @description
#' Calculate sea level changes (cm SLE) relative to a baseline year.
#'
#' @param dataset Dataset to calculate: only acts on year value columns
#'
#' @returns `calculate_sle_anom()` returns ice_data with SL columns in
#' corrected units and relative to baseline year.
#'
#' @export

calculate_sle_anom <- function(data_conv, baseline = 2014) {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat("calculate_sle_anom: recalculate SLE anomalies with respect to",baseline,"\n",file = logfile_build, append = TRUE)

  # CALCULATE SEA LEVEL ANOMALIES w.r.t. calibration start date
  data_conv[ , paste0("y",years_sim)] <- data_conv[ , paste0("y",years_sim)] - data_conv[ , paste0("y",baseline) ]

  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  return(data_conv)

}
