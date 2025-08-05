#' calculate_sle_anom: calculate sea level changes.
#'
#' @description
#' Calculate sea level changes (cm SLE) relative to a baseline year.
#' Convert all to cm SLE
#'
#' @param dataset Dataset to calculate: only acts on year value columns
#'
#' @returns `calculate_sle_anom()` returns ice_data with SL columns in
#' corrected units and relative to baseline year.
#'
#' @export

calculate_sle_anom <- function(data_conv) {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat("calculate_sle_anom: calculate SLE anomalies and standardise units\n",file = logfile_build, append = TRUE)

  # CALCULATE SEA LEVEL ANOMALIES w.r.t. calibration start date
  data_conv[ , paste0("y",years_sim)] <- data_conv[ , paste0("y",years_sim)] - data_conv[ , paste0("y",cal_start) ]

  # Convert mm SLE volume (above flot for GloGEM; also OGGM?) to cm SLE for glaciers
  if (i_s == "GLA") data_conv[ , paste0("y",years_sim)] <- data_conv[ , paste0("y",years_sim) ] / 10.0

  # Convert m SLE to cm SLE for ice sheets
  if (i_s %in% c("GIS", "AIS")) data_conv[ , paste0("y",years_sim)] <- data_conv[ , paste0("y",years_sim) ] * 100.0

  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  return(data_conv)

}
