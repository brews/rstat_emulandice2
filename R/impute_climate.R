#' impute_climate: fill missing data in climate forcings
#'
#' @description
#' Imputes missing 2100 and 2300 values if 2099 and 2299 values are there
#' Constructs fixed climate for GIS post-2100 fixed_climate = 2100 simulations
#' Drops columns that are not needed
#'
#' @param climate_dataset Dataset
#' @param construct_fixed Set to TRUE to construct post-2100 forcings for GIS
#'
#' @returns `impute_climate()` returns climate dataset
#'
#' @export

impute_climate <- function(climate_dataset, construct_fixed = FALSE) {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat(paste("impute_climate: fill missing 2100 and 2300\n\n"),
      file = logfile_build, append = TRUE)

  # Impute two common missing cases that mean a simulation would be dropped unnecessarily

  # GCM only reached 2099: impute 2100 with this value
  miss_ind <- is.na(climate_dataset$y2100) & !is.na(climate_dataset$y2099)
  if (length(miss_ind[miss_ind]) > 0) {
    cat(sprintf("Imputing %i GCM simulations in data file by setting 2100 to 2099 value:\n", length(miss_ind[miss_ind])),
        file = logfile_build, append = TRUE)
    cat(paste(climate_dataset[ miss_ind, c("scenario")], climate_dataset[ miss_ind, c("GCM")], "\n"), "\n",
        file = logfile_build, append = TRUE)
    climate_dataset[ miss_ind, "y2100"] <- climate_dataset[ miss_ind, "y2099"]
  }

  # Same thing but for 2299/2300
  miss_ind <- is.na(climate_dataset$y2300) & !is.na(climate_dataset$y2299)
  if (length(miss_ind[miss_ind]) > 0) {
    cat(sprintf("Imputing %i GCM simulations in data file by setting 2300 to 2299 value:\n", length(miss_ind[miss_ind])),
        file = logfile_build, append = TRUE)
    cat(paste(climate_dataset[ miss_ind, c("scenario")], climate_dataset[ miss_ind, c("GCM")], "\n"), "\n",
        file = logfile_build, append = TRUE)
    climate_dataset[ miss_ind, "y2300"] <- climate_dataset[ miss_ind, "y2299"]
  }

  # GREENLAND ONLY (due to lack of forcings):
  # Construct whole duplicate array of forcings with fixed climate from 2100
  # Not very efficient, but very many are used in ensemble and saves index errors too
  # Shuffle index is documented in Appendix C of Goelzer et al. (2025) The Cryosphere
  if (construct_fixed) {

    cat(paste("Reconstructing fixed climates from 2100 (repeat 2091-2100, using Heiko's year shuffle)\n"),
        file = logfile_build, append = TRUE)

    # Index for each decade after fixed date
    #decadal_ind <- seq(from = 2101, to = 2291, by = 10)

    # Paste 2091-2100 values into these
    #for (dd in 1:length(decadal_ind)) {
    #  climate_dataset[ , paste0("y", decadal_ind[dd] + 0:9)] <- climate_dataset[ , paste0("y", 2091:2100)]
    #}

    shuffled_time_repeat <- c(2093, 2099, 2095, 2100, 2092, 2097, 2098, 2094,
    2091, 2096, 2100, 2097, 2099, 2095, 2096, 2091, 2093, 2098, 2094, 2092,
    2099, 2095, 2094, 2096, 2091, 2097, 2100, 2093, 2092, 2098, 2100, 2095,
    2098, 2094, 2093, 2097, 2092, 2099, 2096, 2091, 2097, 2098, 2099, 2093,
    2095, 2100, 2092, 2096, 2091, 2094, 2098, 2091, 2094, 2100, 2099, 2092,
    2093, 2096, 2095, 2097, 2100, 2094, 2091, 2096, 2095, 2093, 2092, 2099,
    2097, 2098, 2100, 2098, 2091, 2096, 2093, 2092, 2099, 2094, 2097, 2095,
    2094, 2097, 2095, 2098, 2093, 2092, 2096, 2099, 2100, 2091, 2097, 2095,
    2092, 2094, 2100, 2098, 2099, 2091, 2096, 2093, 2093, 2091, 2096, 2095,
    2097, 2099, 2098, 2092, 2094, 2100, 2097, 2100, 2098, 2096, 2091, 2094,
    2099, 2092, 2093, 2095, 2091, 2099, 2100, 2093, 2095, 2094, 2092, 2098,
    2096, 2097, 2094, 2097, 2095, 2099, 2092, 2098, 2096, 2093, 2100, 2091,
    2094, 2098, 2093, 2097, 2092, 2100, 2096, 2095, 2091, 2099, 2095, 2091,
    2096, 2100, 2094, 2097, 2093, 2092, 2098, 2099, 2091, 2094, 2092, 2097,
    2096, 2100, 2098, 2093, 2099, 2095, 2096, 2091, 2094, 2093, 2098, 2097,
    2092, 2100, 2095, 2099, 2098, 2091, 2100, 2092, 2097, 2094, 2096, 2093,
    2099, 2095, 2095, 2096, 2091, 2100, 2099, 2093, 2094, 2092, 2097, 2098)

    # Fill 2101-2300 with shuffled decade
    climate_dataset[ , paste0("y", 2101:2300) ] <- climate_dataset[ , paste0("y", shuffled_time_repeat)]

  }

  # Only need scenario, GCM, and date range of simulations
  return( climate_dataset[, c("scenario", "GCM", paste0("y", first_year:final_year)) ] )

}
