#' load_obs: read observational data.
#'
#' @description
#' Load IMBIE or Hugonnet et al. observations:
#' ICE SHEETS: IMBIE FOR AR6 1992-2020
#' Downloaded from https://ramadda.data.bas.ac.uk/repository/entry/show?entryid=77b64c55-7166-4a06-9def-2e400398e452
#' on 28/3/23 via doi https://doi.org/10.5285/77B64C55-7166-4A06-9DEF-2E400398E452
#' in IMBIE preprint https://essd.copernicus.org/preprints/essd-2022-261/essd-2022-261.pdf
#'
#' GLACIERS: Hugonnet et al. (2021) regional files
#' https://www.sedoo.fr/theia-publication-products/?uuid=c428c5b9-df8f-4f86-9b75-e04c778e29b9
#'
#' @returns `load_obs()` returns annual observations with columns ["Year", "SLE",
#' "SLE_sd"]
#'
#' @export


load_obs <- function() {

  # GLA -----------------------------------------------------------------------
  if (i_s == "GLA") {

    cat("\nload_obs: requested glacier_data is", glacier_data, "\n", file = logfile_build, append = TRUE)

    # Hugonnet -----
    if (glacier_data == "Hugonnet") {

      # Extended Data Table file
      obs_filename <- sprintf("%s/GLA/Hugonnet/ED_table_1_2_data.csv", inputs_ext)

      cat("\nload_obs: reading observations file\n", obs_filename, file = logfile_build, append = TRUE)
      obs_file <- read.csv(obs_filename)

      # Select region
      obs_file <- obs_file[ obs_file$reg == reg_num, ]

      # Select columns: time periods and mass change (Gt)
      # obs_file <- obs_file[ , c("time", "dm", "err_dm") ]
      obs_file <- obs_file[ , c("period", "dmdt", "err_dmdt") ]

      # Total mass loss for period - 20 year mean
      obs_file[ obs_file$period == "2000-01-01_2020-01-01", 2:3] <- 20.0* obs_file[ obs_file$period == "2000-01-01_2020-01-01", 2:3]

      # 5 year means
      obs_file[ obs_file$period == "2000-01-01_2005-01-01", 2:3] <- 5.0* obs_file[ obs_file$period == "2000-01-01_2005-01-01", 2:3]
      obs_file[ obs_file$period == "2005-01-01_2010-01-01", 2:3] <- 5.0* obs_file[ obs_file$period == "2005-01-01_2010-01-01", 2:3]
      obs_file[ obs_file$period == "2010-01-01_2015-01-01", 2:3] <- 5.0* obs_file[ obs_file$period == "2010-01-01_2015-01-01", 2:3]
      obs_file[ obs_file$period == "2015-01-01_2020-01-01", 2:3] <- 5.0* obs_file[ obs_file$period == "2015-01-01_2020-01-01", 2:3]

    } # unit conversion and rename columns happens below

    # GlaMBIE -----
    if (glacier_data == "GlaMBIE") {

      # Regional file
      obs_filename <- Sys.glob(sprintf("%s/GLA/GlaMBIE/GlaMBIE_Data_DOI_10.5904_wgms-glambie-2024-07/glambie_results_20240716/calendar_years/%i_*.csv",
                                       inputs_ext, reg_num))

      cat("\nload_obs: reading observations file\n", obs_filename, file = logfile_build, append = TRUE)
      obs_file <- read.csv(obs_filename)

      # Get mass changes (Gt)
      obs_file <- obs_file[ , c("end_dates", "combined_gt", "combined_gt_errors") ]

      # Cumulative sum of changes
      tmp1 <- cumsum(obs_file[,2])

      # Errors: square root of cumulative sum of squared errors
      tmp2 <- sqrt(cumsum(obs_file[,3]**2))

      # Add baseline row
      obs_file <- rbind(rep(NA, 3), obs_file)
      obs_file[1, 1] <- 2000
      obs_file[ , 2] <- c(0, tmp1)
      obs_file[ , 3] <- c(0, tmp2)

      # GlaMBIE quotes +/- 1.96 s.d. so convert this to 1 s.d.
      obs_file[ ,3] <- obs_file[ ,3] / 1.96

    } # conversion happens below

    # Convert Gt/yr to mm SLE
    obs_file[ , 2:3] <- obs_file[ , 2:3] / 362.5

    # Convert to sea level rise
    obs_file[ , 2] <- -1* obs_file[ , 2]

    # xxx change to Period or similar as some are multi-year
    names(obs_file) <- c("Year", "SLE", "SLE_sd")

  } else {

    # Ice sheets -----
    if (deliverable_test) {
      if (i_s == "GIS") obs_file <- read.csv(paste0(inputs_ext,"/GIS/IMBIE/imbie_greenland_2021_mm.csv"))
      if (i_s == "AIS") obs_file <- read.csv(paste0(inputs_ext,"/AIS/IMBIE/imbie_antarctica_2021_mm.csv"))
      obs_file <- read.csv(obs_filename)
    } else {
      if (i_s == "GIS") obs_filename <- paste0(inputs_ext,"/GIS/IMBIE/imbie3_greenland_Gt_partitioned.csv")
      if (i_s == "AIS") {
        if (reg == "ALL") obs_filename <- paste0(inputs_ext,"/AIS/IMBIE/imbie3_antarctica_Gt_partitioned.csv")
        if (reg == "WAIS") obs_filename <- paste0(inputs_ext,"/AIS/IMBIE/imbie3_west_antarctica_Gt_partitioned.csv")
        if (reg == "EAIS") obs_filename <- paste0(inputs_ext,"/AIS/IMBIE/imbie3_east_antarctica_Gt_partitioned.csv")
        if (reg == "PEN") obs_filename <- paste0(inputs_ext,"/AIS/IMBIE/imbie3_antarctic_peninsula_Gt_partitioned.csv")
      }
      cat("\nload_obs: reading observations file\n", obs_filename, file = logfile_build, append = TRUE)
      obs_file <- read.csv(obs_filename, check.names = FALSE)
    }

    # Pick columns and tidy names
    if (deliverable_test) {

      obs_file <- obs_file[ , c("Year","Cumulative.mass.balance..mm.", "Cumulative.mass.balance.uncertainty..mm.") ]
      names(obs_file)[2:3] <- c("SLE", "SLE_sd")

      # Uncertainties are negative relative to mean in IMBIE 2021
      obs_file[,3] <- -1.0 * obs_file[,3]

    } else {

      obs_file <- obs_file[ , c("Date","Cumulative mass balance anomaly (Gt)",
                                "Cumulative mass balance anomaly uncertainty (Gt)") ]

      # Get December rows for year total in cumulative sum
      # Rename with year
      if (i_s == "AIS" && reg == "ALL") {
        obs_file <- obs_file[ format(as.Date(obs_file[,1]),"%m") == 12,] # date format YYYY-MM-DD
        obs_file[,1] <- as.numeric(format(as.Date(obs_file[,1]),"%Y"))
      } else {
        obs_file <- obs_file[ format(as.Date(obs_file[,1],tryFormats = c("%d/%m/%Y")),"%m") == 12,] # DD/MM/YYYY
        obs_file[,1] <- as.numeric(format(as.Date(obs_file[,1],tryFormats = c("%d/%m/%Y")),"%Y"))
      }

      # Convert cumulative Gt mass change to mm SLE
      obs_file[,2:3] <- obs_file[,2:3] / 362.5
      obs_file[,2] <- -1.0 * obs_file[,2]

      names(obs_file) <- c( "Year", "SLE", "SLE_sd")

    }

    # Uncertainties are negative relative to mean in IMBIE
    obs_file[,3] <- -1 * obs_file[,3]

  } # ice sheets

  # Convert mm to cm SLE for all observations
  obs_file[,2:3] <- obs_file[,2:3] / 10

  return(obs_file)



}
