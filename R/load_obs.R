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

  if (i_s == "GLA") {

    # Read regional file
    reg_number <- strsplit(reg, split = "RGI")[[1]][2]
    obs_filename <- paste0(inputs_ext,"/GLA/Hugonnet/time_varying_glacier_areas/dh_", reg_number, "_rgi60_reg_cumul.csv")

    cat("\nload_obs: reading observations file\n", obs_filename, file = logfile_build, append = TRUE)
    obs_file <- read.csv(obs_filename)

    obs_file <- obs_file[ , c("time", "dm", "err_dm") ]

    # Get every 12th value because these have uncertainties - check this is right XXX
    obs_file <- obs_file[ 1 + (0:20*12), ]

    # Convert dates to round years for now
    # XXX Check this is OK - some dates are 31/12 not 1/1
    obs_file[ ,1] <- 2000:2020

    # Convert Gt/yr to cm mass loss
    obs_file[ , 2:3] <- obs_file[ , 2:3] / (10 * 362.5)
    # Convert to sea level rise
    obs_file[ , 2] <- -1* obs_file[ , 2]

    names(obs_file) <- c("Year", "SLE", "SLE_sd")

  } else {

    #  if (i_s == "GIS") obs_file <- read.csv(paste0(inputs_ext,"/GIS/IMBIE/imbie_greenland_2021_mm.csv"))
    #  if (i_s == "AIS") obs_file <- read.csv(paste0(inputs_ext,"/AIS/IMBIE/imbie_antarctica_2021_mm.csv"))

    if (i_s == "GIS") obs_filename <- paste0(inputs_ext,"/GIS/IMBIE/imbie3_greenland_partitioned_mm.csv")
    if (i_s == "AIS") obs_filename <- paste0(inputs_ext,"/AIS/IMBIE/imbie3_antarctica_partitioned_mm.csv")

    cat("\nload_obs: reading observations file\n", obs_filename, file = logfile_build, append = TRUE)
    obs_file <- read.csv(obs_filename)

    # Pick columns and tidy names
    # obs_file <- obs_file[ , c("Year","Cumulative.mass.balance..mm.", "Cumulative.mass.balance.uncertainty..mm.") ]
    # names(obs_file)[2:3] <- c("SLE", "SLE_sd")

    # XXX check when files final
    if (i_s == "AIS") obs_file <- obs_file[ , c("YYYY.MM.DD","Cumulative.mass.balance.anomaly..mm.", "Cumulative.mass.balance.anomaly.uncertainty..mm.") ]
    if (i_s == "GIS") obs_file <- obs_file[ , c("YYYY.MM.DD","Cumulative.mass.balance.anomaly..Gt.", "Cumulative.mass.balance.anomaly.uncertainty..Gt.") ]
    names(obs_file) <- c( "Year", "SLE", "SLE_sd")

    # Convert formats
    # Pick December months for years (previously used Jan in old IMBIE)
    obs_file <- obs_file[ format(as.Date(obs_file[,1]),"%m") == 12,]

    # Rename as annual
    obs_file[,1] <- as.numeric(format(as.Date(obs_file[,1]),"%Y"))

    # Uncertainties are negative in file xxx look at why and check Heiko method
    obs_file[,3] <- -1 * obs_file[,3]

  }

  # Convert mm to cm
  obs_file[,2:3] <- obs_file[,2:3] / 10

  return(obs_file)



}
