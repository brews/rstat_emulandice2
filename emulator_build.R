#' ---
#' title: "emulandice2: emulator_build"
#' output:
#'    html_notebook:
#'      toc: true
#'      number_sections: true
#'
#' ---
#_______________________________________________________________________________
# BUILD EMULATOR
#
# Writes RData file: paste0("outdir", out_name, "_EMULATOR.RData")
# in data-raw/ to be read by FACTS for predicting land ice contributions
# Other output is written to out/
#
# Set plot_level > 0 to output plot pdf files
#_______________________________________________________________________________

# SETUP ------------------------------------------------------------------------
#' # SETUP

# Get arguments from RScript
args <- commandArgs(TRUE)

# Defaults if no args set (used for testing and Markdown)
if (length(args) == 0) {

  warning("No arguments set - using defaults")
  i_s <- "AIS"
  reg_num <- 1
  final_year <- 2150

} else {

  # Ice source and final year ----------------------------------------------------------
  #' # Choose ice source and final year

  # Ice source
  i_s <- args[1]

  # Region number (only used by glaciers for now)
  reg_num <- as.numeric(args[2])

  # End year
  final_year <- as.numeric(args[3]) # if past 2100, applies model/ensemble selections later

}

# Check ice source name
stopifnot(i_s %in% c("GIS","AIS", "GLA"))

# Region is set here

# ICE SHEET SECTOR (when implemented xxx)
if (i_s %in% c("GIS", "AIS")) {
  reg <- "ALL"
  stopifnot(reg %in% c("ALL")) # will add basins
}

# RGI NUMBER
if (i_s == "GLA") reg <- paste0("RGI", sprintf("%02i", reg_num)) # zero-padded

# Check region name is valid
stopifnot(reg %in% c("ALL", paste0("RGI", sprintf("%02i",1:19))))

# Fix random seed
set.seed(2024)

# Directory for output files
rdatadir <- "./data-raw/" # RData file containing emulator
outdir <- "./out/" # Everything else

# Create directories if they do not exist (may not if running on HPC)
if ( ! file.exists(rdatadir) ) dir.create(file.path(rdatadir))
if ( ! file.exists(outdir) ) dir.create(file.path(outdir))

# Directories for input datasets
# (all in the same place, but grouped by type in case want to change structure)
inputs_preprocess <- paste0(system.file("extdata", package = "emulandice2"), "/")
inputs_ext <- inputs_preprocess

# Get configuration file for ice source region
config_filename <- paste0("config_",i_s,"_",reg,".yml")
config_file <- system.file(config_filename,
                           package = 'emulandice2', mustWork = TRUE)

# Analysis choices ------------------------------------------------------------------------

#' # Analysis choices
#' ## Dataset, ice source, region [ensemble]

# Switch to go back to deliverable settings for testing
deliverable_test <- config::get("deliverable_test", file = config_file)

# Just read, filter and plot simulations (for testing etc)
read_sims_only <- FALSE

# Impute missing years in simulations: currently just AIS to 2150
#impute_sims <- ifelse(i_s == "AIS" && final_year == "2150", TRUE, FALSE)
impute_sims <- "fill"
stopifnot(impute_sims %in% c("none", "fill", "extend"))
if (deliverable_test) impute_sims <- "none"

# This is how far the imputation will extend a simulation (or not)
# will only use the extend simulation if forcing exists (often doesn't)
if (impute_sims == "none") impute_nyrs <- 0
if (impute_sims == "fill") impute_nyrs <- 5  # restricted fill
if (impute_sims == "extend") impute_nyrs <- 50 # extended

# Later there are options to pick sub-ensembles (obsolete / not used?)
ensemble_subset <- NA


# ENSEMBLE DATA
# Main end dates of simulations in PROTECT ensembles

# If add new final_year option, need to add to sle_lim list for plots too (xxx add check)
# Currently two Greenland and glacier ensembles to choose from

if (i_s == "AIS") {
  stopifnot(final_year %in% c(2100, 2150, 2200, 2300))
  ensemble_subset <- "all_forced"
  stopifnot( ensemble_subset %in% c("GCM_forced", "RCM_forced", "all_forced")) # only RCM option used for now
}

if (i_s == "GIS") {
  stopifnot(final_year %in% c(2100, 2150, 2200, 2250, 2300))
}

if (i_s == "GLA") {

  ensemble_subset <- "PPE" # xxx Now ignored because ensembles are combined - keeping here for now
  stopifnot(ensemble_subset %in% c("forcing", "PPE"))

  if (ensemble_subset == "forcing" && final_year > 2100) {
    warning("ensemble_subset is set to 'forcing' so reducing final_year to 2100")
    final_year <- 2100
  }
  stopifnot(final_year %in% c(2100, 2150, 2300))
}

# Set max ensemble size for training GPs
# Uses minimum of this or 70% of dataset for train and test validation
# e.g. if 1000, then trains on 700 and uses 300 for testing
# if 1500, then trains on 1000 and uses 500 for testing
# if 2000, then still trains on 1000 and uses 1000 for testing
# Or can set to NA, e.g. for laGP which can handle large data
N_max_em <- 1000L

# Long names for outputs
if (i_s == "GIS") ice_name <- "Greenland"
if (i_s == "AIS") ice_name <- "Antarctica"
if (i_s == "GLA") {
  ice_name <- read.csv(paste0(inputs_ext,"/GLA/regionnames.txt"), header = FALSE)[reg_num,1]
}

# Sample size for unif_temps design - used for convenience when adding uncertainty
# (Main effects sample size is set in load_design_to_pred.R, and
# AR6 prior sample is equal to number of GSAT projections)
N_unif <- 2000L

# Do LOO validation?
validation_type <- config::get("validation_type", file = config_file)
stopifnot(validation_type %in% c("tvt", "loo"))

# Subsample for LOO
N_k <- NA # integer for every N_k-th simulation; NA for full LOO # xxx add switch by size?

# May as well switch on full LOO if GIS 2300 (quick)
if (deliverable_test) {
  if (i_s == "GIS" && final_year > 2200) {
    validation_type <- "loo"
    N_k <- NA
  }
}

print("************************************************************************************************")
print("Hello! Welcome to emulandice2: build")
print("************************************************************************************************")

print(paste(ice_name,"region",reg))
if (validation_type == "loo") {
  print(paste("LOO with N_k =",N_k,"(could be very slow)"))
}
print(paste0("Config file: ./inst/", config_filename))

#' ## Projection times and possible scenarios

# SIMULATION YEARS in dataset i.e. columns in CSV

# First year of simulations we want to use
# checks later this is within CSV file header range
if (i_s == "AIS") first_year <- 1950
if (i_s == "GIS") first_year <- 1960
if (i_s == "GLA") first_year <- 1980

years_sim <- first_year:final_year

# Timeslice frequency to predict after break year
# (see below)
nyrs <- 5

if (deliverable_test) nyrs <- 5

# Check reasonable choice
stopifnot(nyrs %in% c(1, 2, 5, 10))

# Full list of possible emissions scenarios to look for
# (dropped from unif_temps design if not simulated)
# over-recon is Heiko's reconstruction of SSP534-over forcing

# Scenario list for plots and projections (not selecting simulations)
# XXX SHOULD I ADD RCPS FOR GIS?? CHECK WHERE USED IN SIM PLOTS
scenario_list <- c("SSP119", "SSP126", "SSP245", "SSP370", "SSP534-over", "SSP534-over-recon", "SSP585")
if (deliverable_test) scenario_list <- c("SSP119", "SSP126", "SSP245", "SSP370", "SSP585")

#' ## Ice model(s)

if (i_s == "AIS") {

  # All models (do not change!)
  model_list_full <- c( "Kori", "PISM", "CISM", "ElmerIce", "BISICLES", "IMAUICE" )
  if (deliverable_test) model_list_full <- c( "Kori", "PISM", "CISM", "ElmerIce" )

  # Would drop short simulations anyway but early on is better for emulator inputs
  if (ensemble_subset == "GCM_forced" ||
      (ensemble_subset == "all_forced" && final_year > 2200) ) {
    model_list <- c( "Kori", "PISM", "BISICLES", "IMAUICE" )
    if (deliverable_test) model_list <- c( "Kori", "PISM")
  } else model_list <- model_list_full

  model_list <- model_list_full

}

if (i_s == "GIS") {

  # All models (do not change!)
  model_list_full <- c( "CISM", "IMAUICE", "ElmerIce", "GISM" )

  # Pick models to use: full list or CISM only
  if ( final_year <= 2100 ) model_list <- model_list_full
  if ( final_year > 2100 ) model_list <- "CISM"

  # If ElmerIce: change cal range later to 1992-2014 (if 2 yr timeslices)

  # Flag to require matching historical + projection retreat values in select_sims()
  # for CISM runs only
  # Since 250719, have excluded all but a few 2300 for keeping: so set to FALSE
  need_retreat_match <- FALSE
  if (deliverable_test) need_retreat_match <- TRUE

  # Only CISM went beyond 2100 (at all / to any great extent)
  if ( final_year > 2100 &&
       ( length(model_list) > 1 ||
         (length(model_list) == 1 && model_list != "CISM") ) ) return()

}

if (i_s == "GLA") {

  # All models (do not change!)
  model_list_full <- c( "GloGEM", "OGGM") # "GO" )

  # Pick models, or set to model_list_full to use all
  model_list <- model_list_full

  # Fraction of glaciers that must have completed (guidance from Fabien Maussion)
  # Selection is done in select_sims()
  # Only OGGM and GO have completion % information, not GloGEM
  # Some regions only reach ~92% so can't go higher without adjusting
  # XXX Later change to 0.8 for GO Russian Arctic and AIS peripherals, 0.9 otherwise?
  complete_thresh <- 0.95 # NA to not use
  if (reg %in% c("RGI16", "RGI18")) complete_thresh <- 0.90
  if (deliverable_test) complete_thresh <- 0.80

}

# Check selected model names are correct
stopifnot( length( setdiff(model_list, model_list_full )) == 0 )

# Emulator choices ------------------------------------------------------------------------

# Stationary (RobustGaSP) or deep Gaussian Process emulator
emulator_type <- config::get("emulator_type", file = config_file)
stopifnot(emulator_type %in% c("statGP", "laGP", "deepgp"))

N_mcmc <- NA
if (emulator_type == "deepgp") N_mcmc <- 100L

#' ## Set emulator covariance function
# Choose emulator covariance function here so can put in output name for now

if (emulator_type == "statGP") {

  # Can choose matern_5_2, matern_3_2,
  # or pow_exp (power-exponential with alpha = 0.1, 1.0, 1.9, 2.0)
  # Could add

  # XXX Specify by ice sheet sector later if using
  emulator_covar <- config::get("emulator_covar", file = config_file)

  stopifnot(emulator_covar %in% c("matern_5_2", "matern_3_2",
                                  "pow_exp_01", "pow_exp_10",
                                  "pow_exp_19", "pow_exp_20"))
}

if (emulator_type == "deepgp") {

  # Squared exponential ("gauss" in RobustGaSP) or Matern
  # Matern smoothness is v=2.5 by default in deepgp, i.e. matern_5_2
  emulator_covar <- "matern" # exp2"
  stopifnot(emulator_covar %in% c("exp2", "matern"))

}

# Set here because of conditionals in make_emu.R
laGP_scaling <- FALSE

if (emulator_type == "laGP") {

  emulator_covar <- "exp2" # Just used for naming etc for now; Gaussian is default in laGP
  stopifnot(emulator_covar == "exp2")

  laGP_method <- "alcray" # alc, alcray (faster but worse)
  laGP_nugget_prior <- 0.1 # prior value for nuggets

}

#' ## Open output file

# Create name for output files
out_name <- paste0(i_s,"_",reg,"_",paste(model_list, collapse = "_"),
                   "_", emulator_covar)
logfile_build <- paste0(outdir, out_name,"_build.txt")

#______________________________________________________
# START WRITING LOG FILE
cat("_____________________________________\n", file = logfile_build)

cat(paste("LAND ICE SOURCE:", ice_name, reg, "\n"), file = logfile_build, append = TRUE)

cat(paste0("\nConfig file: ./inst/", config_filename, "\n\n"), file = logfile_build, append = TRUE)

if (deliverable_test) cat(paste("\nPROTECT deliverable settings\n"), file = logfile_build, append = TRUE)
if (impute_sims != "none") {
  cat(paste("Impute missing data in simulations:", impute_sims, "\n"), file = logfile_build, append = TRUE)
  cat(paste("including extension of timeseries by up to", impute_nyrs, "years\n"), file = logfile_build, append = TRUE)
}
cat( paste("\nEnsemble subset:", ensemble_subset,"\n"), file = logfile_build, append = TRUE)
cat(paste( "MODELS:", paste(model_list, collapse = ", "), "\n"), file = logfile_build, append = TRUE)
cat(paste("\nDate range of simulations to be used:",
          first_year,"-", final_year, "\n"),
    file = logfile_build, append = TRUE)
cat(paste("\nEmulator type:", emulator_type), file = logfile_build, append = TRUE)
cat(paste("\nEmulator covariance:", emulator_covar), file = logfile_build, append = TRUE)
if (emulator_type == "deepgp") cat(paste("\nN_MCMC:", N_mcmc, "\n"), file = logfile_build, append = TRUE)
if (emulator_type == "laGP") {
  cat("\nlaGP scaling: ", laGP_scaling, "\n", file = logfile_build, append = TRUE)
  cat("laGP method: ", laGP_method, "\n", file = logfile_build, append = TRUE)
  cat("laGP nugget prior: ", laGP_nugget_prior, "\n", file = logfile_build, append = TRUE)
}
cat(paste("\nValidation type:", validation_type), file = logfile_build, append = TRUE)
cat("\n", file = logfile_build, append = TRUE)

#' ## Glacier maximum contributions
# Get glacier cap --------

if (i_s == "GLA") glacier_cap <- emulandice2::get_glacier_cap(reg)

# Calibration dates --------
#' ## Baseline and calibration dates

# Ice sheets: Otosaka et al. (2023) IMBIE is 1992-2020
# Glaciers: Hugonnet et al. (2021) is 2000-2020
if (i_s == "AIS") cal_end <- 2020
if (i_s == "GIS") cal_end <- 2021 # xxx to 2023 when imbie3
if (i_s == "GLA") cal_end <- 2020 # because OGGM fails if too early xxx obsolete?

if (deliverable_test) cal_end <- 2020

# Start of calibration period
# xxx Note cal_start MUST be same as baseline in current code (and makes sense)

# PROTECT: Earliest Greenland = 1960, Antarctic = 1950, glaciers = 1980

# XXX Implement different baselines for 2100/50 and 2300?

# Antarctica
if (i_s == "AIS") {
  cal_start <- 2015 # BISICLES starts in 2007; IMAUICE in 2014
  if (deliverable_test) cal_start <- 2000
}

# Greenland
if (i_s == "GIS") {
  if (final_year > 2100) { # CISM overlap with IMBIE3
    cal_start = 1972 # 1971 when final IMBIE3
  } else {
    cal_start = 1995 # Elmer/Ice overlap with IMBIE3
  } # xxx change to 1995 when decoupled baseline
  if (deliverable_test) cal_start <- 2000
}

# Glaciers
if (i_s == "GLA") cal_start = 2000

# Checks for current data ranges: better to check against data file! xxx

# IMBIE and GLAMBIE end in 2023
stopifnot(cal_end <= 2023)

# IMBIE3 start dates
if (i_s == "AIS") stopifnot( cal_start >= 1979 )
if (i_s == "GIS") stopifnot( cal_start >= 1971 )

# GLAMBIE start date
if (i_s == "GLA") stopifnot( cal_start >= 2000 )

# Construct emulated time series
if (deliverable_test) {
  break_yr <- NA
  proj_start <- cal_start + nyrs
  years_em <- seq( from = proj_start, by = nyrs, to = final_year )
} else {
  break_yr <- 2030 # end of annual frequency for emulation
  years_em <- c( (cal_start + 1):break_yr-1, seq( from = break_yr, by = nyrs, to = final_year ))
}

# BISICLES tests xxx drop a couple
#years_em <- years_em[ -(length(years_em)-1) ]
#years_em <- years_em[ -(length(years_em)-2) ]
#years_em <- years_em[ -(length(years_em)-4) ]

# Basic check not done something daft with timeslices
stopifnot(2100 %in% years_em)

# End of calibration is in projection period, so check we are predicting this year
stopifnot(cal_end %in% years_em)

if (deliverable_test) {
  cat( paste("Predicting every", nyrs, "years from",
             years_em[1], "to", years_em[length(years_em)], "\n"),
       file = logfile_build, append = TRUE)
} else {
  cat( paste("Predicting annually from", years_em[1], "to", break_yr,
             "then every", nyrs, "years to", years_em[length(years_em)], "\n"),
       file = logfile_build, append = TRUE)
}
cat(paste("with respect to year", cal_start, "\n"), file = logfile_build, append = TRUE)

N_ts <- length(years_em)
cat(paste("Timeslices:", N_ts, "\n"), file = logfile_build, append = TRUE)

#' ## Leave-one-out (LOO) validation choices
validation_years <- c( cal_end, 2050, 2100, 2150, 2200, 2300)
cat(paste("Validation years:", paste(validation_years, collapse = ",")), "\n", file = logfile_build, append = TRUE)

# (Checks these years are emulated later)

#' ## Emulation input choices

# Emulator settings ------------------------------------------------------------
#_______________
cat("\nEMULATOR INPUTS:\n", file = logfile_build, append = TRUE)

# // Temps ------------------------------------------------------------

# GSAT timeslices for ice_design
# XXX consider going back earlier?

temps_baseline <- 2015

# Not too many, to avoid linear combinations (esp bad for fixed climate GIS) or overfitting
# Altered below if request shorter projections e.g. to 2150 only
if (i_s == "AIS") temps_list <- 2300
if (i_s == "GIS") {
  temps_list <- 2100
  if (deliverable_test) temps_list <- 2100
}
if (i_s == "GLA") {
  temps_list <- c(2100, 2300)
  if (deliverable_test) temps_list <- 2300
}

# Number of years to average over
# e.g. setting 10 with temps_list = 2300 and temps_baseline = 2015
# gives decadal mean 2291-2300 relative to 2015-2024
N_temp_yrs <- 30

cat(paste("GSAT baseline first year:", temps_baseline, "\n"), file = logfile_build, append = TRUE)
cat(paste("GSAT final year(s):", paste(temps_list, collapse = ","), "\n"), file = logfile_build, append = TRUE)
if (max(temps_list) > final_year) {
  cat("GSAT timeslice(s) extend beyond ice model simulation: adjusting\n", file = logfile_build, append = TRUE)
  temps_list <- temps_list[ temps_list <= final_year ]
  if (length(temps_list) == 0) temps_list <- final_year
  cat(paste("New GSAT input timeslice(s):", paste(temps_list, collapse = ","), "\n"), file = logfile_build, append = TRUE)
}
cat(paste("GSAT period:", N_temp_yrs, "years\n"), file = logfile_build, append = TRUE)

# xxx Can use this elsewhere! e.g. plot_design.R instead of reconstructing
temps_list_names <- paste0("GSAT_", temps_list)

# // Ice model params ----------------------------------------------------------

# Ice model parameters for ice_design


if (i_s == "AIS") {

  ice_cont_list_model <- list()
  ice_factor_list_model <- list()

  # Kori: all
  ice_cont_list_model[["Kori"]] <- c("heat_flux_PICO", "heat_flux_Plume", "heat_flux_ISMIP6_local",
                                     "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope")
  ice_factor_list_model[["Kori"]] <- c("melt_param")

  # Kori GCM-forced only
  if ( ensemble_subset %in% c("GCM_forced", "all_forced") ) {
    ice_cont_list_model[["Kori"]] <- c(ice_cont_list_model[["Kori"]],
                                       "lapse_rate", "PDD_ice", "PDD_snow", "refreeze")


    ice_factor_list_model[["Kori"]] <- c( ice_factor_list_model[["Kori"]],
                                          "init_atmos", "init_ocean")

  }

  # Kori RCM-forced only
  if ( ensemble_subset %in% c("RCM_forced", "all_forced") ) {
    ice_cont_list_model[["Kori"]] <- c(ice_cont_list_model[["Kori"]],
                                       "sliding_exponent")
  }

  # PISM
  ice_cont_list_model[["PISM"]] <- c( "heat_flux_PICO" )

  # PISM GCM-forced only
  if ( ensemble_subset %in% c("GCM_forced", "all_forced") ) {
    ice_cont_list_model[["PISM"]] <- c(ice_cont_list_model[["PISM"]],
                                       "sliding_exponent",
                                       "lapse_rate",  "refreeze_frac",
                                       "PDD_ice", "PDD_snow")
    ice_factor_list_model[["PISM"]] <- c( "init_atmos" )
  }

  # PISM RCM-forced only
  if ( ensemble_subset == "RCM_forced" ||
       (ensemble_subset == "all_forced" && final_year <= 2200) ) {
    ice_cont_list_model[["PISM"]] <- c(ice_cont_list_model[["PISM"]],
                                       "overturning_PICO",
                                       "tillwater_decay_rate",
                                       "eff_fraction_overburden_pressure")
  }

  # PISM different resolution between the two
  if ( ensemble_subset == "all_forced" && final_year <= 2200 ) {
    ice_cont_list_model[["PISM"]] <- c(ice_cont_list_model[["PISM"]],
                                       "resolution")
  }


  # CISM
  ice_cont_list_model[["CISM"]] <- c( "resolution",
                                      "heat_flux_ISMIP6_nonlocal",
                                      "heat_flux_ISMIP6_nonlocal_slope")

  # Local is only varied in CISM for runs to 2100
  # but these are imputed to 2150 if impute_sims = "extend"
  if (final_year == "2100" || impute_sims == "extend") ice_cont_list_model[["CISM"]] <- c(ice_cont_list_model[["CISM"]],
                                                                                          "heat_flux_ISMIP6_local")
  ice_factor_list_model[["CISM"]] <- c("melt_param", "sliding_law")

  # Elmer/Ice
  ice_cont_list_model[["ElmerIce"]] <- c("heat_flux_PICO", "sliding_exponent")

  # BISICLES
  ice_cont_list_model[["BISICLES"]] <- "heat_flux_ISMIP6_nonlocal"
  ice_factor_list_model[["BISICLES"]] <- c("shelf_collapse", "sliding_law")

  # IMAUICE
  ice_factor_list_model[["IMAUICE"]] <- "GIA"

  # Combine model lists
  ice_cont_list <- NA
  ice_factor_list <- NA

  for (mm in model_list) {
    if (length(ice_cont_list_model[[mm]]) > 0) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[[mm]])
    if (length(ice_factor_list_model[[mm]]) > 0) ice_factor_list <- c(ice_factor_list, ice_factor_list_model[[mm]])
  }

  # If both models present, can also include this
  # as they use different values
  # i.e. this covers GCM-forced 2300
  if ("Kori" %in% model_list && "PISM" %in% model_list) {
    ice_cont_list <- c(ice_cont_list, "overturning_PICO")
  }

  # Drop NA and duplicates
  ice_cont_list <- unique( ice_cont_list[ -1 ] )
  ice_factor_list <- unique( ice_factor_list[ -1 ] )

  # Combine RCM and GCM-forced
  if ( ensemble_subset == "all_forced" && final_year <= 2200 ) {
    ice_factor_list <- c(ice_factor_list, "forcing_type")
  }

  # Add RCM factor (will fail if only using Elmer/Ice)
  # xxx need to add something to
  if ( (ensemble_subset == "all_forced" && final_year <= 2200) ||
       (ensemble_subset == "RCM_forced" && final_year == 2100)) {
    ice_factor_list <- c(ice_factor_list, "RCM")
  }


  # Add model switch and GCM vs RCM-forced factor:
  if ( length(model_list) > 1 ) ice_factor_list <- c(ice_factor_list, "model")

}

# Continuous and categorical (factor) model inputs
if (i_s == "GIS") {

  # xxx Drop SP_climate column - not used and has missing
  # xxx Ignoring retreat_hist for now

  # Individual model lists
  # No factors for GISM
  # xxx Make init_yrs continuous?
  ice_factor_list_model <- list()
  ice_factor_list_model[["CISM"]] <- c("thermodyn", "RCM_init", "init_yrs", "elev_feedback")
  ice_factor_list_model[["IMAUICE"]] <- c("sliding")
  ice_factor_list_model[["ElmerIce"]] <- c("sliding")

  # Combined model lists
  # Continuous parameters
  ice_cont_list <- c("retreat", "resolution")

  # Factors
  # Ignore model_variant as this (sub-name) should be accounted for by other inputs
  ice_factor_list <- "RCM"
  for (mm in model_list) {
    if (length(ice_factor_list_model[[mm]]) > 0) ice_factor_list <- c(ice_factor_list, ice_factor_list_model[[mm]])
  }

  # Drop duplicates
  ice_factor_list <- unique( ice_factor_list )

  # Add model input
  if (length(model_list) > 1) ice_factor_list <- c(ice_factor_list, "model")

}

if (i_s == "GLA") {

  ice_cont_list_model <- list()

  # GloGEM
  ice_cont_list_model[["GloGEM"]] <- c("prec_corr_factor", "ddf_ice",
                                       "ratio_ddf_ice_to_snow",
                                       "prec_gradient" )

  # OGGM
  ice_cont_list_model[["OGGM"]] <- c("prec_corr_factor", "ddf_ice",
                                     "temp_melt", "temp_bias", "glen_a")

  # GO
  ice_cont_list_model[["GO"]] <- c("prec_corr_factor", "ice_albedo",
                                   "temp_sens", "psi_constant","trans",
                                   "t_tip", "t_phase")

  # Combine
  ice_cont_list <- NA
  if ("GloGEM" %in% model_list) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[["GloGEM"]])
  if ("OGGM" %in% model_list) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[["OGGM"]])
  if ("GO" %in% model_list) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[["GO"]])
  ice_cont_list <- ice_cont_list[-1]
  ice_cont_list <- unique( ice_cont_list )

  # Factors
  ice_factor_list <- NA

  # Ensemble is for any setup differences, e.g.:
  # For OGGM, forcing uses reanalysis 2000-2020 and parameter uses GM
  # For GloGEM, forcing parameters are regional means over glaciers but
  # parameter ensemble has same value everywhere

  # If using both ensembles xxx check if this should be 2150 if/when imputing??
  # could just check dataset for ensemble == forcing xxx

  if (final_year <= 2100) ice_factor_list <- c(ice_factor_list, "ensemble")

  # Multiple models
  if (length(model_list) > 1) ice_factor_list <- c(ice_factor_list, "model")

  # Drop initial NA if added any
  if ( length(ice_factor_list > 1) ) ice_factor_list <- ice_factor_list[-1]

  #  if (length(model_list) > 1) { ice_factor_list <- "model"
  #  } else ice_factor_list <- NA

}

cat(paste("\nContinuous inputs:", paste(ice_cont_list, collapse = " "), "\n"), file = logfile_build, append = TRUE)

# XXX add check that not NA or single value column
# e.g. sliding for CISM is always Schoof

# Combine lists
# assumes always have at least 1 continuous
# but factor might be NA
# Save whether any factors for other uses
if (anyNA(ice_factor_list)) {
  include_factors <- FALSE
  ice_param_list <- ice_cont_list
} else {
  include_factors <- TRUE
  ice_param_list <- c(ice_cont_list, ice_factor_list)
}

if (include_factors) {
  cat(paste("Factors:", paste(ice_factor_list, collapse = " "), "\n"), file = logfile_build, append = TRUE)
}

#' ## Emulator details

if (emulator_type == "statGP") {

  # Could set to FALSE if want to check for inert inputs
  lower_bound <- TRUE # RobustGaSP default = TRUE
  alpha = NA

  # Matern
  if (emulator_covar %in% c("matern_5_2", "matern_3_2")) kernel <- emulator_covar

  # Power exponential
  if (emulator_covar == "pow_exp_01") {
    kernel <- "pow_exp"
    alpha = 0.1
  }
  if (emulator_covar == "pow_exp_10") {
    kernel <- "pow_exp"
    alpha = 1.0
  }
  if (emulator_covar == "pow_exp_19") {
    kernel <- "pow_exp"
    alpha = 1.9 # default for pow_exp
  }
  if (emulator_covar == "pow_exp_20") {
    kernel <- "pow_exp"
    alpha = 2.0
  }

  stopifnot(kernel %in% c("pow_exp", "matern_5_2", "matern_3_2"))

}

if (emulator_type == "deepgp") {
  # Placeholder if I want to set matern smoothness later
}

# Plot: choices ------------------------------------------------------------
#' ## Plot choices

# Plot all or just subset of figures
# 0 for none, 1 for main, 2 for exhaustive
plot_level <- 2

stopifnot(plot_level %in% c(0,1,2)) # using plot_level = 3 to distinguish main.R calls

# Sub-sample to plot; exclude any dates not predicted by emulator
yy_plot <- c(as.character(cal_end),"2100", "2150", "2200", "2300")
yy_plot <- yy_plot[ yy_plot %in% years_em ]

# Same for LOO timeslices
validation_years <- validation_years[ validation_years %in% years_em ]
if (length(validation_years) == 0 ) warning("None of the requested validation years are in predictions")

# Match short and full scenario names for plots
# xxx ADD RCPs?
scen_name <- list()
for (scen in scenario_list) {
  tmp <- strsplit(scen, split="")[[1]]
  scen_name[[scen]] <- paste( c(tmp[1:4], "-", tmp[5], ".", tmp[6]), collapse = "")

  if (scen %in% c("SSP534-over", "SSP534-over-recon")) scen_name[[scen]] <- "SSP5-3.4-OS"
}

# Plot limits for each yy_plot timeslice
sle_lim <- list()
sle_inc <- list()

if (i_s == "AIS") {
  sle_lim[[as.character(cal_end)]] <- c(-4, 8); sle_inc[[as.character(cal_end)]] <- 0.5
  sle_lim[["2050"]] <- c(-10, 90); sle_inc[["2050"]] <- 2
  sle_lim[["2100"]] <- c(-70, 170); sle_inc[["2100"]] <- 5
  sle_lim[["2150"]] <- c(-150, 300); sle_inc[["2150"]] <- 5
  sle_lim[["2200"]] <- c(-250, 500); sle_inc[["2200"]] <- 10
  sle_lim[["2250"]] <- c(-250, 800); sle_inc[["2200"]] <- 10
  sle_lim[["2300"]] <- c(-300, 1000); sle_inc[["2300"]] <- 20
}

if (i_s == "GIS") {
  sle_lim[[as.character(cal_end)]] <- c(-1, 2); sle_inc[[as.character(cal_end)]] <- 0.1
  sle_lim[["2050"]] <- c(-1, 10); sle_inc[["2050"]] <- 0.5
  sle_lim[["2100"]] <- c(-20, 40); sle_inc[["2100"]] <- 1
  sle_lim[["2150"]] <- c(-50, 100); sle_inc[["2150"]] <- 2
  sle_lim[["2200"]] <- c(-100, 220); sle_inc[["2200"]] <- 5
  sle_lim[["2250"]] <- c(-150, 350); sle_inc[["2200"]] <- 5
  sle_lim[["2300"]] <- c(-200, 450); sle_inc[["2300"]] <- 10
}


if (i_s == "GLA") {

  # Large regions (>1cm)
  # Checked with region 17; special limits for other large regions below
  if (glacier_cap >= 1.0) {
    sle_lim[[as.character(cal_end)]] <- c(-1, 2); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-1, glacier_cap); sle_inc[["2050"]] <- 0.1
    sle_lim[["2100"]] <- c(-2, 1.5*glacier_cap); sle_inc[["2100"]] <- 0.1
    sle_lim[["2150"]] <- c(-3, 2*glacier_cap); sle_inc[["2150"]] <- 0.1
    sle_lim[["2200"]] <- c(-5, 2*glacier_cap); sle_inc[["2200"]] <- 0.1
    sle_lim[["2300"]] <- c(-5, 2*glacier_cap); sle_inc[["2300"]] <- 0.1
  }

  # Adjust lower end for dinky glacier regions (< 1cm)
  if (glacier_cap < 1.0) {
    sle_lim[[as.character(cal_end)]] <- c(-0.1, 1); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-0.005, glacier_cap); sle_inc[["2050"]] <- 0.1
    sle_lim[["2100"]] <- c(-0.005, 1.1*glacier_cap); sle_inc[["2100"]] <- 0.1
    sle_lim[["2150"]] <- c(-0.005, 1.3*glacier_cap); sle_inc[["2150"]] <- 0.1
    sle_lim[["2200"]] <- c(-0.01, 1.4*glacier_cap); sle_inc[["2200"]] <- 0.1
    sle_lim[["2300"]] <- c(-0.01, 1.5*glacier_cap); sle_inc[["2300"]] <- 0.1
  }

  # Specific region over-rides
  if (reg == "RGI01") {
    sle_lim[[as.character(cal_end)]] <- c(-0.1, 1); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-10, 1.5*glacier_cap); sle_inc[["2050"]] <- 0.5
    sle_lim[["2100"]] <- c(-10, 1.5*glacier_cap); sle_inc[["2100"]] <- 0.5
    sle_lim[["2150"]] <- c(-15, 2*glacier_cap); sle_inc[["2150"]] <- 0.5
    sle_lim[["2200"]] <- c(-25, 2*glacier_cap); sle_inc[["2200"]] <- 1
    sle_lim[["2300"]] <- c(-5, 2*glacier_cap); sle_inc[["2300"]] <- 0.2 # -70, 1 XXX why was it so big ??
  }
  if (reg == "RGI19") {
    sle_lim[[as.character(cal_end)]] <- c(-0.1, 1); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-10, glacier_cap); sle_inc[["2050"]] <- 0.5
    sle_lim[["2100"]] <- c(-20, 1.1*glacier_cap); sle_inc[["2100"]] <- 0.5
    sle_lim[["2150"]] <- c(-25, 1.3*glacier_cap); sle_inc[["2150"]] <- 0.5
    sle_lim[["2200"]] <- c(-35, 1.4*glacier_cap); sle_inc[["2200"]] <- 1
    sle_lim[["2300"]] <- c(-50, 1.5*glacier_cap); sle_inc[["2300"]] <- 1
  }
}

# IPCC AR6 colours
# e.g. see
# https://www.ipcc.ch/site/assets/uploads/2022/09/IPCC_AR6_WGI_VisualStyleGuide_2022.pdf
# https://github.com/IPCC-WG1/colormaps/blob/master/categorical_colors.xlsx

AR6_rgb <- list()
AR6_rgb[["SSP119"]] <- rgb(0, 173, 207, maxColorValue = 255)
AR6_rgb[["SSP126"]] <- rgb(23, 60, 102, maxColorValue = 255)
AR6_rgb[["SSP245"]] <- rgb(247, 148, 32, maxColorValue = 255)
AR6_rgb[["SSP370"]] <- rgb(231, 29, 37, maxColorValue = 255)
AR6_rgb[["SSP585"]] <- rgb(149, 27, 30, maxColorValue = 255)

AR6_rgb[["RCP26"]] <- rgb(23, 60, 102, maxColorValue = 255)
AR6_rgb[["RCP85"]] <- rgb(149, 27, 30, maxColorValue = 255)

# 60% transparency
AR6_rgb_med <- list()
alpha_med <- 153
AR6_rgb_med[["SSP119"]] <- rgb(0, 173, 207, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP126"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP245"]] <- rgb(247, 148, 32, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP370"]] <- rgb(231, 29, 37, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP585"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_med)

AR6_rgb_med[["RCP26"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["RCP85"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_med)

# 10% transparency; 20% = 51
AR6_rgb_light <- list()
alpha_light <- 51
AR6_rgb_light[["SSP119"]] <- rgb(0, 173, 207, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP126"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP245"]] <- rgb(247, 148, 32, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP370"]] <- rgb(231, 29, 37, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP585"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_light)

AR6_rgb_light[["RCP26"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["RCP85"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_light)

# Overshoot colour
# According to
# https://pyam-iamc.readthedocs.io/en/stable/tutorials/ipcc_colors.html
AR6_rgb[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255)
AR6_rgb_med[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_light[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_light)

AR6_rgb[["SSP534-over-recon"]] <- rgb(146, 57, 122, maxColorValue = 255)
AR6_rgb_med[["SSP534-over-recon"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_light[["SSP534-over-recon"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_light)

# PLOT RANGES

# SL simulations are up with glacier cap

# Observations
if (i_s == "GIS") ylim_obs <- c(-1.5,2)
if (i_s == "AIS") ylim_obs <- c(-10,12)

# Hugonnet for glaciers, not IMBIE!
# and do list or similar for regions
if (i_s == "GLA") {
  ylim_obs <- c(-1.5,2)
  if (reg %in% c("RGI12", "RGI18")) ylim_obs <- c(-0.01,0.03)
}


# ________________----
# START ------------------------------------------------------------
#' ## START


#' # Load and process data

#' ## Load observations
# Load obs -------------------------------------------------------------------

# Needs to be before select_sims for history matching filtering of glaciers
obs_data <- emulandice2::load_obs()

#' ## Load climate and ice simulations
# Load sims: climate ---------------------------------------------------------------------

# GET CLIMATE SIMULATIONS

# Returns CSV file data
climate_csv <- emulandice2::load_sims(variable = "climate")

# Fill any missing final year values (all rows in climate file - not very efficient)
climate_data <- impute_climate(climate_csv)

# XXX DROP ROWS NOT IN CSV HERE I THINK

# Calculate climate change timeslice(s) e.g. GSAT_2100 for emulator input(s)
# Also for all rows in climate data - not very effcient
# Option to add ensemble mean for each SSP for missing forcings
# (for imputing to extend ice simulations later)
impute_gcms <- ifelse(impute_sims == "extend", TRUE, FALSE)
temps_data <- emulandice2::calc_temps(climate_data, mean_impute = impute_gcms)

# For GIS post-2100, repeat with fixed climate forcings
# No need to set mean_impute, because this is already filling in [?]
if ( i_s == "GIS" && final_year > 2100) {
  climate_data_fixed <- impute_climate(climate_csv, construct_fixed = TRUE)
  temps_data_fixed <- emulandice2::calc_temps(climate_data_fixed)
}


# Load sims: ice ---------------------------------------------------------------------
# GET ICE SIMULATIONS

ice_data <- emulandice2::load_sims(variable = "ice", source = i_s, region = reg) # ice dataset

# Index of first column with name format of yXXXX
ice_file_yr_start_col <- suppressWarnings( myind <- min(which( nchar(names(ice_data)) == 5
                                                               & substr(names(ice_data), start = 1, stop = 1) == "y"
                                                               & !is.na(as.numeric(substr(names(ice_data), start = 2, stop = 5)) ) ) ) )
# Get full data year range: i.e. from this col to last
#ice_file_yr_start <- as.numeric(substr(names(ice_data)[ice_file_yr_start_col], 2, 5))
#ice_file_yr_end <- as.numeric(substr(names(ice_data)[length(names(ice_data))], 2, 5))

# Check requested years are within file year range from these columns
stopifnot(first_year >= as.numeric(substr(names(ice_data)[ ice_file_yr_start_col ], 2, 5)) &&
            final_year <= as.numeric(substr(names(ice_data)[ length(names(ice_data)) ], 2, 5)) )

# Get column number of first ice model input
# which is first one after this list
# Note GCM is included in this list for plots but isn't considered an ice model input elsewhere
ice_param_col_1 <- max(which(c("ice_source", "region", "scenario") %in% names(ice_data))) + 1
# removed "group", "model" so can use as factors

# Get column number of last ice model input
# which is just before first column in y**** format (**** is numeric)
ice_param_col_2 <- ice_file_yr_start_col - 1

# Check requested ice model inputs are in this
ice_param_list_full <- names(ice_data[ice_param_col_1:ice_param_col_2])

cat("\nDeduced ice model inputs from CSV header:\n", file = logfile_build, append = TRUE)
cat(paste(paste(ice_param_list_full, collapse = " "), "\n"), file = logfile_build, append = TRUE)
stopifnot(ice_param_list %in% ice_param_list_full)

# Select sims ---------------------------------------------------------------------

# Rename overshoot to something more clear in context: reconstructed overshoot
if (i_s == "GIS") {
  ice_data[ ice_data$scenario == "SSP585-o2300", "scenario" ] <- "SSP534-over-recon"
}

# Select ice source, region, model(s) and any other exclusions
ice_data <- emulandice2::select_sims("main")

# Calculate SLE change w.r.t. cal_start year, and tidy units
ice_data <- emulandice2::calculate_sle_anom()

# Do second selection for glaciers using values of SLE change
if (deliverable_test) {
  if (i_s == "GLA") {
    ice_data <- emulandice2::select_sims("history_match")
  }
}


# Match climate ---------------------------------------------------------------

# Get corresponding climate change(s) (match by GCM + scenario)
temps <- emulandice2::match_gcms(ice_data, temps_data, mean_impute = impute_gcms)

# For GIS post-2100, get fixed climate forcing change(s)
# and overwrite into rows of temps with fixed_date = 2100
if (i_s == "GIS" && final_year > 2100) {

  # Index of simulations forced with fixed climate from 2100 (column flag in dataset)
  fixed_ind <- ice_data$fixed_date == 2100 & !is.na(ice_data$fixed_date)

  cat("\nNow try matching again after reconstructing fixed post-2100 forcings im dataset\n",
      file = logfile_build, append = TRUE)

  temps_fixed <- emulandice2::match_gcms(ice_data, temps_data_fixed)
  temps[ fixed_ind, ] <- temps_fixed[ fixed_ind, ]

}

# Drop scenario and GCM columns: just keep climate column(s)
temps <- temps[ , -(1:2) ]

# Make numeric
if (length(temps_list) == 1) { temps <- as.numeric(temps)
} else temps <- apply(temps, 2, as.numeric)


# Find ice simulations that have climate forcing (just last timeslice if multiple)
if ( length(temps_list) == 1 ) { sim_index <- !is.na(temps)
} else sim_index <- !is.na(temps[, length(temps_list)])

# Keep only these in both ice and climate data
ice_data <- ice_data[ sim_index, ]
if ( length(temps_list) == 1) { temps <- temps[ sim_index ]
} else temps <- temps[ sim_index, ]


# END OF ICE SIMULATION SELECTION
N_sims <- dim(ice_data)[1]

cat(paste("\nFINAL DATA SELECTION: using", N_sims, "ice simulations for",
          i_s, reg, "\n"), file = logfile_build, append = TRUE)

cat("\nOf which:", "\n", file = logfile_build, append = TRUE)
for (mm in model_list) {
  cat( paste0(mm, ": ", length( ice_data[ice_data$model == mm, 1] )), "\n",
       file = logfile_build, append = TRUE)
}

# Retrieve Greenland fixed post-2100 climate forcings
if ( i_s == "GIS" && final_year > 2100) {

  # Update fixed_ind with final ice simulation dataset
  fixed_ind <- ice_data$fixed_date == 2100 & !is.na(ice_data$fixed_date)

  cat(paste("\nNumber of simulations forced with fixed post-2100 climate:",
            dim(ice_data[ fixed_ind, ])[1], "\n"),
      file = logfile_build, append = TRUE)

  match_sims_fixed <- unique(ice_data[ fixed_ind, c("scenario", "GCM")])

  # Also select in climate_data for full time series forcing plot
  climate_data_test <- apply(match_sims_fixed, 1, function(x) { # as in match_gcms()

    # For each row in forcings list, get climate timeseries
    climate_data_fixed[ climate_data_fixed$GCM == x[ "GCM" ]
                        & climate_data_fixed$scenario == x[ "scenario"], ]
  })

  # Ugh: convert list to numeric matrix...
  tmp <- matrix(0.0, nrow = dim(match_sims_fixed)[1], ncol = dim(climate_data_fixed)[2] - 2)
  for ( cc in 1:length(climate_data_test)) {
    tmp[ cc, ] <- as.numeric(unlist(climate_data_test[[cc]][, 3:dim(climate_data_fixed)[2]]))
  }
  colnames(tmp) <- colnames(climate_data_fixed[ , 3:dim(climate_data_fixed)[2]])

  # Overwrite old climate_data_fixed with selected this subset and scenario/GCM columns
  climate_data_fixed <- cbind(match_sims_fixed, tmp)

  # Print
  cat(paste("\nUsing these",dim(match_sims_fixed)[1],"forcings fixed from 2100:\n"),
      file = logfile_build, append = TRUE)
  ms <- match_sims_fixed[ sort(match_sims_fixed[,"scenario"], index.return = TRUE)$ix, ]
  for( mm in 1:dim(ms)[1]) {
    cat( unlist(ms[mm, c("scenario", "GCM")]), "\n", file = logfile_build, append = TRUE)
  }

}

# Get final list of scenarios and GCMs to write to text and plot forcings (not very efficient!)
if ( i_s == "GIS" && final_year > 2100) {
  match_sims <- unique(ice_data[ !fixed_ind, c("scenario", "GCM")])
} else {
  match_sims <- unique(ice_data[ , c("scenario", "GCM")])
}

climate_data_test <- apply(match_sims, 1, function(x) { # as in match_gcms()

  # For each row in forcings list, get climate timeseries
  climate_data[ climate_data$GCM == x[ "GCM" ]
                & climate_data$scenario == x[ "scenario"], ]
})

# Ugh: convert list to numeric matrix...
tmp <- matrix(0.0, nrow = nrow(match_sims), ncol = ncol(climate_data) - 2)
#print(dim(tmp))
#print(nrow(match_sims))
#print(ncol(climate_data))
#print(length(climate_data_test))

for ( cc in 1:length(climate_data_test)) {
  if (nrow(climate_data_test[[cc]][, 3:dim(climate_data)[2]]) == 0 ) {
    cat(paste("\nWaarning: cannot find forcing number",cc,"in CSV file:\n"),
        file = logfile_build, append = TRUE)
  } else {
  tmp[ cc, ] <- as.numeric(unlist(climate_data_test[[cc]][, 3:dim(climate_data)[2]]))
  }
}
colnames(tmp) <- colnames(climate_data[ , 3:dim(climate_data)[2]])

# Overwrite old climate_data with selected this subset and scenario/GCM columns
climate_data <- cbind(match_sims, tmp)

cat(paste("\nUsing these",dim(match_sims)[1],"full forcings:\n"),
    file = logfile_build, append = TRUE)
ms <- match_sims[ sort(match_sims[,"scenario"], index.return = TRUE)$ix, ]
for( mm in 1:dim(ms)[1]) {
  cat( unlist(ms[mm, c("scenario", "GCM")]), "\n", file = logfile_build, append = TRUE)
}


# Cross-check number of timeseries kept
cat(paste("\nKeeping climate timeseries (should match number of forcings above):",
          dim(climate_data)[1], "\n"), file = logfile_build, append = TRUE)

# Check some simulations found!
stopifnot(N_sims > 0)

# Ice sheet regions ------------------------------------------------------------

do_regions <- FALSE # xxx for testing
# xxx can remove this exception when I get IMAUICE regions and remake regional CSV files
if ( i_s == "AIS" && (
  "BISICLES" %in% model_list || "IMAUICE" %in% model_list)) do_regions <- FALSE

if (i_s %in% c("AIS","GIS") && do_regions) {

  cat("\nIce sheet regional fractions\n", file = logfile_build, append = TRUE)

  # Get row numbers i.e. selected simulations of main dataset
  sims_index <- rownames(ice_data[ ,  paste0("y", years_em) ])

  # No need to run calculation_sle_anom: just anomaly and x100

  region_names <- list() # names of regions
  region_fracs_all <- list() # histograms for each region
  region_fracs <- list() # mean or adjusted median fraction for each region

  # Calculate mean fractions for regions
  if (i_s == "GIS") {

    # This file has ALL + 6 regions
    # xxx issue: Remake after deliverable: this is sle not slc
    region_file <- read.csv(paste0( inputs_preprocess, "/GIS/SLE_SIMULATIONS_GIS_p9_240304.csv"))

    # Translate CSV regions to nicer names for netcdf files
    region_names[["nw"]] <- "NW"
    region_names[["no"]] <- "NO"
    region_names[["cw"]] <- "CW"
    region_names[["ne"]] <- "NE"
    region_names[["sw"]] <- "SW"
    region_names[["se"]] <- "SE"

    # All simulations (to construct index)
    # xxx could use load_sims here
    all <- region_file[ region_file$region == "ALL",  ]
    nrows_all <- dim(all)[1]

    # Timeslices for sims selected in main analysis
    all <- all[ sims_index, paste0("y", years_em) ]

    # Plot: GIS regions ----------------------------------------------------------

    # Open plot file for histograms
    # xxx use prefix name as for other pdfs
    if (plot_level > 0) {
      pdf( file = paste0( outdir, "region_fractions_", i_s, ".pdf" ))
      par(mfrow = c(3,2))
    }

    for (rr in names(region_names) ) {

      # Get all simulations for region and number rows
      region_all <- region_file[ region_file$region == rr, paste0("y", years_em) ]
      rownames(region_all) <- 1:nrows_all

      rr_name <- region_names[[rr]]

      # Pick same rows as main analysis
      region_all <- region_all[ sims_index, ]

      # Calculate fractions (all timeslices in all simulations)
      region_fracs_all <- unlist(  region_all / all )

      # Mean of these
      region_fracs[[ rr_name ]] <- mean(region_fracs_all, na.rm = TRUE)

      # Print to file
      cat( sprintf( "%s: %.4f\n", rr_name,
                    region_fracs[[ rr_name ]] ), file = logfile_build, append = TRUE)

      # Plot
      if (plot_level > 0) {

        hist(region_fracs_all, xlim = c(0,1),
             breaks = seq(from = floor(min(region_fracs_all, na.rm = TRUE)),
                          to = ceiling(max(region_fracs_all, na.rm = TRUE)), by = 0.01),
             main = paste0(ice_name, ": ", rr_name), xlab = "Fraction" )
        abline(v = region_fracs[[ rr_name ]], lwd = 2, col = "blue")
        text( 0.7, 300, sprintf("Mean: %.3f",
                                region_fracs[[ rr_name ]]), col = "blue")
      }


    }

    if (plot_level > 0) dev.off()

    cat( paste("\nTotal:", sum(unlist(region_fracs)), "\n"), file = logfile_build, append = TRUE)

  }

  # Calculate adjusted mean fractions for regions
  if (i_s == "AIS") {

    region_names <- c( "WAIS1", "WAIS2", "WAIS3", # ASE, Ross, RF
                       "PEN",
                       paste0("EAIS", 1:7) )

    # All simulations (to construct index)
    all <- emulandice2::load_sims(variable = "ice", source = i_s)

    nrows_all <- dim(all)[1]

    # Timeslices for sims selected in main analysis
    all <- all[ sims_index, paste0("y", years_em) ]

    for (rr in 1:length(region_names) ) {

      # Regional CSV
      # xxx Could add region arg to load_sims?
      region_file <- read.csv(paste0( inputs_preprocess, "/AIS/regions/SLE_SIMULATIONS_AIS_full_region_",rr,"_240527.csv"))

      rr_name <- region_names[[rr]]

      # Get all simulations for region and number rows
      region_all <- region_file[ , paste0("y", years_em) ]
      rownames(region_all) <- 1:nrows_all

      # Pick same rows as main analysis
      region_all <- region_all[ sims_index, ]

      # Calculate fractions (all timeslices in all simulations)
      region_fracs_all[[ rr_name ]] <- as.numeric(unlist(region_all))  / as.numeric(unlist(all))

      # Replace infinities with missing
      region_fracs_all[[ rr_name ]][is.infinite(region_fracs_all[[ rr_name ]])] <- NA

      # Calculate MEDIAN not mean for Antarctica
      region_fracs[[ rr_name ]] <- median(region_fracs_all[[ rr_name ]], na.rm = TRUE)

      # Print to file
      cat( sprintf( "%s: %.4f\n", rr_name,
                    region_fracs[[ rr_name ]] ), file = logfile_build, append = TRUE)

    }

    total_median <- sum(unlist(region_fracs))

    cat( paste("\nTotal of medians:", total_median, "\n"), file = logfile_build, append = TRUE)

    missing <- 1.0 - total_median
    cat(sprintf("\nMissing fraction before adjustment: %.3f\n", missing), file = logfile_build, append = TRUE)

    # Get median fractions to adjust
    region_fracs_adj <- unlist(region_fracs)

    # Sort sectors - this is from when I redistributed only to largest sectors
    cat("\nSectors in decreasing contribution:\n", file = logfile_build, append = TRUE)
    sec_sort <- sort(unlist(region_fracs_adj), decreasing = T, index.return = T)
    for (ss in sec_sort$ix) {
      cat(sprintf("%i: %.1f%%\n", ss, 100.0*region_fracs_adj[ss]), file = logfile_build, append = TRUE)
    }

    # Now redistribute amongst all regions instead
    n_largest <- length(region_names)

    cat(paste("\nTake largest",n_largest,"sub-sectors:\n"), file = logfile_build, append = TRUE)
    largest <- sec_sort$ix[1:n_largest]
    cat(largest, "\n", file = logfile_build, append = TRUE)

    cat("\nNormalise these, from:\n", file = logfile_build, append = TRUE)
    cat(region_fracs_adj[largest],"\n", file = logfile_build, append = TRUE)

    # Proportion of total of this subset
    prop <- region_fracs_adj[largest] / (sum(region_fracs_adj[largest]))
    cat("to:\n", file = logfile_build, append = TRUE)
    cat(prop, "\n", file = logfile_build, append = TRUE)
    stopifnot(sum(prop) - 1 < 0.0001)

    tot_adj <- 0.0
    tot_adj_largest <- 0.0

    # Plot: AIS regions ----------------------------------------------------------

    # Pdf later than for GIS because adjusting fractions
    if (plot_level > 0) {
      # xxx Use prefix name - can probably move both outside i_s chunks
      pdf( file = paste0( outdir, "region_fractions_", i_s, ".pdf" ))
      par(mfrow = c(3,2))
    }

    cat("\nAdjust median fractions to sum to 1:\n", file = logfile_build, append = TRUE)

    for (ss in 1:length(region_names)) {

      # Using approx. fraction
      if (ss %in% largest) {

        miss_bits <- prop[ which(ss == largest, arr.ind = T) ] * missing
        region_fracs_adj[ss] = region_fracs_adj[ss] + miss_bits


        tot_adj_largest <- tot_adj_largest + region_fracs_adj[ss]

        cat( sprintf("%i: median = %.3f, adjusted = %.3f (%.0f%% adjustment)\n",
                     ss, region_fracs[[ss]], region_fracs_adj[ss], 100.0*miss_bits / region_fracs[[ss]] ),
             file = logfile_build, append = TRUE)

      } else {

        cat( sprintf("%i: median = %.3f\n",
                     ss, region_fracs_adj[ss]), file = logfile_build, append = TRUE )

      }

      tot_adj <- tot_adj + region_fracs_adj[ss]


      # Plot histograms now so can show median and adjusted together
      if (plot_level > 0) {
        hist(region_fracs_all[[ ss ]], xlim = c(-1,1),
             breaks = seq(from = floor(min(region_fracs_all[[ ss ]], na.rm = TRUE)),
                          to = ceiling(max(region_fracs_all[[ ss ]], na.rm = TRUE)), by = 0.01),
             main = paste0(ice_name, ": ", region_names[[ss]]), xlab = "Fraction" )
        abline(v = region_fracs[[ ss ]], lwd = 2, col = "darkred")
        abline(v = region_fracs_adj[ ss ], lwd = 2, col = "red", lty = 2)

        # xxx sort ypos for 2300 and 2100
        text( 0.45, 800, pos = 4, sprintf("Median: %.3f",
                                          region_fracs[[ ss ]]), col = "darkred", cex = 0.9)
        text( 0.45, 400, pos = 4, sprintf("Adjusted: %.3f",
                                          region_fracs_adj[ ss ]), col = "red", cex = 0.9)
      }

      # Overwrite original fraction list with adjusted AFTER plotting histograms
      region_fracs[[ss]] <- region_fracs_adj[ss]


    }

    if (plot_level > 0) dev.off()

    cat(sprintf("\nTotal of largest %i sectors after adjustment: %.3f\n", n_largest, tot_adj_largest), file = logfile_build, append = TRUE )
    cat(sprintf("Total of all sectors after adjustment = %.3f\n", tot_adj), file = logfile_build, append = TRUE)

  }

} # ice sheet regions

# Final checks ------

# Degrees of freedom check: do we have enough simulations (rows)
# for predicting timeslices (columns)?
stopifnot(N_sims > N_ts)

# Check multiple values for the inputs, otherwise fail
for ( pp in ice_param_list ) {
  if (length( unique(ice_data[,pp]) ) == 1) {
    stop( paste("Only one unique value of requested input", pp, "in dataset: please drop"))
  }
}

# Check for NAs in columns we plan to use to emulate, otherwise fail
if (anyNA( ice_data[ , ice_param_list ] )) stop("NAs found in ice_data columns to use as inputs in emulation: please drop/fix")

# COMBINE CLIMATE FORCING AND CONTINUUOUS ICE MODEL INPUTS INTO DESIGN MATRIX
ice_design <- as.matrix( data.frame(temps, ice_data[ ice_cont_list ]) )

# Add climate col names
colnames(ice_design)[ 1:length(temps_list) ] <- temps_list_names

# Create axis label for plots
GSAT_lab <- list()
for (tt in 1:length(temps_list_names)) {
  GSAT_lab[[temps_list_names[tt]]] <- paste0('Global mean temperature ',
                                             temps_list[tt]-N_temp_yrs+1,'-',temps_list[tt],
                                             ' rel. to ',temps_baseline,'-',temps_baseline+N_temp_yrs-1,' (degC)')
}

# One-hot encoding ---------------------------------------------------------------
#' ## One-hot encoding of factors

ice_factor_values <- list()

# ADD FACTOR COLUMNS
if ( include_factors ) {

  # Adding factors
  for ( ff in ice_factor_list ) {

    cat(paste("Factor to add:", ff, "\n"), file = logfile_build, append = TRUE)
    ff_vals <- sort(unique(ice_data[ ,ff]))

    cat(paste("Levels:", length(ff_vals), "\n"), file = logfile_build, append = TRUE)

    # First alphabetical value will be reference/nominal: ff_vals[1]
    cat(paste("Adding dummy variables with reference value:", ff_vals[1], "\n"), file = logfile_build, append = TRUE)

    for ( vv in ff_vals ) {

      # Drop first (reference) level to avoid collinearity
      if (vv == ff_vals[1]) next

      # Name of column is factor:level
      cat(paste0("Generating column ", ff, ":", vv, "\n"), file = logfile_build, append = TRUE)

      # Set to 1 or 0
      ice_design <- cbind(ice_design, ifelse(ice_data[, ff] == vv, 1, 0 ) )
      colnames(ice_design)[dim(ice_design)[2]] <- paste(ff, vv, sep = ":")

    }
    # Alternative code
    # for (j in 1:length(ff_vals)) dummy[,j] <- as.integer(ice_data[, ff] == ff_vals[j])

    #}
    # Save to use for prior
    ice_factor_values[[ff]] <- ff_vals
  } # factors loop
}

# Save list of continuous inputs in design
input_cont_list <- c(temps_list_names, ice_cont_list)

# Save list of ice inputs: not ice_param_list but expanded dummy versions
ice_dummy_list <- NA

if (include_factors) {
  ice_dummy_list <- colnames(ice_design)[ ! colnames(ice_design) %in% input_cont_list]
  ice_all_list <- c( ice_cont_list, ice_dummy_list)
} else {
  ice_all_list <- ice_cont_list
}


#' ## Scale inputs for emulator
# Scale inputs ---------------------------------------------------------------
# xxx Can I move this into get_inputs()?

cat("\nOriginal ranges of inputs:\n", file = logfile_build, append = TRUE)
for (cc in 1:dim(ice_design)[2]) {
  cat( paste(colnames(ice_design)[cc], min(ice_design[,cc]), "to",
             max(ice_design[,cc]), "\n"), file = logfile_build, append = TRUE)
}

# Scale columns of continuous parameters (climate and ice model)
cat("\nCentre and scale continuous inputs (mean = 0, s.d. = 1)\n", file = logfile_build, append = TRUE)
ice_design_scaled_cont <- scale(ice_design[, input_cont_list])

# Store scaling to use later for prior
inputs_centre <- attr(ice_design_scaled_cont,"scaled:center")
inputs_scale <- attr(ice_design_scaled_cont,"scaled:scale")

# Fill back into original design
ice_design_scaled <- ice_design
ice_design_scaled[, input_cont_list] <- ice_design_scaled_cont

cat("\nNew ranges of inputs after scaling:\n", file = logfile_build, append = TRUE)
for (cc in 1:dim(ice_design_scaled)[2]) {
  cat( paste(colnames(ice_design_scaled)[cc], min(ice_design_scaled[,cc]), "to",
             max(ice_design_scaled[,cc]), "\n"), file = logfile_build, append = TRUE)
}

# Make sure scenario list only includes those of simulations
scenario_list <- scenario_list[ scenario_list %in% unique(ice_data[,"scenario"]) ]
#cat(paste("Scenario list:",paste(scenario_list, collapse = ","), "\n"), logfile_build, append = TRUE)


#' # Plot simulations
# Plot: sims -----------------------------------------------------------------------

cat("\nPlot simulator projections\n", file = logfile_build, append = TRUE)

# Plot simulations (some with observations)
# Can repeat from main.R to tweak or add model discrepancy to history matching window
if (plot_level > 0) {
  pdf( file = paste0( outdir, out_name, "_SIMS.pdf"),
       width = 9, height = 5)
  emulandice2::plot_designs("sims", plot_level)
  emulandice2::plot_timeseries("sims", plot_level)
  emulandice2::plot_scatter("sims", "none", plot_level)
  emulandice2::plot_distributions("sims", plot_level)
  dev.off()
}

# Impute missing ---------------------------------------------------------------


if (impute_sims != "none") {

  # Impute data (take from end of calibration period ta avoid calibrating imputed)
  cat( paste0("\nRequested impute simulations with SVD: ",impute_sims,"\n"),
       file = logfile_build, append = TRUE)

  # Use SVD to impute missing projection years within time series, and at end (up to impute_nyrs limit)
  years_proj <- years_em[years_em >= cal_end]
  ice_data_proj <- ice_data[ , paste0("y", years_proj) ]
  num_miss <- is.na(ice_data_proj)

  miss_sims <- apply(ice_data_proj, 1, function(x) {
    ifelse( length(x[ is.na(x) ]) > 0, TRUE, FALSE)
  })

  cat( paste("\nNumber of simulations with missing values:",
             sum(miss_sims),"\n"), file = logfile_build, append = TRUE)
  cat( paste("\nImputing",sum(num_miss),"simulation values\n"), file = logfile_build, append = TRUE)

  if (sum(num_miss) > 0) {

    ice_data_impute <- emulandice2::SVDimpute( as.matrix(ice_data_proj) )

    pdf( file = paste0( outdir, out_name, "_impute.pdf"),
         width = 9, height = 5)

    # All data
    matplot(years_proj, t(ice_data_impute), type = "n",
            col = grey(0.1, 0.1), lty = 1, xlab = "Year", ylab = "Sea level contribution (cm SLE)",
            main = ice_name)

    # Imputed values (where original had NA)
    matlines(years_proj, t(ice_data_impute[ miss_sims, ]),
             type = "l", col = "red", lty = 1, lwd = 0.5)
    # Simulated values
    matlines(years_proj, t(ice_data_proj[ miss_sims, ]),
             type = "l", col = "black", lty = 1, lwd = 0.5)

    dev.off()

    # Add historical years xxx change if imputing back too
    ice_data_impute <- cbind(ice_data[ , paste0("y", years_em[years_em < cal_end])], ice_data_impute)

    # if any missing
  } else {
    # else return original
    ice_data_impute <- cbind(ice_data[ , paste0("y", years_em[years_em < cal_end])], ice_data_proj)
  }
}

# Sims only for testing: stop here
save.image(file="~/PROTECT/emulandice2/sims.RData")
if ( read_sims_only) stop("Stopping after reading and plotting simulations", call. = FALSE)

# ________________----
#' # Build emulator
# BUILD EMULATOR  ------------------------------------------------------------

# FULL DATASET:

# Inputs
XX <- ice_design_scaled

# Outputs: original or imputed
#if (impute_sims) {
#  YY <- cbind(ice_data[ , paste0("y", years_em[years_em < cal_end])], ice_data_impute)
#} else YY <- ice_data[ , paste0("y", years_em) ]
if (impute_sims != "none") {
  YY <- ice_data_impute
} else YY <- ice_data[ , paste0("y", years_em) ]


# Train emulators with:

# If validation_type == "loo":
# 1. Select N = 1000 here, if N_ensemble > 1000 and doing LOO
# 2. Otherwise, skip and use N = N_ensemble

# If validation_type == "tvt":
# 3. N =~ 1000, if dataset large and using normal GP
# 4. N = 70% of N_ensemble, if dataset medium-large OR if GP can cope with large dataset (e.g. laGP)
# i.e. select non-random sample and reserve 30% / remaining for testing

#' # Select data subset
# Select data subset  ------------------------------------------------------------

# SUBSET DATA FOR TRAINING: 70% of total, or 70% of N_max_em
# Samples a balance of factor levels, not just random

# Will be set to TRUE later if subset taken
train_subset <- FALSE

# Only do this selection if not using LOO validation later
# except if reproducing deliverable, which set limit of 1000 for all
# Note uses of ice_data not YY here are fine: same number of rows as YY (which has imputed years)
if ( validation_type != "loo" | # case 3,4
     ( nrow(ice_data) > 1000L &&  validation_type == "loo" && deliverable_test)) { # case 1

  # Get full dataset design
  # Yes, really! Not emulator inputs, because full list includes e.g. GCM, SSP etc
  # which is good for sampling GSAT and noisy ice responses to GCMs for given GSAT
  # So this uses columns that may be ignored everywhere else
  Xraw <- ice_data[, ice_param_list_full]

  # Make into nice data frame with factors
  make_factor <- function(x) {
    x[is.na(x)] <- "NA"
    factor(x)
  }

  Xraw <- lapply(Xraw, function(x) {
    if(is.character(x)) {
      make_factor(x)
    } else {
      x
    }}) |> as.data.frame()

  # Run through factor list to pick up any numeric or T/F factors
  for (cc in colnames(Xraw)) {
    if (cc %in% ice_factor_list & !is.factor(Xraw[, cc])) Xraw[, cc] <- make_factor(Xraw[, cc])
  }

  cat( paste("\nMax sample size for training:",N_max_em,"\n"),
       file = logfile_build, append = TRUE)

  # Number of simulations to train with
  # If limit set for large dataset: trains with N_max_em at most
  if ( !is.na(N_max_em)) {

    # Case 3/4 switch: case 4 unless too big, then case 3
    target_size <- round( min(0.7 * nrow(ice_data), N_max_em) )

  } else {
    # If no limit set (e.g. for GP that can handle large data): train with 70% of full ensemble
    # Case 4 with no limit
    target_size <- round(0.7 * nrow(ice_data))

  }

  # Or subset for LOO in deliverable_test mode xxx double-check this
  if (nrow(ice_data) > 1000L && deliverable_test && validation_type == "loo") {
    target_size <- 1000L
  }

  cat( paste("\nSelecting",target_size,"simulations for training:\n"),
       file = logfile_build, append = TRUE)

  # Was random sample for deliverable, which used LOO
  if ( nrow(ice_data) > 1000L && deliverable_test && validation_type == "loo") {
    cat( paste("- random sample\n"),
         file = logfile_build, append = TRUE)
    train <- sort(sample(nrow(ice_data), target_size))

  } else {

    # Order ensemble using all factors in original dataset file
    # to pick the most informative simulations with respect to the factor levels
    cat( paste("- ordered sample\n"),
         file = logfile_build, append = TRUE)

    # Output factors
    cat("\n** Factor levels being used for ordering:\n", file = logfile_build, append = TRUE)
    for (jj in which(sapply(Xraw, is.factor))) {
      cat(paste0("\t", names(Xraw)[ jj ], ":\n"), file = logfile_build, append = TRUE)
      cat(paste0("\t", paste(levels(Xraw[[ jj ]]), collapse = ", "), "\n"), file = logfile_build, append = TRUE)
    }

    ## Reorder dataset design to make sure factor levels well-sampled at start of list
    # (simple random if no factors)
    reordered <- reorder_rows(Xraw, frontLoad = TRUE)

    # Improved method: select first N_subset of rows
    train <- reordered[ 1:target_size ]

  }

  # Apply random/ordered selection to raw design (just for checking), and inputs and outputs
  Xraw_sub <- Xraw[ train, ]
  XX_sub <- XX[ train, ]
  YY_sub <- YY[ train, ]
  train_subset <- TRUE

  # Factor levels in training data - all factors, not just emulated
  cat("\n** Factor levels present in training subset:\n", file = logfile_build, append = TRUE)
  for (jj in which(sapply(Xraw_sub, is.factor))) {
    cat(paste0("\t", names(Xraw_sub)[ jj ], ":\n"), file = logfile_build, append = TRUE)
    cat(paste0("\t", paste(levels(Xraw_sub[[ jj ]]), collapse = ", "), "\n"), file = logfile_build, append = TRUE)
  }

} # if not LOO (or if sampling for deliverable_test LOO)

# make_emu -----

# Build emulator
# Writes emu obj into .RData workspace file later for running in FACTS
# Note this call is repeated in do_LOO.R

# Train with random/ordered subset, or else full dataset ice_data[_impute]
if (train_subset) {
  Xtrain <- XX_sub
  Ytrain <- YY_sub
} else {
  # Need to keep XX,YY if test and train validation - draw test_set
  Xtrain <- XX
  Ytrain <- YY
}

print("Building emulator...")
emu_mv <- emulandice2::make_emu( as.matrix(Xtrain), as.matrix(Ytrain) ) # uses same in do_loo() call below

save.image(file="~/PROTECT/emulandice2/make_emu.RData")

# ________________----
# TEST ------------------------------------------------------------

#' # Predict for SA designs
# Design: main effects ---------------------------------------------------------

#' ## Main effects
# Main effects (i.e. one-at-a-time design for sensitivity analysis)
design_sa <- emulandice2::load_design_to_pred("main_effects", 100L)

cat(paste("\nPredict for main effect plots:\n"), file = logfile_build, append = TRUE)

# Predict: overwrite object
myem <- list()
for (input in names( design_sa )) {

  cat(paste("Main effects:",input,"\n"), file = logfile_build, append = TRUE)

  design_sa_scaled_cont <- scale(design_sa[[input]][ , input_cont_list],
                                 center = inputs_centre,
                                 scale = inputs_scale )

  design_sa_scaled <- as.data.frame( design_sa[[input]] )
  design_sa_scaled[ , input_cont_list ] <- design_sa_scaled_cont

  myem[[input]] <- emulandice2::emulator_predict( design_sa_scaled )
}

save.image(file="~/PROTECT/emulandice2/MEFF.RData")

#' ## Uniform temperature prior

# Design: uniform --------------------------------------------------------------

# Design "unif_temps" makes projections using uniform priors for GSAT with same ranges as sims
# a better comparison than using FaIR projected distributions for each SSP

design_pred <- emulandice2::load_design_to_pred("unif_temps", N_unif)

cat(paste("\nPredict for uniform temp designs:\n"), file = logfile_build, append = TRUE)

for (scen in scenario_list) {

  cat(paste("Scenario with uniform priors:",scen,"\n"), file = logfile_build, append = TRUE)

  design_pred_scaled_cont <- scale(design_pred[[scen]][ , input_cont_list],
                                   center = inputs_centre,
                                   scale = inputs_scale )
  design_pred_scaled <- as.data.frame( design_pred[[scen]]  )
  design_pred_scaled[ , input_cont_list] <- design_pred_scaled_cont
  myem[[scen]] <- emulandice2::emulator_predict( design_pred_scaled )
}

save.image(file="~/PROTECT/emulandice2/unif_temps.RData")


# Sample emu uncertainty ----------------------------------------------------------------------
projections <- list()

# Want to see unif_temps final projections (samples with uncertainty) for validation
for (scen in scenario_list) {
  projections[[scen]] <- emulandice2::emulator_uncertainty(myem[[scen]])
}

# Plot: SA -----------------------------------------------------

# Plot sensitivity analysis
if (plot_level > 0) {
  pdf( file = paste0( outdir, out_name, "_SA.pdf"),
       width = 9, height = 5)
  emulandice2::plot_scatter("prior", "main_effects", plot_level)
  emulandice2::plot_scatter("prior", "unif_temps", plot_level)
  emulandice2::plot_scatter("posterior", "unif_temps", plot_level) # overkill?
  dev.off()
}


#' # Validate

# Validate ---------------------------------------------------------------------

# LOO VALIDATION: i.e. train on all-but-one, for validation
# Should only be used for small datasets

# Builds LOO emulators, and plots + keeps results for requested timeslices
if (validation_type == "loo") {

  cat("\nLEAVE ONE OUT VALIDATION\n", file = logfile_build, append = TRUE)

  # Test every N_k-th run
  # this is the slow bit....
  # xxx Improve: stratified by output value instead of every N_k
  loo_valid_all <- emulandice2::do_loo( as.matrix(Xtrain), as.matrix(Ytrain), validation_years, N_k = N_k)

  # To store results
  loo_mean <- list()
  loo_sd <- list()
  wrong <- list()

  # Loop over time slices to calculate metrics and make plots
  for ( yy in validation_years) {

    yind <- paste0( "y", yy)

    # Get LOO prediction (in do_loo.R)
    loo_mean[[yind]] <- loo_valid_all$mean[ , yind]
    loo_sd[[yind]] <- loo_valid_all$sd[ , yind]

    # N_k selection of runs
    N_k_index <- !is.na(loo_mean[[yind]])
    N_k_subset <- length( loo_mean[[yind]][ N_k_index ]  )

    # Which ones were within predicted intervals and which ones missed?
    wrong[[ yind ]] <- Ytrain[ , yind] > ( loo_mean[[yind]] + 2*loo_sd[[yind]] ) |
      Ytrain[ , yind] < (loo_mean[[yind]]  - 2*loo_sd[[yind]])

    # Fraction that missed
    frac_right <- 1 - ( length(which(wrong[[yind]][N_k_index] == TRUE)) / N_k_subset )

    # xxx Could save in list for plot_loo, or output summary there - duplication
    loo_err <- loo_mean[[yind]] - Ytrain[ , yind ]
    loo_std_err <- loo_err / loo_sd[[yind]]

    # Just keep calculated values
    loo_err <- loo_err[ N_k_index ]
    loo_std_err <- loo_std_err[ N_k_index ]

    # PRINT RESULTS
    cat(paste("\nLOO VALIDATION:",yy, "\n"), file = logfile_build, append = TRUE)
    cat(sprintf("Coverage (within %i emulator 95%% intervals): %.2f%%\n", yy,
                frac_right*100.0), file = logfile_build, append = TRUE)
    cat(sprintf("Mean of %i emulator absolute errors (cm): %.1f\n", yy,
                mean(abs(loo_err))), file = logfile_build, append = TRUE)
    cat(sprintf("Range of %i emulator absolute errors (cm): [%.1f, %.1f]\n", yy,
                min(loo_err), max(loo_err)),
        file = logfile_build, append = TRUE)
    cat(sprintf("Mean of %i emulator standardised errors: %.1f\n", yy,
                mean(loo_std_err)), file = logfile_build, append = TRUE)
    cat(sprintf("Range of %i emulator standardised errors: [%.1f, %.1f]\n", yy,
                min(loo_std_err), max(loo_std_err)),
        file = logfile_build, append = TRUE)

  } # years

  # Plot: LOO-------
  # Plot LOO results
  pdf( file = paste0( outdir, out_name, "_LOO.pdf"),
       width = 9, height = 5)
  emulandice2::plot_loo()
  dev.off()

} # validation_type == "loo"



# Builds emulators on 70% of data (or 70% of N_max_em for large datasets),
# and plots + keeps results for requested timeslices
if (validation_type == "tvt") {

  cat("\nTRAIN AND TEST VALIDATION\n", file = logfile_build, append = TRUE)

  # Get index of all rows except training data
  test_set <- reordered[-(1:target_size)]

  # Predict for all the original design points not in the training set
  # Note inputs are already scaled
  # ice_design_scaled has same number of rows as ice_data
  emu_test <- emulandice2::emulator_predict( ice_design_scaled[ test_set, ] )

  # Unlike LOO, should be no missing data in these: i.e. values for all test sims
  test_mean <- list()
  test_sd <- list()
  test_wrong <- list()

  # Use final year requested for LOO validation for now
  for ( yy in validation_years) {

    #  yy <- as.character(validation_years[length(validation_years)])
    yind <- paste0("y", yy)

    test_mean[[yind]] <- emu_test$mean[ , yind]
    test_sd[[yind]] <- emu_test$sd[ , yind]

    # Get test dataset to validate with
    # YY is the full dataset (with any imputed values), so this should be all but YY[ train ]
    test_data <- YY[ test_set, ]

    # Misses
    test_wrong[[ yind ]] <- test_data[ , yind] > ( test_mean[[yind]] + 2*test_sd[[yind]] ) |
      test_data[ , yind] < ( test_mean[[yind]]  - 2*test_sd[[yind]] )
    ww <- test_wrong[[yind]]

    # Again, no need to select this time unlike for LOO
    # xxx removed test_set selection which was a bug?!
    frac_right <- 1 - ( length(which(test_wrong[[yind]] == TRUE)) / length(test_set) )
    test_err <- test_mean[[yind]] - test_data[ , yind]
    test_std_err <- test_err / test_sd[[yind]]

    cat(sprintf("\nTRAIN AND TEST VALIDATION (N = %i):", length(test_set)),
        file = logfile_build, append = TRUE)
    cat(sprintf("\nNumber within %s emulator 95%% intervals: %.2f%%\n", yy,
                frac_right*100.0), file = logfile_build, append = TRUE)
    cat(sprintf("Mean of %s emulator absolute errors (cm): %.1f\n", yy,
                mean(abs(test_err))), file = logfile_build, append = TRUE)
    cat(sprintf("Range of %s emulator absolute errors (cm): [%.1f, %.1f]\n", yy,
                min(test_err), max(test_err)),
        file = logfile_build, append = TRUE)
    cat(sprintf("Mean of %s emulator standardised errors: %.1f\n", yy,
                mean(test_std_err)), file = logfile_build, append = TRUE)
    cat(sprintf("Range of %s emulator standardised errors: [%.1f, %.1f]\n", yy,
                min(test_std_err), max(test_std_err)),
        file = logfile_build, append = TRUE)

    # Plot: train and test --------
    # Plot train and test results
    yrange <- range(c(test_mean[[yind]] - 4*test_sd[[yind]],
                      test_mean[[yind]] + 4*test_sd[[yind]]), na.rm = TRUE)

    pdf( file = paste0( outdir, out_name, "_VALIDATION_", yy, ".pdf"),
         width = 5, height = 5)

    plot( test_data[ , yind], test_mean[[yind]],
          pch = 20,
          xlim = yrange, ylim = yrange, cex = 0.8,
          xaxs = "i", yaxs = "i",
          xlab = paste("Simulated sea level contribution at",yy,"(cm SLE)"),
          ylab = paste("Emulated sea level contribution at",yy,"(cm SLE)"),
          main = paste0("Test set validation (N = ", length(test_set), ")") )
    abline ( a = 0, b = 1 )
    if (i_s == "GLA") {
      abline( h = glacier_cap, col = "lightgrey", lwd = 0.5, lty = 5)
      abline( v = glacier_cap, col = "lightgrey", lwd = 0.5, lty = 5)
    }

    # +/- 2 s.d. error bars
    arrows( test_data[ , yind], test_mean[[yind]] - 2*test_sd[[yind]],
            test_data[ , yind], test_mean[[yind]] + 2*test_sd[[yind]],
            code = 3, angle = 90, lwd = 0.4, length = 0.02 )

    # Replot over in red for those that missed
    points( test_data[ ww, yind], test_mean[[yind]][ww],
            pch = 20, col = "red")
    arrows( test_data[ ww, yind],
            test_mean[[yind]][ww] - 2*test_sd[[yind]][ww],
            test_data[ ww, yind],
            test_mean[[yind]][ww] + 2*test_sd[[yind]][ww],
            code = 3, angle = 90, lwd = 0.4, length = 0.02, col = "red" )

    text( yrange[1], yrange[1] + 0.95*(yrange[2] - yrange[1]), pos = 4,
          ice_name, cex = 1.3)

    text( yrange[1], yrange[1] + 0.85*(yrange[2] - yrange[1]), pos = 4,
          sprintf("%.0f%%", frac_right*100.0), col = ifelse(frac_right < 0.9, "red", "black") )

    dev.off()

  } # validation_years loop
}


# ________________----
# SAVE BUILD FILE ------------------------------------------------------------
#' # Save emulator build file

# SAVE EMULATOR BUILT FROM WHOLE ENSEMBLE
# and the rest of the workspace, at least for now
RData_file <- paste0(rdatadir, out_name, "_EMULATOR.RData")

# Bit of duplication or unused
to_save <- c("climate_data", # CLIMATE MODEL SIMULATION DATA
             "ice_data", # ALL SELECTED ICE MODEL SIMULATION DATA
             "YY", # ice_data or subset of ice_data, with any imputed values
             "obs_data", # OBSERVATION DATA
             "inputs_preprocess", "inputs_ext", # Paths for package data
             "out_name", # General part of all output filenames
             "outdir", "logfile_build", # Used to write output in emulator function (see below)
             "deliverable_test", "do_regions", "impute_sims", # Analysis flags for info
             "model_list", # Could reconstruct from filename, but useful to have
             "scen_name", # Nicely formatted lookup name list of all scenarios looked for in datas
             "years_sim", # List of simulated years
             "ice_design", # Simulation ensemble design, i.e. input values
             "ice_param_list_full", # Lists of all simulated inputs
             "ice_cont_list", "ice_factor_list", "ice_all_list", # Lists of emulated inputs: continuous, factors, all
             "ice_dummy_list", "ice_factor_values", # Dummy column names and values for factor inputs
             "N_temp_yrs", # GSAT mean years; used in priors
             "temps", "temps_baseline", "temps_list", "temps_list_names", # GSAT means and names used
             "input_cont_list", # List of emulated continuous inputs, i.e. c(temps_list_names, ice_cont_list)
             "emulator_type",
             "emu_mv", # EMULATOR! function object
             "include_factors", # Are there any factors
             "years_em", "N_ts", # List and number of emulated years
             "inputs_centre", "inputs_scale", # Rescaling values for transforming params before/after emulation
             "first_year", "final_year", "cal_start", "cal_end", # Dates of data and calibration period
             "yy_plot", # Dates to plot
             "validation_type", "validation_years", # Save these for validation plotting
             "ice_name", # Nice ice source name for plots
             "GSAT_lab", # Nice plotting labels for GSAT means
             "sle_lim", "sle_inc", "ylim_obs", # Plotting ranges and increments (inc not used currently)
             "AR6_rgb", "AR6_rgb_light", "AR6_rgb_med" # Plotting colours
)

# Add extra bits for particular ice sources
if ( i_s == "GIS" && final_year > 2100) {
  to_save <- c(to_save, "climate_data_fixed") # Climate forcings fixed post-2100
}

if (i_s == "GLA") to_save <- c(to_save, "glacier_cap") # Glacier region maximum contributions

# RobustGaSP settings (no need to save emulator_covar as it is in RData name)
# Not sure if these are needed, as only predict is used
if (emulator_type == "statGP") to_save <- c(to_save, "lower_bound", "kernel", "alpha")

# laGP settings
# Need to save these because build and predict are done together
if (emulator_type == "laGP") to_save <- c(to_save, "laGP_scaling", "laGP_method",
                                          "laGP_nugget_prior")
# LOO validation bits
if (validation_type == "loo") {
  to_save <- c(to_save, "loo_mean", "loo_sd", "wrong")
}

# Train and test validation bits
if ( validation_type == "tvt" ) {
  to_save <- c(to_save, "test_mean", "test_sd", "test_wrong", "test_set" )
}

save(list = to_save, file = RData_file)

cat(paste("\nSaved objects to RData file:",RData_file,"\n"), file = logfile_build, append = TRUE)






