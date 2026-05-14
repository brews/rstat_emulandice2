#!/bin/bash
#
# Run GLA analysis
# ./run_GLA.sh final_year [build_date]
# where final_year is 2100, 2150 or 2300
#
# if no build_date specified, use today's date for predicting
# i.e. only specify build_date if running predict on older build files
#
#______________________________________________________

# Specify emulandice2 and results directories
# Predict call assumes build file is in package directory ./data-raw
# and looks for climate file in gsat_dir
emulandice_dir=/Users/tamsinedwards/PROTECT/emulandice2
results_dir=/Users/tamsinedwards/PROTECT/RESULTS
gsat_dir=/Users/tamsinedwards/PROTECT/gsat

#______________________________________________________

if [ $# -eq 0 ]; then
    echo "No arguments provided: final_year [build_date]"
    exit 1
fi

if [ $# -gt 2 ]; then
    echo "Too many arguments: final_year [build_date]"
    exit 1
fi

# Final year is command line argument
final_year=$1

if [ "$final_year" != 2100 -a "$final_year" != 2150 -a "$final_year" != 2300 ]
then
     echo "Incorrect final year argument: please choose from 2100, 2150 or 2300"
     exit 1
fi

# Today's date
now=$(date +'%y%m%d')

# Build date defaults to today if not given
build_date="${2:-$now}"

# Seed for prediction
seed=2024

# Dated name for directory
outdir="$results_dir"/"$now"_GLA_ALL_"$final_year" # all regions in one directory

for region in $(seq -f "%02g" 1 19) #  all regions
#for region in 19 # if running one region (must zero-pad to 2 digits, e.g. 01)
do

  ########################################
  # BUILD
  ########################################

  echo
  echo "run_GLA.sh: build file for region RGI: $region"

  Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GLA $region $final_year

  ########################################
  # PREDICT
  ########################################

  echo
  echo "run_GLA.sh: predict for region RGI: $region"

  build_file="GLA_RGI"$region"_"$final_year"_"$build_date"_EMULATOR.RData"

  echo "Build date:" $build_date
  echo "Build file:" $build_file
  echo

  for ssp in "ssp119" "ssp126" "ssp245" "ssp370" "ssp534-over" "ssp585"
  do

  echo "Scenario:" $ssp

  # IPCC AR6: FaIR 2LM
  gsat_file=twolayer_SSPs.h5

  echo "GSAT file:" $gsat_file

  ./emulandice_steer.sh GLA RGI"$region" ./data-raw/"$build_file" "$gsat_dir"/"$gsat_file" $ssp ./out/GLA_RGI"$region"_"$ssp"_"$final_year"/ $seed GLA_RGI"$region"_"$ssp"_"$final_year"

  done
done

# Won't move if predictions exist already
mkdir $outdir
mv "$emulandice_dir"/out/GLA* "$emulandice_dir"/data-raw/GLA*.RData $outdir
