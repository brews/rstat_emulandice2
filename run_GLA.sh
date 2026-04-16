#!/bin/bash
#
# Run GLA analysis
# ./run_GLA.sh final_year
# where final_year is 2100 or 2300
#
#______________________________________________________

# Specify emulandice2 and results directories
emulandice_dir=/Users/tamsinedwards/PROTECT/emulandice2
results_dir=/Users/tamsinedwards/PROTECT/RESULTS
gsat_dir=/Users/tamsinedwards/PROTECT/gsat

# Assumes build file is in package directory ./data-raw
# and climate file is in package directory ./inst/extdata/GSAT

#______________________________________________________

# Final year is command line argument
final_year=$1

if [ "$final_year" != 2100 -a "$final_year" != 2150 -a "$final_year" != 2300 ]
then
     echo "Incorrect final year argument: please choose from 2100, 2150 or 2300"
     exit 1
fi

#model_list=GloGEM_OGGM_GO
model_list=GloGEM_OGGM

now=$(date +'%y%m%d')
outdir="$results_dir"/"$now"_GLA_ALL_"$final_year"

#for region in $(seq -f "%02g" 1 19) #  all regions
for region in 09 # one region (must zero-pad)
do

  echo
  echo "run_GLA.sh: build file for region RGI: $region"

  Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GLA $region $final_year

  # Emulator file name
  if [ "$region" == "01" -o "$region" == "03" -o "$region" == "04" -o "$region" == "05" -o "$region" == "07" -o "$region" == "19" ]
  then
       covar="pow_exp_01"
  else
       covar="pow_exp_20"
  fi

  # Over-write with tests # xxx should I set this one back?
  #if [ "$region" == "03" -o "$region" == "17" ]
  if [ "$region" == "17" ]
  then
    covar="pow_exp_10"
  fi

   if [ "$region" == "09" ]
   then
    covar="pow_exp_10"
    #covar="sexp" # For dgpsi: N.B. does not currently work in predict stage, only build stage
   fi

  # START PREDICTION
  echo
  echo "run_GLA.sh: predict for region RGI: $region"

  for ssp in "ssp119" "ssp126" "ssp245" "ssp370" "ssp585" "ssp534-over"
  do

  echo "Scenario:" $ssp

  # IPCC AR6: FaIR 2LM
  gsat_file=twolayer_SSPs.h5

  # Victor test files: FaIR 3LM
  # gsat_file="$ssp".temperature.fair.temperature_climate.nc

  echo "GSAT file:" $gsat_file

  ./emulandice_steer.sh GLA RGI"$region" ./data-raw/GLA_RGI"$region"_"$model_list"_"$covar"_EMULATOR.RData "$gsat_dir"/"$gsat_file" $ssp ./out/GLA_RGI"$region"_"$ssp"_"$final_year"/ 2024 GLA_RGI"$region"_"$ssp"_"$final_year"

  done
done

# Won't move if predictions already exist
mkdir $outdir
mv "$emulandice_dir"/out/GLA* "$emulandice_dir"/data-raw/GLA*_EMULATOR.RData $outdir
