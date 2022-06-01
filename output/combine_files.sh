#!/bin/bash

# 6.0.0
Rscript combine_files.R /z/Comp/spkent/simulation/omisvm-sims/6.0/ wr-car-6.0.0 1 output/wr-car
cp wr-car/omisvm-sims-results-wr-car-6.0.0-1.rds /z/Comp/spkent/simulation/omisvm-sims/6.0

Rscript combine_files.R /z/Comp/spkent/simulation/omisvm-sims/6.0/ wr-car-6.0.0 2 output/wr-car
cp wr-car/omisvm-sims-results-wr-car-6.0.0-2.rds /z/Comp/spkent/simulation/omisvm-sims/6.0

# 7.0.0
Rscript combine_files.R /z/Comp/spkent/simulation/omisvm-sims/7.0/ wr-era-7.0.0 1 output/wr-era
cp wr-era/omisvm-sims-results-wr-era-7.0.0-1.rds /z/Comp/spkent/simulation/omisvm-sims/7.0

Rscript combine_files.R /z/Comp/spkent/simulation/omisvm-sims/7.0/ wr-era-7.0.0 2 output/wr-era
cp wr-era/omisvm-sims-results-wr-era-7.0.0-2.rds /z/Comp/spkent/simulation/omisvm-sims/7.0
