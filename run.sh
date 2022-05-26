#!/bin/bash

# 6.0.0 - wr-car
for ((i=0; i<2475; i++)) 
do 
    Rscript wr-car-6.0.0-1.R --args 6.0.0 $i 200 output/wr-car
done 

Rscript output/combine_files.R output/6.0 6.0.0 1

# for ((i=0; i<1050; i++)) 
# do 
#     Rscript sim/simulation-1.0.0_step-2.R --args $i 4 output/1.0
# done 

# Rscript output/combine_files.R output/1.0 1.0.0 2
