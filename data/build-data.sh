#!/bin/bash

# Set-up directory structure 
for dir in amrev size-imdb size-swd size-wq tma wr-car wr-era
do 
    mkdir $dir
    cd $dir
    mkdir processed
    mkdir raw
    cd ..
done 

# TODO: pull in data from this script


Rscript build-data_wr-car.R
Rscript build-data_wr-era.R