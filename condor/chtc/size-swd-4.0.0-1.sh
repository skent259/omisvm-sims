#!/bin/bash

# R setup on CHTC
tar -xzf R412.tar.gz
tar -xzf packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

# Gurobi setup on CHTC
export PATH
. /etc/profile.d/modules.sh
module load GUROBI/9.5.1 

# run script
tar -xzf to-transfer.tar.gz

sim="4.0.0"
i=$1
batch_size="50"

Rscript sim/size-swd-4.0.0-1.R $sim $i $batch_size # use default output_dir, data_dir

# zip up the output
rm output/size-swd/gridsearch-spec_size-swd.rds
tar -czf sim-size-swd-4.0.0-1_i=$i.tar.gz output/

# clean-up 
rm packages.tar.gz 
rm R412.tar.gz
rm to-transfer.tar.gz

