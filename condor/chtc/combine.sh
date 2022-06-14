#!/bin/bash

tar -xzf R412.tar.gz
tar -xzf packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

tar -xzf output.tar.gz 
Rscript output/combine_files.R output/size-wq size-wq-5.0.0 1d output/size-wq
tar -czf output.tar.gz output/

rm packages.tar.gz 
rm R412.tar.gz 