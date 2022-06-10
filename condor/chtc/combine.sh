#!/bin/bash

tar -xzf R412.tar.gz
tar -xzf packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

tar -xzf output.tar.gz 
Rscript output/combine_files.R output/size-swd size-swd-4.0.0 1 output/size-swd
tar -czf output.tar.gz output/
