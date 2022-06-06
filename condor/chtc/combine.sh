#!/bin/bash

tar -xzf R412.tar.gz
tar -xzf packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

tar -xzf output.tar.gz 
Rscript output/combine_files.R output/per-tma per-tma-2.0.0 2 output/per-tma
tar -czf output.tar.gz output/
