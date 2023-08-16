#!/bin/bash

# 1.0.1 - per-amrev
for ((i=0; i<792; i++)) 
do 
    Rscript sim/per-amrev-1.0.1-1.R --args 1.0.0 $i 25 output/per-amrev
done 

Rscript output/combine_files.R output/per-amrev 1.0.0 1

for ((i=0; i<600; i++)) 
do 
    Rscript sim/per-amrev-1.0.1-2.R --args 1.0.0 $i 1 output/per-amrev data/per-amrev/processed mae
    Rscript sim/per-amrev-1.0.1-2.R --args 1.0.0 $i 1 output/per-amrev data/per-amrev/processed mzoe
done 

Rscript output/combine_files.R output/per-amrev 1.0.0 2


# 2.0.0 - per-tma
for ((i=0; i<1650; i++)) 
do 
    Rscript sim/per-tma-2.0.0-1.R --args 2.0.0 $i 30 output/per-tma
done 

Rscript output/combine_files.R output/per-tma 2.0.0 1

for ((i=0; i<250; i++)) 
do 
    Rscript sim/per-tma-2.0.0-2.R --args 2.0.0 $i 2 output/per-tma data/per-tma/processed mae
    Rscript sim/per-tma-2.0.0-2.R --args 2.0.0 $i 2 output/per-tma data/per-tma/processed mzoe
done 

Rscript output/combine_files.R output/per-tma 2.0.0 2


# 3.0.1 - size-imdb
for ((i=0; i<4455; i++)) 
do 
    Rscript sim/size-imdb-3.0.1-1.R --args 3.0.0 $i 10 output/size-imdb
done 

Rscript output/combine_files.R output/size-imdb 3.0.0 1

for ((i=0; i<700; i++)) 
do 
    Rscript sim/size-imdb-3.0.1-2.R --args 3.0.0 $i 2 output/size-imdb data/size-imdb/processed mae
    Rscript sim/size-imdb-3.0.1-2.R --args 3.0.0 $i 2 output/size-imdb data/size-imdb/processed mzoe
done 

Rscript output/combine_files.R output/size-imdb 3.0.0 2


# 4.0.0 - size-swd
for ((i=0; i<2772; i++)) 
do 
    Rscript sim/size-swd-4.0.0-1.R --args 4.0.0 $i 50 output/size-swd
done 

Rscript output/combine_files.R output/size-swd 4.0.0 1

for ((i=0; i<700; i++)) 
do 
    Rscript sim/size-swd-4.0.0-2.R --args 4.0.0 $i 2 output/size-swd data/size-swd/processed mae
    Rscript sim/size-swd-4.0.0-2.R --args 4.0.0 $i 2 output/size-swd data/size-swd/processed mzoe
done 

Rscript output/combine_files.R output/size-swd 4.0.0 2


# 5.0.0 - size-wq
for ((i=0; i<3960; i++)) 
do 
    Rscript sim/size-wq-5.0.0-1.R --args 5.0.0 $i 50 output/size-wq
done 

Rscript output/combine_files.R output/size-wq 5.0.0 1

for ((i=0; i<1000; i++)) 
do 
    Rscript sim/size-wq-5.0.0-2.R --args 5.0.0 $i 2 output/size-wq data/size-wq/processed mae
    Rscript sim/size-wq-5.0.0-2.R --args 5.0.0 $i 2 output/size-wq data/size-wq/processed mzoe
done 

Rscript output/combine_files.R output/size-wq 5.0.0 2


# 6.0.0 - wr-car
for ((i=0; i<2475; i++)) 
do 
    Rscript sim/wr-car-6.0.0-1.R --args 6.0.0 $i 200 output/wr-car
done 

Rscript output/combine_files.R output/wr-car 6.0.0 1

for ((i=0; i<500; i++)) 
do 
    Rscript sim/wr-car-6.0.0-2.R --args 6.0.0 $i 10 output/wr-car data/wr-car/processed mae
    Rscript sim/wr-car-6.0.0-2.R --args 6.0.0 $i 10 output/wr-car data/wr-car/processed mzoe
done 

Rscript output/combine_files.R output/wr-car 6.0.0 2


# 7.0.0 - wr-era
for ((i=0; i<2475; i++)) 
do 
    Rscript sim/wr-era-7.0.0-1.R --args 7.0.0 $i 200 output/wr-era
done 

Rscript output/combine_files.R output/wr-era 7.0.0 1

for ((i=0; i<500; i++)) 
do 
    Rscript sim/wr-era-7.0.0-2.R --args 7.0.0 $i 10 output/wr-era data/wr-era/processed mae
    Rscript sim/wr-era-7.0.0-2.R --args 7.0.0 $i 10 output/wr-era data/wr-era/processed mzoe
done 

Rscript output/combine_files.R output/wr-era 7.0.0 2
