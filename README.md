# Simulations for Ordinal Multiple Instance Support Vector Machines

Striving for reproducibility, this repo details the simulations for *Ordinal Multiple Instance Support Vector Machines* by Sean Kent and Menggang Yu.  This allows for recreation of all figures and tables used in the paper, except in the case of proprietary data. Full details and descriptions are available in the paper. 

As a quick reference for getting started:

* The experimens can be run via the `run.sh` file, however this will take considerable CPU time. We recommend that you run in a high-throughput environment in batches (see `run.sh` for inspiration)
* Figures from the paper can be recreated by running `analysis/create-all-figures.R`.
* Data (open-source) can be downloaded by running `data/build-data.sh`. 
* Simulation source code can be found in the `sim/` directory.
* The `condor/` directory can be ignored. It was used to run simulations in the HTCondor (high-throughput) environment.


## Methods Compared

The main model code relies on the `mildsvm` package (v0.4.0) in R, which can be found here: https://github.com/skent259/mildsvm 

| Method name        | Function               | parameters                                      | Reference |
| ------------------ | ---------------------- | ----------------------------------------------- | --------- |
| OMI-SVM (proposed) | `mildsvm::omisvm`      | `method = "qp-heuristic"`                       | [1]       |
| MI-SVM (OVA)       | `mildsvm::misvm_orova` | `method = "qp-heuristic"`                       | [2], [3]  |
| SI-SVOREXC         | `mildsvm::svor_exc`    | `method = "smo"`                                | [4], [5]  |
| MIOR (as written)  | `mildsvm::mior`        | `method = "qp-heuristic", option = "xiao"`      | [6]       |
| MIOR (corrected)   | `mildsvm::mior`        | `method = "qp-heuristic", option = "corrected"` | [6]       |

## Experiments 

| Data set              | Experiment type      | Name      | ID  | Section    |
| --------------------- | -------------------- | --------- | --- | ---------- |
| Amazon review ratings | Performance          | per-amrev | 1.0 | Evaluation |
| tissue micro-array    | Performance          | per-tma   | 2.0 | Evaluation |
| IMDB movie reviews    | Training data size   | size-imdb | 3.0 | Evaluation |
| SWD                   | Training data size   | size-swd  | 4.0 | Appendix   |
| winequality-red       | Training data size   | size-wq   | 5.0 | Appendix   |
| CAR                   | Witness rate changes | wr-car    | 6.0 | Appendix   |
| ERA                   | Witness rate changes | wr-era    | 7.0 | Appendix   |


## References

[1] Kent, S., & Yu, M. (2023+). Ordinal multiple instance support vector machines. *In prep*.

[2] Andrews, S., Tsochantaridis, I., & Hofmann, T. (2003). Support vector machines for multiple-instance learning. *Advances in Neural Information Processing Systems* *15*, 577–584). 

[4] Hsu, C.-W., & Lin, C.-J. (2002). A comparison of methods for multiclass support vector machines. *IEEE Transactions on Neural Networks*, *13*(2), 415–425. https://doi.org/10.1109/72.991427

[4] Chu, W., & Keerthi, S. S. (2007). Support vector ordinal regression. *Neural Computation*, *19*(3), 792–815. https://doi.org/10.1162/neco.2007.19.3.792

[5] Alpaydın, E., Cheplygina, V., Loog, M., & Tax, D. M. J. (2015). Single- vs. Multiple-instance classification. *Pattern Recognition*, *48*(9), 2831–2838. https://doi.org/10.1016/j.patcog.2015.04.006

[6] Xiao, Y., Liu, B., & Hao, Z. (2018). Multiple-instance ordinal regression. *IEEE Transactions on Neural Networks and Learning Systems*, *29*(9), 4398–4413. https://doi.org/10.1109/TNNLS.2017.2766164
