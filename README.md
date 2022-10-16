# QIoT

This package is build for quantiles inference of treatment effects. It mainly involves hypothesis testing and constructing confidence regions for treatment effects under stratified randomized experiments and observational studies.

## Install
with `devtools `:
```S
devtools::install_github("Yongchang-Su/QIoT")
```


## Usage

Here we make a brief introduction to the functions the packages contains.

- null_dist_block: This function generates the distribution of statistics under randomized assumption. It allows both Monte Carlo approximation and exact distribution. This function is exported to users that requires repeated calculation of $p$-values, so that they can generate the null distribution once and use it as an input to calculate $p$-values in p_val_block or p_val_block_sides.
- min_stat_block: We are interested in null hypothesis with form $\tau_{k}\le c$, where $\tau_{1}\le \ldots \le \tau_{n}$ are treatment effects in ascending order. This function calculates the statistics to test the hypothesis. Different algorithms are available for the calculation, including greedy algorithm (efficient but only approximate), dynamic programming (exact but less efficient), as well as calculating with gurobi optimizer (installation of gurobi required).
- p_val_block: This function outputs valid $p$-values for the null hypothesis mentioned in min_stat_block.
- p_val_block_sides: This $p$-value calculating function generalize previous results by allowing null hypotheses with forms $\tau_{k}\ge c$ and $\tau_{k}\ne c$.
- block_conf_quant_larger: This function outputs a vector of length $n$, with $i$-th element being the lower limit of confidence interval for $\tau_{i}$ with certain confidence. We showed in our paper that those confidence intervals combined forms the confidence region for all treatment effects with the same confidence.
- sen_ls_p: This function generates $p$-values in observational studies under large-sample approximation.
- sen_block_conf_quant_larger: This is the version of block_conf_quant_larger in sensitivity analysis.