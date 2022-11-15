 # QIoT

This package is build for quantiles inference of treatment effects. It mainly involves hypothesis testing and constructing confidence regions for treatment effects under stratified randomized experiments and observational studies.

## Install
with `devtools `:
```S
devtools::install_github("Yongchang-Su/QIoT")
```


## Usage

Here we make a brief introduction to the functions the packages contains.

- p_val_block: We are interested in null hypothesis with form $\tau_{k}\le c$, where $\tau_{1}\le \ldots \le \tau_{n}$ are treatment effects in ascending order. Different algorithms are available for the calculation, including greedy algorithm (efficient but only approximate), dynamic programming (exact but less efficient), as well as calculating with gurobi optimizer (installation of gurobi required). This function outputs valid $p$-values for the null hypothesis. In addition, different tie-dealing method might result in different $p$-values. Therefore in default, the function outputs both upper and lower bound as well as another special $p$-values that corresponds to control first, treatment first and first come first serve tie-dealing methods.
- p_val_block_sides: This $p$-value calculating function generalize previous form of null hypotheses to $\tau_{k}\ge c$ and $\tau_{k}\ne c$.
- block_conf_quant_larger: This function outputs a vector of length $n$, with $i$-th element being the lower limit of confidence interval for $\tau_{i}$ with certain confidence. We showed in our paper that those confidence intervals combined forms the confidence region for all treatment effects with the same confidence.
- sen_ls_p: This function generates $p$-values in observational studies under large-sample approximation.
- sen_block_conf_quant_larger: This is the version of block_conf_quant_larger in sensitivity analysis.

## Data
One set of data used in the paper is included in the package.

- cadmium: The data is originally collected by the US National Health and Nutrition Examination Survey, 2005-2006, to study how smoking would affect the cadmium level in human's blood. It was later cleaned and included in the package ``bigmatch''. The data contains 1536 observations and 34 variables.

## Sample application of the package

Here we share some sample application of our package.

```S

```
