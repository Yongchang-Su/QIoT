 # QIoT

This package is build for quantiles inference of treatment effects. It mainly involves hypothesis testing and constructing confidence regions for treatment effects under stratified randomized experiments and observational studies.

## Install
with `devtools `:
```S
devtools::install_github("Yongchang-Su/QIoT")
```


## Usage

Here we make a brief introduction to the functions the packages contains.

- p_val_block: We are interested in null hypothesis with form $\tau_{k}\le c$, where $\tau_{1}\le \ldots \le \tau_{n}$ are treatment effects in ascending order. Different algorithms are available for the calculation, including greedy algorithm (efficient but only approximate), dynamic programming (exact but less efficient), as well as calculating with gurobi optimizer (installation of gurobi required). This function outputs valid $p$-values for the null hypothesis. In addition, different tie-dealing method might result in different $p$-values. Therefore in default, the function outputs both upper and lower bound as well as another special $p$-values that corresponds to control first, treatment first and first come first serve tie-dealing methods. If you want to do hypothesis testing in sensitivity analysis setting, you can enter a value for parameter gam.
- p_val_block_sides: This $p$-value calculating function generalize previous form of null hypotheses to $\tau_{k}\ge c$ and $\tau_{k}\ne c$.
- block_conf_quant_larger: This function outputs a vector of length $n$, with $i$-th element being the lower limit of confidence interval for $\tau_{i}$ with certain confidence. We showed in our paper that those confidence intervals combined forms the confidence region for all treatment effects with the same confidence.
- sen_ls_p: This function generates $p$-values in observational studies under large-sample approximation. This method performs more powerful test but only asymptotically valid.
- sen_block_conf_quant_larger: This is the version of block_conf_quant_larger in sensitivity analysis with different choice of gam. 

## Data
One set of data used in the paper is included in the package.

- cadmium: The data is originally collected by the US National Health and Nutrition Examination Survey, 2005-2006, to study how smoking would affect the cadmium level in human's blood. It was later cleaned and included in the package ``bigmatch''. The data contains 1536 observations and 34 variables.

## Sample application of the package

Here we share some sample application of our package.

```S
### Simulation data with normal(5, 1) distributed treatment effects
S = 10
n = 10
N = S*n
block = rep(1:S, each = n)
Z = rep(rep(c(0,1), 5), S)
Y = rnorm(S*n) + Z*(5+rnorm(S*n))

### Choose Stephenson rank sum as our test statistic
method.list.all = list()
method.list.all[[1]] = list(name = "Stephenson", s=3)

### Calculate p-values for null that 10th largest treatment effect is no more than 3
pval_block(Z, Y, block, k = 90, c = 3, method.list.all)
```
with output similar to
```S
$upper
[1] 0.00012

$lower
[1] 0.00012

$fix
[1] 0.00012
```
and the statement is clearly rejected at 5% significance level. Moreover, you can move on to more advanced inference like calculating confidence region of treatment effect vector.
```S
### Calculate lower limit of CIs of treatment effects
CI_lower = block_conf_quant_larger(Z, Y, block, method.list.all)
plot(CI_lower[CI_lower>-Inf], (1:length(CI_lower))[CI_lower>-Inf], type = "p", xlab = "c", ylab = "k", cex = 1.5, col = "black", pch = 18)
segments(CI_lower[CI_lower>-Inf], (1:length(CI_lower))[CI_lower>-Inf], 5, (1:length(CI_lower))[CI_lower>-Inf], lty= 2)
```
The visualization of the results looks like this:
![Title](https://i.postimg.cc/m2jD9RbR/Rplot.png)
The plot is 90% confidence region of all treatment effects ordered from largest to smallest. Note that only the largest 21 treatment effects are shown in the plot, as the rest are non-informative CIs. It tells us the lowest possible values of quantiles of treatment effects simultaneously with certain confidence.

