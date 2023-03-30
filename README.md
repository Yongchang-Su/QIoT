 # QIoT

This package is build for quantiles inference of treatment effects. It mainly involves hypothesis testing and constructing confidence regions for quantiles of treatment effects under stratified randomized experiments and observational studies.

## Install
with `devtools `:
```S
devtools::install_github("Yongchang-Su/QIoT")
```


## Usage

Here we make a brief introduction to the functions the packages contains.

- *pval_quantile_scre*: We are interested in null hypotheses like $\tau_{k}\le c$, where $\tau_{1}\le \ldots \le \tau_{n}$ are treatment effects in ascending order. Different algorithms are available for the calculation, including greedy algorithm (efficient but only approximately optimal), dynamic programming (exact but less efficient), as well as calculating with gurobi optimizer (installation of gurobi required). This function outputs valid $p$-values for the null hypothesis. Similarly, one can test null hypothese such as $\tau_{k}\ge c$ and $\tau_{k}\ne c$, with the help of the input parameter "alternative". In addition, different tie-dealing method might result in different $p$-values. Therefore in default, the function outputs both upper and lower bound as well as another special $p$-values that corresponds to control first, treatment first and first come first serve tie-dealing methods. 
- *pval_quantile_sen*: This function does the same hypothesis testing described in *pval_quantile_scre* under the sensitivity analysis setting. In addition, the $p$-value calculated here is only asymptotically valid.
- *ci_quantile_scre*: The function is built for calculating confidence region for treatment effects under stratified completely randomized experiment. The output of the function is a list containing upper bounds and lower bounds for effects. The "quantiles" parameter allows user to specify the quantiles of treatment effects she wants to calculate. The "alternative" parameter determines whether lower or upper one-sided, or two-sided confidence interval is favored.
- *ci_quantile_sen*: The function calculates confidence region for treatment effects under sensitivity analysis, with the rest of the settings similar to that of *ci_quantile_scre*.

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
pval_quantile_scre(Z, Y, block, k = 90, c = 3, method.list.all = method.list.all)
```
with output similar to
```S
$upper
[1] 0.00092

$lower
[1] 0.00092

$fix
[1] 0.00092
```
and the statement is clearly rejected at 5% significance level. Moreover, you can move on to more advanced inference like calculating confidence region of treatment effect vector.
```S
### Calculate lower limit of CIs of treatment effects
CIs = ci_quantile_scre(Z, Y, block, method.list.all = method.list.all)
CI_lower = CIs$LB
plot(CI_lower[CI_lower>-Inf], (1:length(CI_lower))[CI_lower>-Inf], type = "p", xlab = "c", ylab = "k", cex = 1.5, col = "black", pch = 18)
segments(CI_lower[CI_lower>-Inf], (1:length(CI_lower))[CI_lower>-Inf], 5, (1:length(CI_lower))[CI_lower>-Inf], lty= 2)
```
The visualization of the results looks like this:
![Title](https://i.postimg.cc/m2jD9RbR/Rplot.png)
The plot is 90% confidence region of all treatment effects ordered from largest to smallest. Note that only the largest 21 treatment effects are shown in the plot, as the rest are non-informative CIs. It tells us the lowest possible values of quantiles of treatment effects simultaneously with certain confidence.

