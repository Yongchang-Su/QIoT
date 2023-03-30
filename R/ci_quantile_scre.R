#' ci_quantile_scre
#' 
#' A function to help determine confidence region of treatment effects under SCRE.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param quantiles Specify which quantiles of treatment effects that you want to calculate confidence intervals. It should be a subvector of integers from 1 to number of units. If \code{quantiles == NULL}, it will calculate all quantiles. Default is NULL.
#' @param alternative Options: \code{"lower"}, \code{"upper"}, \code{"two.sided"}, which will give lower one-sided, upper one-sided and two-sided confidence intervals, respectively. Default is "upper".
#' @param method.list.all A list that contains the type of the rank scores. It should be a list with length equal to the number of strata. Default is \code{NULL}.
#' @param opt.method Algorithm that is used for optimization. Available algorithms are \code{"Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi"}. Gurobi installation is required for gurobi to be used. Default is \code{"Greedy"}.
#' @param ties A subvector of \code{c("upper", "lower", "fix")} indicating which tie-dealing methods we use to calculate statistics. "upper" will use the method that will produce maximum statistic, while "lower" will get minimum. "fix" however will order the ties the same way as "first" method in rank function. Default is \code{c("upper", "lower", "fix")}.
#' @param stat.null Null distribution of test statistics. If no input, then it will be generated automatically.
#' @param switch Logical variable. If true, the function uses switching treatment and control label is the number of treated units is less than that of control units in one stratum. Default is "False".
#' @param null.max The total amount of values we use to approximate the null distribution. Default is \code{1e5}.
#' @param confidence scalar between 0 and 1. Confidence level of the confidence region. Default is \code{0.9}.
#' @returns A list contains upper and lower limits of confidence intervals. 
#' @examples 
#' data("cadmium")
#' Y = cadmium$cadmium
#' block = cadmium$mset
#' Z = cadmium$z
#' n = length(Z)
#' ### use stratified Wilcoxon test statistics for all strata. 
#' method.list.all = list()
#' method.list.all[[1]] = list(name = "Wilcoxon") 
#' ### Calculate 90% confidence intervals for the top 10 percent largest treatment effects.
#' CIs = ci_quantile_scre(Z,Y,block,quantiles=floor(0.9*n):n,alternative = "two.sided",method.list.all=method.list.all,opt.method = "Greedy", switch = TRUE, null.max=10^5)
#' 
#' @export


ci_quantile_scre <- function(Z, Y, block, quantiles = NULL, alternative = "upper", method.list.all = NULL, opt.method = 'Greedy', ties = "fix", stat.null = NULL, switch = FALSE, null.max = 10^5,  confidence = 0.9){
  res = list()
  alpha = 1 - confidence
  if(alternative == "upper"){
    LB = block_conf_quant_larger(Z, Y, block, quantiles, method.list.all, opt.method, ties, stat.null, switch, null.max,  alpha)
    res$LB = LB 
  }
  if(alternative == "lower"){
    UB = block_conf_quant_larger(Z, -Y, block, quantiles, method.list.all, opt.method, ties, stat.null, switch, null.max,  alpha)
    res$UB = -rev(UB)
  }
  if(alternative == "two.sided"){
    LB = block_conf_quant_larger(Z, Y, block, quantiles, method.list.all, opt.method, ties, stat.null, switch, null.max,  alpha/2)
    res$LB = LB    
    UB = block_conf_quant_larger(Z, -Y, block, quantiles, method.list.all, opt.method, ties, stat.null, switch, null.max,  alpha/2)
    res$UB = -rev(UB)    
  }else{
    warnings("Invalid input for alternative")
  }
    
  return(res)
}
