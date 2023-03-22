#' ci_quantile_sen
#' 
#' A function to help determine confidence region with respect to k and c in sensitivity analysis
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param gam A parameter for sensitivity analysis. Default is 1.
#' @param quantiles Specify which quantiles of treatment effects that you want to calculate confidence intervals. It should be a subvector of integers from 1 to number of units. If \code{quantiles == NULL}, it will calculate all quantiles. Default is NULL.
#' @param alternative Options: \code{"lower"}, \code{"upper"}, \code{"two.sided"}, which will give lower one-sided, upper one-sided and two-sided confidence intervals, respectively. Default is "upper".
#' @param method.list.all A list that contains the type of the rank scores. It should be a list with length equal to the number of strata. Default is \code{NULL}.
#' @param opt.method Algorithm that is used for optimization. Available algorithms are \code{"Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi"}. Gurobi installation is required for gurobi to be used. Default is \code{"Greedy"}.
#' @param ties A subvector of \code{c("upper", "lower", "fix")} indicating which tie-dealing methods we use to calculate statistics. "upper" will use the method that will produce maximum statistic, while "lower" will get minimum. "fix" however will order the ties the same way as "first" method in rank function. Default is \code{c("upper", "lower", "fix")}.
#' @param stat.null Null distribution of test statistics. If no input, then it will be generated automatically.
#' @param switch Logical variable. If true, the function uses switching treatment and control label is the number of treated units is less than that of control units in one stratum. Default is "False".
#' @param null.max The total amount of values we use to approximate the null distribution. Default is \code{1e5}.
#' @param alpha scalar. Confidence level of the confidence region. Default is \code{0.1}.
#' @returns A list contains upper and lower limits of confidence intervals. 
#' @examples 111
#' @export

ci_quantile_sen <- function(Z, Y, block, gam = 1, alternative = "upper", method.list.all = NULL, opt.method = 'Greedy', ties = "fix",switch = FALSE, null.max = 10^5,  alpha = 0.1){
  res = list()
  if(alternative == "upper"){
    LB = sen_block_conf_quant_larger(Z, Y, block, quantiles, gam, method.list.all, opt.method, ties, stat.null, switch, null.max,  alpha)
    res$LB = LB
  }
  if(alternative == "lower"){
    UB = sen_block_conf_quant_larger(Z, -Y, block, quantiles, gam, method.list.all, opt.method, ties, stat.null, switch, null.max,  alpha)
    res$LB = -rev(UB)
  }
  if(alternative == "two.sided"){
    LB = sen_block_conf_quant_larger(Z, Y, block, quantiles, gam, method.list.all, opt.method, ties, stat.null, switch, null.max,  alpha/2)
    res$LB = LB    
    UB = sen_block_conf_quant_larger(Z, -Y, block, quantiles, gam, method.list.all, opt.method, ties, stat.null, switch, null.max,  alpha/2)
    res$LB = -rev(UB)    
  }
  
  return(res)
}
