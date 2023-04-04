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
#' ### Calculate 90% confidence intervals for the top 10 percent largest treatment effects under Gamma = 3.
#' CIs = ci_quantile_sen(Z,Y,block,gam=3,quantiles=floor(0.9*n):n,alternative = "two.sided",method.list.all=method.list.all,opt.method = "Greedy", switch = TRUE, confidence = 0.9)
#' 
#' @export

ci_quantile_sen <- function(Z, Y, block, gam = 1, quantiles=NULL,alternative = "upper", method.list.all = NULL, opt.method = 'Greedy', ties = "fix",switch = FALSE, confidence = 0.9){
  if(!is.vector(Z, mode = "integer")){stop("Z should be a binary vector")}
  if(!(alternative %in% c("lower", "upper", "two.sided"))){stop("Invalid input for alternative")}
  if(!is.list(method.list.all)){stop("method.list.all should be a list")}
  if(!(opt.method %in% c("Greedy", "DP", "Mcknap", "LP", "ILP", "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi"))){stop("Invalid input for opt.method")}
  
  alpha = 1 - confidence
  res = list()
  if(alternative == "upper"){
    LB = sen_block_conf_quant_larger(Z, Y, block, quantiles, gam, method.list.all, opt.method, ties, switch, alpha)
    res$LB = LB
  }
  if(alternative == "lower"){
    UB = sen_block_conf_quant_larger(Z, -Y, block, quantiles, gam, method.list.all, opt.method, ties, switch, alpha)
    res$UB = -rev(UB)
  }
  if(alternative == "two.sided"){
    LB = sen_block_conf_quant_larger(Z, Y, block, quantiles, gam, method.list.all, opt.method, ties, switch, alpha/2)
    res$LB = LB    
    UB = sen_block_conf_quant_larger(Z, -Y, block, quantiles, gam, method.list.all, opt.method, ties, switch, alpha/2)
    res$UB = -rev(UB)    
  }else{
    warnings("Invalid input for alternative")
  }
  
  return(res)
}
