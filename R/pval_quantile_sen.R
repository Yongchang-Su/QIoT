#'  pval_quantile_sen
#' 
#' A function to calculate valid p-value under sensitivity analysis for null hypothesis with respect to treatment effects. Specifically, we are interested in testing kth smallest treatment effect is less than, greater than or not equal to c.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param k A parameter in the null hypothesis.
#' @param c A parameter in the null hypothesis.
#' @param alternative Options: \code{"greater", "less", "two.sided"}, which correspond to different relationships between kth smallest treatment effect and c. For example, \code{alternative == "greater"} denotes null hypothesis that the kth smallest treatment effect is greater than c. Default is "less".
#' @param gam A parameter in sensitivity analysis controlling the odds of propensity scores. Default is 1.
#' @param method.list.all A list that contains the type of the rank scores. It should be a list with length equal to the number of strata. Default is \code{NULL}.
#' @param opt.method Algorithm that is used for optimization. Available algorithms are \code{"Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi"}. Gurobi installation is required for gurobi to be used. Default is \code{"Greedy"}.
#' @param ties A subvector of \code{c("upper", "lower", "fix")} indicating which tie-dealing methods we use to calculate statistics. "upper" will use the method that will produce maximum statistic, while "lower" will get minimum. "fix" however will order the ties the same way as "first" method in rank function. Default is \code{c("upper", "lower", "fix")}.
#' @returns A list contains upper and lower bound and a special value of $p$-values depending on the tie-dealing method.
#' @examples 
#' data("cadmium")
#' Y = cadmium$cadmium
#' block = cadmium$mset
#' Z = cadmium$z
#' ### use stratified Wilcoxon test statistics for all strata. 
#' method.list.all = list()
#' method.list.all[[1]] = list(name = "Wilcoxon") 
#' ### Test null hypothesis that 90% quantile of treatment effects is less than or equal to 0 under Gamma = 3.
#' p1 = pval_quantile_sen(Z=Z,Y=Y,block=block,k=floor(0.9*length(Y)),c=0, gam = 3, method.list.all=method.list.all)
#' ### Test null hypothesis that 90% quantile of treatment effects is greater than or equal to 1 under Gamma = 3.
#' p2 = pval_quantile_sen(Z=Z,Y=Y,block=block,k=floor(0.9*length(Y)),c=1,alternative="greater",gam=3,method.list.all=method.list.all)

#' @export


pval_quantile_sen = function(Z,Y,block,k,c,alternative = "less", gam = 1, method.list.all = NULL, opt.method="Greedy", ties = c("upper", "lower", "fix")){
  n = length(Z)
  if(alternative == "two.sided"){
    pval1 = sen_ls(Z, Y, block, k, c, gam, method.list.all, opt.method, ties)
    pval2 = sen_ls(Z, -Y, block, n+1-k, -c, gam, method.list.all, opt.method)
    pval = list()
    if("upper" %in% ties){
      pval$upper = 2*min(pval1$upper, pval2$upper)
    }
    if("lower" %in% ties){
      pval$lower = 2*min(pval1$lower, pval2$lower)
    }
    if("fix" %in% ties){
      pval$fix = 2*min(pval1$fix, pval2$fix)
    }
    return(pval)
  }  
  else if(alternative == "greater"){
    Y = -Y
    k = n + 1 - k
    c = -c
    
  }else{
    warnings("Invalid input for alternative")
  }
  pval = sen_ls(Z, Y, block, k, c, gam, method.list.all, opt.method, ties)
  return(pval)
  
}
