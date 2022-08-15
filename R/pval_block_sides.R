#' pval_block_sides
#' 
#' A function to calculate valid p-value for generalized null hypothesis: two-sided or 2 one-sided tests
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param k A parameter in hypothesis test
#' @param c A parameter in hypothesis test
#' @param alternative Options: "greater", "less", "two.sided", each defines differents alternative hypothesis
#' @param Gam A parameter for sensitivity analysis. If Gam = 1, the analysis is performed under normal setting. If Gam>1, then the analysis is under sensitivity setting.
#' @param method.list.all A list that contains the method of the rank scores. It might vary among different blocks
#' @param opt.method Algorithm that is used for optimization. Available algorithms are "Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi". Gurobi installation is required for gurobi to be used.
#' @param null.max The total amount of values we use to approximate the null distribution.
#' @export


pval_block_sides <- function(Z, Y, block, k, c, alternative = "greater", Gam=1, method.list.all = NULL, null.max = 10^5 ){
  if(alternative == "two.sided"){
    pval1 = pval_block(Z, Y, block, k, c,method.list.all, Gam, opt.method , null.max)
    pval2 = pval_block(Z, -Y, block, n+1-k, -c,method.list.all, Gam, opt.method, null.max)
    pval = list(upper = 2*max(pval1$upper,pval2$upper), lower = 2*max(pval1$lower,pval2$lower))
    return(pval)
  }  
  else if(alternative == "less"){
    Y = -Y
    k = n + 1 - k
    c = -c
    
  }
  pval = pval_block(Z, Y, block, k, c,method.list.all, Gam, opt.method , null.max)
  return(pval)
  
}