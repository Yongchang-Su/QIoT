#' pval_quantile_scre
#' 
#' A function to calculate valid p-value under SCRE for null hypothesis with respect to treatment effects.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param k A parameter in the null hypothesis.
#' @param c A parameter in the null hypothesis.
#' @param alternative Options: "greater", "less", "two.sided", which correspond to null hypotheses in forms of $\tau_k\ge c$, $\tau_k\le c$, $\tau_k\ne c$, respectively. Default is "less".
#' @param method.list.all A list that contains the type of the rank scores. It should be a list with length equal to the number of strata. Default is \code{NULL}.
#' @param opt.method Algorithm that is used for optimization. Available algorithms are "Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi". Gurobi installation is required for gurobi to be used. Default is "Greedy"
#' @param ties A subvector of \code{c("upper", "lower", "fix")} indicating which tie-dealing methods we use to calculate statistics. "upper" will use the method that will produce maximum statistic, while "lower" will get minimum. "fix" however will order the ties the same way as "first" method in rank function. Default is \code{c("upper", "lower", "fix")}.
#' @param null.max The total amount of values we use to approximate the null distribution. Default is $10^5$.
#' @returns A list contains upper and lower bound and a special value of $p$-values depending on the tie-dealing method.
#' @examples
#' data("cadmium")
#' Y = cadmium$cadmium
#' block = cadmium$mset
#' Z = cadmium$z
#' method.list.all = list()
#' method.list.all[[1]] = list(name = "Wilcoxon") 
#' pval_quantile_scre(Z=Z,Y=Y,block=block,k=floor(0.8*length(Y)),c=0,method.list.all=method.list.all)
#' @export


pval_quantile_scre <- function(Z, Y, block, k, c, alternative = "less", method.list.all = NULL, opt.method = "Greedy", ties = c("upper", "lower", "fix"), null.max = 10^5 ){
  if(alternative == "two.sided"){
    pval1 = pval_block(Z, Y, block, k, c,method.list.all,opt.method, ties , null.max)
    pval2 = pval_block(Z, -Y, block, n+1-k, -c,method.list.all, opt.method, ties , null.max)
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
    
  }
  pval = pval_block(Z, Y, block, k, c,method.list.all, opt.method, ties , null.max)
  return(pval)
  
}