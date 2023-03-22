#' pval_block
#' 
#' A function to calculate valid p-value for null hypothesis: kth smallest treatment effect is less or equal to c.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param k A parameter in hypothesis test that kth smallest treatment effect is less or equal to c
#' @param c A parameter in hypothesis test that kth smallest treatment effect is less or equal to c
#' @param method.list.all A list that contains the method of the rank scores. It might vary among different blocks
#' @param opt.method Algorithm that is used for optimization. Available algorithms are "Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi". Gurobi installation is required for gurobi to be used.
#' @param ties A subvector of \code{c("upper", "lower", "fix")} indicating which tie-dealing methods we use to calculate statistics. "upper" will use the method that will produce maximum statistic, while "lower" will get minimum. "fix" however will order the ties the same way as "first" method in rank function.
#' @param null.max The total amount of values we use to approximate the null distribution.
#' @param stat.null Optional variable. You can enter the null distribution here by yourself. Otherwise it will be generated within the function.
#' @param switch Logical variable. If true, the function uses switching treatment and control trick when calculating p-value.


pval_block <- function(Z, Y, block, k, c,method.list.all, opt.method = "Greedy", ties = c("upper", "lower", "fix"),null.max = 10^5, stat.null = NULL, switch = FALSE){
  if(!is.factor(block)){
    block = as.factor(block)
  } 
  levels(block) = 1:length(levels(block))
  if(is.null(stat.null)){
    stat.null = null_dist_block(Z, block, method_list_all = method.list.all, null_max = null.max)
  }
  pval = list()
  if("upper" %in% ties){
    stat = min_stat_block(Z, Y, block, k = k, c, method.list.all, opt.method = opt.method, ties = c("lower"))
    pval$upper = mean(stat.null+1e-10>= stat$lower)
  }
  if("lower" %in% ties){
    stat = min_stat_block(Z, Y, block, k = k, c, method.list.all, opt.method = opt.method, ties = c("upper"))
    pval$lower = mean(stat.null+1e-10>= stat$upper)
  }  
  if("fix" %in% ties){
    stat = min_stat_block(Z, Y, block, k = k, c, method.list.all, opt.method = opt.method, ties = c("fix"))
    pval$fix = mean(stat.null+1e-10>= stat$fix)
  }
  return(pval)
}
