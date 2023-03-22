#'  pval_quantile_sen
#' 
#' A function to calculate valid p-value under sensitivity analysis for null hypothesis with respect to treatment effects.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param k A parameter in hypothesis test that at least k units have treatment effects less or equal to c
#' @param c A parameter in hypothesis test that at least k units have treatment effects less or equal to c
#' @param gam A parameter in sensitivity analysis controlling the odds of propensity scores.
#' @param method.list.all A list that contains the type of the rank scores. It should be a list with length equal to the number of strata. Default is \code{NULL}.
#' @param opt.method Algorithm that is used for optimization. Available algorithms are "Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi". Gurobi installation is required for gurobi to be used. Default is "Greedy"
#' @param ties A subvector of \code{c("upper", "lower", "fix")} indicating which tie-dealing methods we use to calculate statistics. "upper" will use the method that will produce maximum statistic, while "lower" will get minimum. "fix" however will order the ties the same way as "first" method in rank function. Default is \code{c("upper", "lower", "fix")}.
#' @param switch Logical variable. If true, the function uses switching treatment and control trick when calculating p-value. Default is "False".
#' @returns A list contains upper and lower bound and a special value of $p$-values depending on the tie-dealing method.
#' @examples 111
#'  
#' @export


pval_quantile_sen = function(Z,Y,block,k,c,gam,method.list.all,opt.method="Greedy", ties = c("upper", "lower", "fix"), switch=FALSE){
  if(!is.factor(block)){
    block = as.factor(block)
  } 
  B = length(levels(block))
  nb = rep(NA, B)
  zb = rep(NA, B)
  for(i in 1:B){
    nb[i] = sum(block == levels(block)[i])
    zb[i] = sum(Z[block == levels(block)[i]])
  }   
  u = 0
  v = 0
  if(switch){
    for(i in 1:B){
      if(zb[i]<nb[i]/2){
        Z[block == levels(block)[i]] = 1 - Z[block == levels(block)[i]]
        Y[block == levels(block)[i]] = -Y[block == levels(block)[i]]
      } 
    }
    for(i in 1:B){
      if(length(method.list.all)==1){
        method.list = method.list.all[[1]]
      }else{
        method.list = method.list.all[[i]]
      }
      r = rank_score(nb[i], method.list)
      m = max_mv(-r, gam)
      u = u + m$u
      v = v + m$v
      u = u + sum(r)
    }
  }else{
    for(i in 1:B){
      if(length(method.list.all)==1){
        method.list = method.list.all[[1]]
      }else{
        method.list = method.list.all[[i]]
      }
      m = max_mv(rank_score(nb[i], method.list), gam)
      u = u + m$u
      v = v + m$v
    }
  }
  pval = list()
  if("upper" %in% ties){
    stat = min_stat_block(Z, Y, block, k = k, c, method.list.all, opt.method = opt.method, ties = c("lower"))
    pval$upper = pnorm(stat$lower, mean = u, sd = sqrt(v), lower.tail = FALSE)
  }
  if("lower" %in% ties){
    stat = min_stat_block(Z, Y, block, k = k, c, method.list.all, opt.method = opt.method, ties = c("upper"))
    pval$lower = pnorm(stat$upper, mean = u, sd = sqrt(v), lower.tail = FALSE)
  }  
  if("fix" %in% ties){
    stat = min_stat_block(Z, Y, block, k = k, c, method.list.all, opt.method = opt.method, ties = c("fix"))
    pval$fix = pnorm(stat$fix, mean = u, sd = sqrt(v), lower.tail = FALSE)
  }
  return(pval)
}
