#' sen_ls_p
#' 
#' A function to calculate valid p-value for null hypothesis: kth smallest treatment effect is less or equal to c.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param k A parameter in hypothesis test that at least k units have treatment effects less or equal to c
#' @param c A parameter in hypothesis test that at least k units have treatment effects less or equal to c
#' @param gam A parameter for sensitivity analysis
#' @param method.list.all A list that contains the method of the rank scores. It might vary among different blocks
#' @param opt.method Algorithm that is used for optimization. Available algorithms are "Greedy", "DP", "Mcknap, "LP", "ILP".
#' @param switch Logical variable. If true, the function uses switching treatment and control trick when calculating p-value.

#' @export


sen_ls_p = function(Z,Y,block,k,c,gam,method.list.all,opt.method="Greedy", switch=FALSE){
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
  stat = min_stat_block(Z,Y,block,k,c,method.list.all, opt.method, tie = c("upper", "lower"))
  pval = list(upper = pnorm(stat$lower, mean = u, sd = sqrt(v), lower.tail = FALSE)
              , lower = pnorm(stat$upper, mean = u, sd = sqrt(v), lower.tail = FALSE))
  return(pval)
}
