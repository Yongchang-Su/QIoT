#' sen_block_conf_quant_larger
#' 
#' A function to help determine confidence region with respect to k and c in sensitivity analysis
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param quantiles Specify which quantiles of treatment effects that you want to calculate confidence intervals. It should be a subvector of integers from 1 to number of units. If \code{quantiles = NULL}, it will calculate all quantiles. Default is NULL.
#' @param gam A parameter for sensitivity analysis
#' @param method.list.all A list that contains the method of the rank scores. It might vary among different blocks
#' @param opt.method Algorithm that is used for optimization. Available algorithms are "Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi". Gurobi installation is required for gurobi to be used.
#' @param ties ties = "upper", "lower" or "fix". It indicates which tie-dealing methods we use to calculate statistics.
#' @param switch Logical variable. If true, the function uses switching treatment and control trick when calculating p-value.
#' @param alpha scalar. It equals to 1-confidence of the confidence region
#' @noRd
#' @keywords internal


sen_block_conf_quant_larger <- function(Z, Y, block, quantiles = NULL, gam = 1, method.list.all = NULL, opt.method = 'Greedy', ties = "fix",switch = FALSE, alpha = 0.1){
  if(!is.factor(block)){
    block = as.factor(block)
  } 
  levels(block) = 1:length(levels(block))
  n = length(Y)
  B = length(levels(block))
  mn = sum(Z)
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
  thres = qnorm(1-alpha, mean = u, sd = sqrt(v))
  cup = max(Y[Z==1]) - min(Y[Z==0]) + 0.01
  cdown = min(Y[Z==1]) - max(Y[Z==0]) - 0.01
  cmid = (cup+cdown)/2
  if(is.null(quantiles)){
    quantiles = 1:n
  }
  quantiles = sort(quantiles)
  c_conf1 = rep(NA, length(quantiles))
  for (k in length(quantiles):1) {
    up = unlist(min_stat_block(Z,Y,block,quantiles[k],cdown,method.list.all,opt.method, ties))
    if(up>thres & quantiles[k]!= n){
      c_conf1[k] = cmid
      next
    }
    cdown = min(Y[Z==1]) - max(Y[Z==0]) - 0.01
    up = unlist(min_stat_block(Z,Y,block,quantiles[k],cdown,method.list.all,opt.method, ties))
    if(up<thres){
      c_conf1[1:k] = -Inf
      break
    }else{
      repeat{
        cmid = (cup+cdown)/2 
        mid = unlist(min_stat_block(Z,Y,block,quantiles[k],cmid,method.list.all,opt.method, ties))
        if(mid>thres){
          cdown = cmid
        }else{
          cup = cmid
        }
        
        if(abs(cup-cdown)<(1e-6)){
          c_conf1[k] = cmid
          break
        }
      }
    }      
  }
  return(c_conf1)
}