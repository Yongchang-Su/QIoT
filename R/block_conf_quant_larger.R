#' block_conf_quant_larger
#' 
#' A function to help determine confidence region of treatment effects under SCRE.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param quantiles Specify the quantiles of treatment effects that you want to calculate confidence intervals. It should be a subvector of integers from 1 to number of units. If NULL, it will calculate all quantiles.
#' @param alternative Options: "greater", "less", "two.sided", which will give confidence intervals. Default is "less".
#' @param method.list.all A list that contains the type of the rank scores. It should be a list with length equal to the number of strata. Default is \code{NULL}.
#' @param opt.method Algorithm that is used for optimization. Available algorithms are "Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi". Gurobi installation is required for gurobi to be used. Default is "Greedy"
#' @param ties Options: "upper", "lower", "fix" indicating which tie-dealing methods we use to calculate confidence intervals. "upper" will use the method that will produce maximum statistic, while "lower" will get minimum. "fix" however will order the ties the same way as "first" method in rank function. Default is "fix".
#' @param stat.null Null distribution of test statistics. If no input, then it will be generated automatically.
#' @param switch Logical variable. If true, the function uses switching treatment and control label is the number of treated units is less than that of control units in one stratum. Default is "False".
#' @param null.max The total amount of values we use to approximate the null distribution. Default is $10^5$.
#' @param alpha scalar. Confidence level of the confidence region. Default is $0.1$.
#' @noRd
#' @keywords internal


block_conf_quant_larger <- function(Z, Y, block, quantiles = NULL, method.list.all = NULL, opt.method = 'Greedy', ties = "fix", stat.null = NULL, switch = FALSE, null.max = 10^5,  alpha = 0.1){
  
  if(!is.factor(block)){
    block = as.factor(block)
  } 
  levels(block) = 1:length(levels(block))
  n = length(Z)
  mn = sum(Z)
  B = length(levels(block))
  
  if(switch){
    nb = rep(NA, B)
    zb = rep(NA, B)
    for(i in 1:B){
      nb[i] = sum(block == levels(block)[i])
      zb[i] = sum(Z[block == levels(block)[i]])
    }   
    for(i in 1:B){
      if(zb[i]<nb[i]/2){
        Z[block == levels(block)[i]] = 1 - Z[block == levels(block)[i]]
        Y[block == levels(block)[i]] = -Y[block == levels(block)[i]]
      } 
    }
  }
  
  if(is.null(stat.null)){
    stat.null =null_dist_block_C(Z,block, method_list_all = method.list.all, null_max = null.max)
  }
  
  
  
  # > threshold <===> p-value <= alpha #
  thres = sort(stat.null, decreasing = TRUE)[ floor(length(stat.null) * alpha) + 1] 
  cup = max(Y[Z==1]) - min(Y[Z==0]) + 0.01
  cdown = min(Y[Z==1]) - max(Y[Z==0]) - 0.01
  cmid = (cup+cdown)/2
  if(!is.null(quantiles)){
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
  }else{
    c_conf1 = rep(NA, n)
    # range of c #
    
    for(k in n:(n-mn)){
      up = unlist(min_stat_block(Z,Y,block,k,cdown,method.list.all,opt.method, ties))
      if(up>thres & k!= n){
        c_conf1[k] = cmid
        next
      }
      cdown = min(Y[Z==1]) - max(Y[Z==0]) - 0.01
      up = unlist(min_stat_block(Z,Y,block,k,cdown,method.list.all,opt.method, ties))
      if(up<thres){
        c_conf1[1:k] = -Inf
        break
      }else{
        repeat{
          cmid = (cup+cdown)/2 
          mid = unlist(min_stat_block(Z,Y,block,k,cmid,method.list.all,opt.method, ties))
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
  }
  
  return(c_conf1 )
}
