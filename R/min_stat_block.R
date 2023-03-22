#' min_stat_block
#' 
#' A function to calculate to calculate test statistics for null hypothesis: kth smallest treatment effect is less or equal to $c$.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param Y A numeric vector contains response values of the group.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param k A parameter in hypothesis test that kth smallest treatment effect is less or equal to $c$.
#' @param method.list.all A list that contains the method of the rank scores. If you use the same methods across all strata, method.list.all will only include 1 element stating that score. Otherwise, \code{method.list.all[[s]]} should be the method for stratum $s$. Available score functions: \code{Wilcoxon, list(name = "Wilcoxon)}; \code{Stephenson, list(name = "Stephenson", s)}.  
#' @param opt.method Algorithm that is used for optimization. Available algorithms are "Greedy", "DP", "Mcknap", "LP", "ILP" and "LP_gurobi", "ILP_gurobi", "PWL_gurobi", "PWLint_gurobi". Gurobi installation is required for gurobi to be used.
#' @param ties A subvector of \code{c("upper", "lower", "fix")} indicating which tie-dealing methods we use to calculate statistics. "upper" will use the method that will produce maximum statistic, while "lower" will get minimum. "fix" however will order the ties the same way as "first" method in rank function.
#' @import Rcpp
#' @import RcppArmadillo
#

min_stat_block <- function(Z, Y, block, k, c, method.list.all, opt.method="Greedy", ties = c("upper", "lower", "fix")){
  if(opt.method %in% c("LP_gurobi", "ILP_gurobi","PWL_gurobi","PWLint_gurobi")){
    if(!require(gurobi)){
      return(warning(" Gurobi should be installed before corresponding methods can be used. ", call. = FALSE))
    }
  }
  N = length(Z)
  if(!is.factor(block)){
    block = as.factor(block)
  }
  levels(block) = 1:length(levels(block))
  output = list()
  comb = matrix(nrow = N, ncol = 3)
  if("upper" %in% ties){
    flag = (Z==0)
    s = sum(flag)
    comb[1:s,2] = Y[flag]
    comb[(s+1):N,2] = Y[!flag]
    comb[1:s,3] = block[flag]
    comb[(s+1):N,3] = block[!flag]
    comb[,1] = c(rep(0, s), rep(1, N-s))
    comb = comb[order(comb[,2]),]
    
    Z2 = comb[,1]
    Y2 = comb[,2]
    block2 = comb[,3]
    
    coeflist = test_stat_matrix_block(Z2, Y2, block2, c, method.list.all)
    
    if(opt.method == "Greedy"){
      stat = LpGreedy_On_C(coeflist, N - k)$obj
    }
    if(opt.method == "DP"){
      stat = DP_C(coeflist, N - k)$obj

    }
    if(opt.method == "LP"){

      stat = Lpsolve_sol(coeflist, N - k, exact = FALSE)
    }
    if(opt.method == "ILP"){
      stat = Lpsolve_sol(coeflist, N - k, exact = TRUE)
    }
    if(opt.method == "LP_gurobi"){

      stat = Gurobi_sol(coeflist, N - k, exact = FALSE)$obj
    }
    if(opt.method == "ILP_gurobi"){
      stat = Gurobi_sol(coeflist, N - k, exact = TRUE)$obj
    }
    if(opt.method == "PWL_gurobi"){
      stat = Gurobi_sol_PL(coeflist, N-k, exact = FALSE)$obj
    }
    if(opt.method == "PWLint_gurobi"){
      stat = Gurobi_sol_PL(coeflist, N-k)$obj
    }
    output$upper = stat
  }
  if("lower" %in% ties){
    flag = (Z==1)
    s = sum(flag)
    comb[,3] = block
    comb[1:s,2] = Y[flag]
    comb[(s+1):N,2] = Y[!flag]
    comb[1:s,3] = block[flag]
    comb[(s+1):N,3] = block[!flag]
    comb[,1] = c(rep(1, s), rep(0, N-s))
    comb = comb[order(comb[,2]),]
    
    Z1 = comb[,1]
    Y1 = comb[,2]
    block1 = comb[,3]
    coeflist = test_stat_matrix_block(Z1, Y1, block1, c, method.list.all)
    if(opt.method == "Greedy"){
      stat = LpGreedy_On_C(coeflist, N - k)$obj
    }
    if(opt.method == "DP"){
      stat = DP_C(coeflist, N - k)$obj
    }
    if(opt.method == "LP"){
      stat = Lpsolve_sol(coeflist, N - k, exact = FALSE)
    }
    if(opt.method == "ILP"){
      stat = Lpsolve_sol(coeflist, N - k, exact = TRUE)
    }  
    if(opt.method == "LP_gurobi"){
      stat = Gurobi_sol(coeflist, N - k, exact = FALSE)$obj
    }
    if(opt.method == "ILP_gurobi"){
      stat = Gurobi_sol(coeflist, N - k, exact = TRUE)$obj
    }
    if(opt.method == "PWL_gurobi"){
      stat = Gurobi_sol_PL(coeflist, N-k, exact = FALSE)$obj
    }
    if(opt.method == "PWLint_gurobi"){
      stat = Gurobi_sol_PL(coeflist, N-k)$obj
    }
    output$lower = stat
  }
  if("fix" %in% ties){
    comb[,1] = Z
    comb[,2] = Y
    comb[,3] = block
    comb = comb[order(comb[,2]),]
    Z3 = comb[,1]
    Y3 = comb[,2]
    block3 = comb[,3]
    coeflist = test_stat_matrix_block(Z3, Y3, block3, c, method.list.all)
    if(opt.method == "Greedy"){
      stat = LpGreedy_On_C(coeflist, N - k)$obj
    }
    if(opt.method == "DP"){
      stat = DP_C(coeflist, N - k)$obj
    }
    if(opt.method == "LP"){
      stat = Lpsolve_sol(coeflist, N - k, exact = FALSE)
    }
    if(opt.method == "ILP"){
      stat = Lpsolve_sol(coeflist, N - k, exact = TRUE)
    } 
    if(opt.method == "LP_gurobi"){
      stat = Gurobi_sol(coeflist, N - k, exact = FALSE)$obj
    }
    if(opt.method == "ILP_gurobi"){
      stat = Gurobi_sol(coeflist, N - k, exact = TRUE)$obj
    }
    if(opt.method == "PWL_gurobi"){
      stat = Gurobi_sol_PL(coeflist, N-k, exact = FALSE)$obj
    }
    if(opt.method == "PWLint_gurobi"){
      stat = Gurobi_sol_PL(coeflist, N-k)$obj
    }
    output$fix = stat
  }
  return(output)
}
