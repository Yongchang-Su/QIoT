#' sen_ls
#' @noRd
#' @keywords internal


sen_ls = function(Z,Y,block,k,c,gam,method.list.all,opt.method="Greedy", ties = c("upper", "lower", "fix")){
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
  # if(switch){
  #   for(i in 1:B){
  #     if(zb[i]<nb[i]/2){
  #       Z[block == levels(block)[i]] = 1 - Z[block == levels(block)[i]]
  #       Y[block == levels(block)[i]] = -Y[block == levels(block)[i]]
  #     } 
  #   }
  #   for(i in 1:B){
  #     if(length(method.list.all)==1){
  #       method.list = method.list.all[[1]]
  #     }else{
  #       method.list = method.list.all[[i]]
  #     }
  #     r = rank_score(nb[i], method.list)
  #     m = max_mv(-r, gam)
  #     u = u + m$u
  #     v = v + m$v
  #     u = u + sum(r)
  #   }
  # }else{
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
  #}
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