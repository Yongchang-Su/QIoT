#' Algorithms that calculate test statistics
#' @import lpSolveAPI 


# LpDominateRemove = function(coeflist){
#   dictionary = coeflist
#   for(s in 1:length(coeflist)){
#     dictionary[[s]] = 1:length(coeflist[[s]])
#   }
#   
#   for(s in 1:length(coeflist)){
#     i = c(1,2,3)
#     r = coeflist[[s]]
#     d = dictionary[[s]]
#     while(i[3]<=ncol(r)){
#       if(r[2,i[3]] == r[2,i[2]]){
#         r = r[,-i[3],drop=FALSE]
#         d = d[-i[3]]
#         next
#       }
#       if((r[2,i[3]] - r[2,i[2]])/(r[1,i[3]] - r[1,i[2]]) >= (r[2,i[2]] - r[2,i[1]])/(r[1,i[2]] - r[1,i[1]])){
#         r = r[,-i[2],drop=FALSE]
#         d = d[-i[2]]
#         if(i[1] != 1){
#           i = i - c(1,1,1)
#         }
#       }else{
#         i = i + c(1,1,1)
#       }
#     }
#     coeflist[[s]] = r
#     dictionary[[s]] = d
#   }
#   return(list(coeflist = coeflist, dictionary = dictionary))
# }
# 
# ## a middle step drawn independently to avoid repeated calculation
# find_a = function(coeflist1){
#   ## obtain optimal gain under the same constraint
#   a1 = c()
#   a2 = c()
#   a3 = c()
#   for(s in 1:length(coeflist1)){
#     a1 = c(a1,diff(coeflist1[[s]][1,]))
#     a2 = c(a2,diff(coeflist1[[s]][2,]))
#     a3 = c(a3, rep(s,ncol(coeflist1[[s]])-1))
#   } 
#   a2 = a2/a1
#   a = rbind(a1,a2,a3)
#   a = a[, order(a[2,], decreasing = TRUE)]
#   return(a)
# }
# 
# ## O(n) method of Greedy algorithm
# On_median = function(x, k = NULL){
#   if(length(x) == 1){
#     return(x)
#   }
#   if(all(x == x[1])){
#     return(x[1])
#   }
#   n = length(x)
#   if(is.null(k)){
#     k = ceiling(n/2)
#   }
#   piv = sample(unique(x),1)
#   les = x[x<=piv]
#   grt = x[x>piv]
#   if(length(les)>=k){
#     return(On_median(les, k))
#   }else{
#     return(On_median(grt, k - length(les))) 
#   }
# }
# 
# LpGreedy_On = function(coeflist, p){
#   if(p<0){
#     return(list(obj=Inf, sol = "No feasible solution"))
#   }
#   ## convert to a maximization problem
#   sm = 0
#   p_w = c()
#   w = c()
#   strata = c()
#   coeflist1 = coeflist
#   for(s in 1:length(coeflist)){
#     sm = sm + coeflist[[s]][2,1]
#     coeflist1[[s]][2,] = coeflist[[s]][2,1] - coeflist[[s]][2,]
#   }
#   coeflist1 = LpDominateRemove(coeflist1)$coeflist
#   
#   for(s in 1:length(coeflist)){
#     p_w = c(p_w, diff(coeflist1[[s]][2,])/diff(coeflist1[[s]][1,]))
#     w = c(w, diff(coeflist1[[s]][1,]))
#     strata = c(strata, rep(s, ncol(coeflist1[[s]])-1))
#   }
#   S = 1:length(p_w)
#   obj = 0
#   grp = rep(1,s)
#   if(p >= sum(w)){
#     sol = coeflist
#     for(s in 1:length(coeflist)){
#       a = rep(0, ncol(coeflist[[s]]))
#       a[as.numeric(colnames(coeflist1[[s]])[ncol(coeflist1[[s]])])] = 1
#       sol[[s]] = a
#     }
#     obj = objcal(coeflist, sol)
#     return(list(obj = as.numeric(obj), sol = sol))
#   }
#   repeat{
#     p_w_S = p_w[S]
#     r = On_median(p_w_S)
#     H = S[p_w_S > r]
#     E = S[p_w_S == r]
#     L = S[p_w_S < r]
#     if(sum(w[H]) > p){
#       S = H
#     }else if(sum(w[H], w[E]) <= p){
#       S = L
#       p = p - sum(w[H], w[E])
#       obj = obj + sum(p_w[H]*w[H], p_w[E]*w[E])
#       for(i in 1:s){
#         grp[i] = grp[i] + sum(strata[H] == i) + sum(strata[E] == i)
#       }
#     }else{
#       for(i in 1:s){
#         grp[i] = grp[i] + sum(strata[H] == i)
#       }
#       break
#     }
#   }
#   obj = obj + sum(p_w[H]*w[H]) + r*(p - sum(w[H]))
#   ss = E[(cumsum(w[E])>p-sum(w[H]))][1]
#   sol = coeflist
#   for(s in 1:length(coeflist)){
#     a = c(1,rep(0, ncol(coeflist[[s]])-1))
#     if(s != strata[ss]){
#       a[coeflist1[[s]][1,grp[s]]+1] = 1
#       if(sum(a==1)>1){
#         a[1] = 0
#       }
#     }else{
#       index1 = coeflist1[[s]][1,grp[s]]+1
#       index2 = coeflist1[[s]][1,grp[s]+1]+1
#       a[index2] = (p - sum(w[H]))/(index2-index1)
#       a[index1] = 1 - a[index2]
#     }
#     sol[[s]] = a
#   }
#   return(list(obj = sm - as.numeric(obj), sol = sol, alpha = r))
#   
# }
# 
# ## middle step function
# a_n_p = function(a, p){
#   cumsum_a1 = cumsum(a[1,])
#   cumsum_a2 = cumsum(a[1,]*a[2,])
#   pos = sum(cumsum_a1<p)
#   
#   if(pos==0){
#     res = p *a[2,1]
#   }else if(pos == length(cumsum_a1)){
#     res = cumsum_a2[pos]
#   }else{
#     res = cumsum_a2[pos] + (p - cumsum_a1[pos])*a[2,pos+1]
#   }
#   return(as.numeric(res))
# }
# 
# ## O(nlogn) method of Greedy algorithm
# LpGreedy = function(coeflist, p, trans = TRUE, a = NULL, dic = NULL){
#   if(p<0){
#     if(trans){
#       return(Inf)
#     }else{
#       return(-Inf)
#     }
#     
#   }
#   coeflist1 = coeflist
#   if(trans){
#     ## convert to a maximization problem  
#     sm = 0
#     for(s in 1:length(coeflist)){
#       sm = sm + coeflist[[s]][2,1]
#       coeflist1[[s]][2,] = coeflist[[s]][2,1] - coeflist[[s]][2,]
#     }
#     ## remove Lp dominated terms to reduce complexity
#     lpres = LpDominateRemove(coeflist1)
#     coeflist1 = lpres$coeflist
#     dictionary = lpres$dictionary
#   }else{
#     if(is.null(dic)){
#       dictionary = coeflist
#       for(s in 1:length(coeflist)){
#         dictionary[[s]] = 1:length(coeflist[[s]])
#       }
#     }else{
#       dictionary = dic
#     }
#     
#   }
#   if(is.null(a)){
#     a = find_a(coeflist1)
#   }
#   cumsum_a1 = cumsum(a[1,])
#   cumsum_a2 = cumsum(a[1,]*a[2,])
#   pos = sum(cumsum_a1<p)
#   ## calculate objective value and solution
#   sol = list()
#   for(s in 1:length(coeflist)){
#     sol[[s]] = c(rep(0, ncol(coeflist[[s]])))
#   }
#   if(pos==0){
#     res = p *a[2,1]
#     pos3 = 0
#   }else if(pos == length(cumsum_a1)){
#     res = cumsum_a2[pos]
#     for(s in 1:length(coeflist)){
#       sol[[s]][ncol(coeflist[[s]])] = 1
#     }
#     if(trans){
#       return(list(obj = as.numeric(sm - res), sol = sol, alpha = 0))
#     }else{
#       return(list(obj = as.numeric(res), sol = sol, alpha = 0))
#     }
#     
#   }else{
#     res = cumsum_a2[pos] + (p - cumsum_a1[pos])*a[2,pos+1]
#     pos3 = a[3,1:pos]
#   }
#   
#   # for(s in 1:length(coeflist1)){
#   #   count_s = sum(pos3 == s) + 1
#   #   if(s!=a[3,pos+1]){  
#   #     index = coeflist1[[s]][1, count_s] + 1
#   #     sol[[s]][index] = 1
#   #   }else{
#   #     index1 = coeflist1[[s]][1,count_s]+1
#   #     index2 = coeflist1[[s]][1,count_s+1]+1
#   #     sol[[s]][index2] = (p - ifelse(pos==0, 0, cumsum_a1[pos]))/a[1,pos+1]
#   #     sol[[s]][index1] = 1 - sol[[s]][index2]
#   #   }
#   for(s in 1:length(coeflist1)){
#     count_s = sum(pos3 == s) + 1
#     if(s!=a[3,pos+1]){  
#       sol[[s]][dictionary[[s]][count_s]] = 1
#     }else{
#       sol[[s]][dictionary[[s]][count_s+1]] = (p - ifelse(pos==0, 0, cumsum_a1[pos]))/a[1,pos+1]
#       sol[[s]][dictionary[[s]][count_s]] = 1 - sol[[s]][dictionary[[s]][count_s+1]]
#     }
#   }
#   if(trans){
#     return(list(obj = sm - as.numeric(res), sol = sol, alpha = a[2,pos+1]))
#   }else{
#     return(list(obj = as.numeric(res), sol = sol, alpha = a[2,pos+1]))
#   }
# }
# 
# ### Dynamic Programming Algorithm
# IpDominateRemove = function(coeflist){
#   for(s in 1:length(coeflist)){
#     c = coeflist[[s]]
#     c = c[,c(1,(2:ncol(c))[diff(c[2,])!=0]),drop=FALSE]
#     coeflist[[s]] = c
#   }
#   return(coeflist)
# }
# DynamicProgram = function(coeflist, p){
#   sm = 0
#   for(s in 1:length(coeflist)){
#     sm = sm + coeflist[[s]][2,1]
#     coeflist[[s]][2,] = coeflist[[s]][2,1] - coeflist[[s]][2,]
#   }
#   if(p<=0){
#     return(sm)
#   }
#   coeflist1 = IpDominateRemove(coeflist)
#   N = length(coeflist)
#   R = matrix(nrow = N + 1, ncol = p + 1)
#   R[, 1] = 0
#   R[N+1,] = 0
#   solpos = R[1:N,1:p,drop=FALSE]
#   for(s in N:1){
#     for(j in 1:p){
#       c = coeflist1[[s]]
#       R[s,j+1] = max(R[s+1, unlist(lapply(j-c[1,], max, 0))+1] + c[2,]*(j-c[1,]>=0))
#       Rwhichmax = which(R[s+1, unlist(lapply(j-c[1,], max, 0))+1] + c[2,]*(j-c[1,]>=0)== R[s,j+1])
#       solpos[s,j] = Rwhichmax[length(Rwhichmax)]
#     }
#   }
#   sol = list()
#   ptr = p
#   for(s in 1:N){
#     a = rep(0, ncol(coeflist[[s]]))
#     if(ptr>0){
#       a[solpos[s, ptr]] = 1
#       ptr = ptr - coeflist1[[s]][1, solpos[s, ptr]]
#     }else{
#       a[1] = 1
#     }
#     sol[[s]] = a
#   }
#   return(list(obj = sm - R[1, p+1], sol = sol, objall = sm - R[1, ]))
# }
# 
# 
# 
# ### branch and bound algorithm
# 
# ## objcal: given solution, calculate objective value
# objcal = function(coeflist, x){
#   obj = 0
#   for(s in 1:length(coeflist)){
#     obj = obj + sum(x[[s]]*coeflist[[s]][2,])
#   }
#   return(obj)
# }
# ## check whether solution contains non-integer
# check_integer = function(x){
#   flag= 0
#   for(s in 1:length(x)){
#     flag = flag + sum(x[[s]]%%1 != 0)
#   }
#   return(flag == 0)
# }
# 
# 
# solveRecBB = function(coeflist1, coeflist2, x.1, s, p, z, a){
#   w1 = rep(0, length(coeflist2))
#   p1 = w1
#   if(s>1){
#     for(i in 1:(s-1)){
#       w1[i] = coeflist2[[i]][1, x.1[i]]
#       p1[i] = coeflist2[[i]][2, x.1[i]]
#     } 
#   }
#   if(sum(w1)>p){
#     return(-Inf)
#   }
#   if(sum(p1)>z){
#     z = sum(p1)
#     #x_star = x.1
#   }
#   if(s > length(coeflist2)){
#     return(sum(p1))
#   }
#   
#   a1 = a[,a[3,]>=s, drop = FALSE]
#   a1[3,] = a1[3,] - (s-1)
#   N = length(coeflist2)
#   for(i in 1:ncol(coeflist2[[s]])){
#     Lpres = LpGreedy(coeflist1[s:N], p - sum(w1), trans = FALSE, a = a1)
#     if(check_integer(Lpres$sol)){
#       return(Lpres$obj + sum(p1))
#     }
#     u_x = Lpres$obj + sum(p1)
#     if(u_x > z){
#       x.1[s] = i
#       obj.cand = solveRecBB(coeflist1, coeflist2, x.1, s+1, p, z, a)
#       z = max(z, obj.cand)
#     }
#     
#   }
#   return(z)
# }
# 
# 
# ## Main algorithm of BB: DyerKayalWalker
# BB_DKW= function(coeflist, p){
#   coeflist1 = coeflist
#   ## convert to a maximization problem  
#   sm = 0
#   for(s in 1:length(coeflist)){
#     sm = sm + coeflist[[s]][2,1]
#     coeflist1[[s]][2,] = coeflist[[s]][2,1] - coeflist[[s]][2,]
#   }
#   ## remove ILP dominated terms to reduce complexity
#   coeflist2 = IpDominateRemove(coeflist1)
#   
#   ## remove LP dominated terms to reduce complexity
#   coeflist1 = LpDominateRemove(coeflist1)$coeflist
#   
#   ## calculate the LP solution package for class reduction
#   a = find_a(coeflist1)
#   Lpres = LpGreedy(coeflist = coeflist1, p, trans = FALSE, a = a)
#   Lpsol = Lpres$sol
#   alpha = Lpres$alpha
#   UB = Lpres$obj
#   
#   ## calculate a lower bound for MCKP and corresponding w
#   LB_p = rep(0, length(coeflist1))
#   LB_w = LB_p
#   ind = LB_p
#   for(i in 1:length(coeflist1)){
#     ind[i] = which(Lpsol[[i]]>0)[1]
#     LB_w[i] = coeflist1[[i]][1, ind[i]]
#     LB_p[i] = coeflist1[[i]][2, ind[i]]
#   }
#   LB = sum(LB_p)
#   ## perform class reduction process
#   for(i in 1:length(coeflist2)){
#     a_i = a[, a[3,]!= i]
#     rem_i = c()
#     for(j in 1:ncol(coeflist2[[i]])){
#       if(j != ind[i]){
#         z = LB - LB_p[i] + coeflist2[[i]][2,j] + a_n_p(a_i, p + LB_w[i] - coeflist2[[i]][1,j])
#         if(z <= LB){
#           rem_i = c(rem_i, j)
#         }
#       }
#     }
#     if(!is.null(rem_i)){
#       coeflist2[[i]] = coeflist2[[i]][, -rem_i]
#       Lpsol[[i]] = Lpsol[[i]][-rem_i]
#     }
#     
#   }
#   
#   ## perform branch and bound with remaining items.
#   obj = solveRecBB(coeflist1, coeflist2, x.1=rep(1, length(coeflist1)),s=1, p=p, z=LB, a=a)
#   # sol = coeflist
#   # for(i in 1:length(coeflist)){
#   #   sol[[i]] = rep(0, ncol(coeflist[[i]]))
#   #   sol[[i]][1:length(x.1[[i]])] = x.1[[i]]
#   # }
#   # return(list(obj =sm - obj, sol = sol))
#   return(sm - obj)
# }
# 
# ## Main algorithm of BB: Mcknap
# BB_Mcknap = function(coeflist, p){
#   
#   coeflist1 = coeflist
#   ## convert to a maximization problem  
#   sm = 0
#   for(s in 1:length(coeflist)){
#     sm = sm + coeflist[[s]][2,1]
#     coeflist1[[s]][2,] = coeflist[[s]][2,1] - coeflist[[s]][2,]
#   }
#   ## remove ILP dominated terms to reduce complexity
#   coeflist2 = IpDominateRemove(coeflist1)
#   
#   ## remove LP dominated terms to reduce complexity
#   coeflist1 = LpDominateRemove(coeflist1)$coeflist
#   
#   ## calculate the LP solution package for class reduction
#   a = find_a(coeflist1)
#   Lpres = LpGreedy(coeflist = coeflist1, p, trans = FALSE, a = a, dic = NULL)
#   Lpsol = Lpres$sol
#   alpha = Lpres$alpha
#   n = length(coeflist1)
#   Aplus = c()
#   Aminus = c()
#   s = 1
#   LB_p = rep(0, length(coeflist1))
#   LB_w = LB_p
#   ind = LB_p
#   for(i in 1:length(Lpsol)){
#     ind[i] = which(Lpsol[[i]]>0)[1]
#     LB_w[i] = coeflist1[[i]][1, ind[i]]
#     LB_p[i] = coeflist1[[i]][2, ind[i]]    
#     if(sum(Lpsol[[i]] > 0, na.rm = TRUE) == 1){
#       pos = which(Lpsol[[i]] == 1)
#       Aplus = c(Aplus, ifelse(pos==length(Lpsol[[i]]), 0, 
#                               (coeflist1[[i]][2, pos+1]-coeflist1[[i]][2, pos])/(coeflist1[[i]][1, pos+1]-coeflist1[[i]][1, pos])))
#       Aminus = c(Aminus, ifelse(pos ==1, Inf, 
#                                 (coeflist1[[i]][2, pos]-coeflist1[[i]][2, pos-1])/(coeflist1[[i]][1, pos]-coeflist1[[i]][1, pos-1]))) 
#     }else{
#       Aplus = c(Aplus, 0)
#       Aminus = c(Aminus, Inf)
#       s = i
#       a = Lpsol[[i]]
#       a[a>0] = c(1,0)
#       Lpsol[[i]] = a 
#     }
#   }
#   cw = p - sum(LB_w)
#   cls = 1:n
#   cplus = vector(length = n)
#   cminus = cplus
#   cplus[1 + n - rank(Aplus, ties.method = "first")] = cls
#   cminus[rank(Aminus, ties.method = "first")] = cls
#   Aplus = c(Aplus, 0)
#   Aminus = c(Aminus, inf_C)  
#   reduceclass = function(s){
#     if(!is.null(coeflist2[[s]])){
#       lst = matrix(nrow = 2, ncol = 0)
#       for(i in 1:ncol(coeflist2[[s]])){
#         U = Lpres$obj-LB_p[s]+coeflist2[[s]][2,i]+alpha*(cw+LB_w[s]-coeflist2[[s]][1,i])
#         if(U>=z_l){
#           lst = cbind(lst,coeflist2[[s]][,i])
#         }
#       }      
#     }
#     return(lst)
#   }
#   z_l = 0
#   a = 1
#   b = 1
#   lst = reduceclass(s)
#   C = s
#   Cs = s
#   #  sol = matrix(lst[1,] + 1, nrow = 1)
#   reduceset = function(lst){
#     ap = max(Aplus[-C])
#     am = min(Aminus[-C])
#     z = max(lst[2,lst[1,]<=p], z_l)
#     cal = p - sum(LB_w[-C]) - lst[1,]
#     U = lst[2,] + sum(LB_p[-C]) + ifelse(cal>=0, cal*ap, cal*am)
#     lst = lst[, (U>z)|((U==z)&(lst[1,]!=p)),drop=FALSE]
#     # if(ncol(l)==0){
#     #   sol = sol[,U>=z_l, drop=FALSE]
#     # }else{
#     #   sol = sol[,(U>z_l)|((U==z_l)&(lst[1,]!=p)),drop=FALSE]
#     # }
#     return(list(lst=lst,z=z))
#   }
#   add = function(L, R){
#     Lp = matrix(nrow = 2, ncol = 0)
#     for(i in 1:ncol(R)){
#       Lp = cbind(Lp, L + R[,i])
#     }
#     w = unique(Lp[1,])
#     lst = matrix(nrow = 2, ncol = 0)
#     for (i in 1:length(w)){
#       lw = (Lp[1,]==w[i])
#       lpw = Lp[2, lw]
#       m = max(lpw)
#       wm = ((1:length(lpw))[lpw == m])[1]
#       lst = cbind(lst, c(w[i], m))
#       # pos = ((1:length(lw))[lw])[wm]
#       # posl = (pos - 1)%%ncol(L) + 1
#       # posr = (pos - 1)%/%ncol(L) + 1
#       # sol1 = cbind(sol1,c(sol[, posl], R[1, posr]+1))
#     }
#     return(lst)
#   }
#   repeat{
#     temp = reduceset(lst)
#     lst = temp$lst
#     z_l = temp$z
#     if(sum(C) == sum(1:length(coeflist2))|ncol(lst) == 0){break}
#     i = cplus[a]
#     a = a + 1
#     if(!(i%in% C)){
#       C = c(C, i)
#       R = reduceclass(i)
#       if(ncol(R)>1){
#         lst = add(lst, R)
#       }
#     }
#     temp = reduceset(lst)
#     lst = temp$lst
#     z_l = temp$z
#     if(sum(C) == sum(1:length(coeflist2))|ncol(lst) == 0){break}
#     i = cminus[b]
#     b = b + 1
#     if(!(i%in% C)){
#       C = c(C, i)
#       R = reduceclass(i)
#       if(ncol(R)>1){
#         lst = add(lst, R)
#       }
#     }
#   }
#   # s = coeflist
#   # for(i in 1:length(coeflist)){
#   #   s[[i]] = c(1, rep(0, ncol(coeflist[[i]])-1))
#   # }
#   # j = 1
#   # for(i in Cs){
#   #   s[[i]][1] = 0
#   #   s[[i]][sol[j,1]] = 1
#   #   j = j + 1
#   # }
#   return(list(obj = sm - z_l))
# }
# 
# 
# BB_Mcknap_C = function(coeflist, p){
#   coeflist1 = coeflist
#   ## convert to a maximization problem  
#   sm = 0
#   for(s in 1:length(coeflist)){
#     sm = sm + coeflist[[s]][2,1]
#     coeflist1[[s]][2,] = coeflist[[s]][2,1] - coeflist[[s]][2,]
#   }
#   ## remove ILP dominated terms to reduce complexity
#   coeflist2 = IpDominateRemove(coeflist1)
#   
#   ## remove LP dominated terms to reduce complexity
#   coeflist1 = LpDominateRemove(coeflist1)$coeflist
#   
#   ## calculate the LP solution package for class reduction
#   a = find_a(coeflist1)
#   Lpres = LpGreedy(coeflist = coeflist1, p, trans = FALSE, a = a, dic = NULL)
#   Lpsol = Lpres$sol
#   alpha = Lpres$alpha
#   n = length(coeflist1)
#   Aplus = c()
#   Aminus = c()
#   s = 1
#   LB_p = rep(0, length(coeflist1))
#   LB_w = LB_p
#   ind = LB_p
#   for(i in 1:length(Lpsol)){
#     ind[i] = which(Lpsol[[i]]>0)[1]
#     LB_w[i] = coeflist1[[i]][1, ind[i]]
#     LB_p[i] = coeflist1[[i]][2, ind[i]]    
#     if(sum(Lpsol[[i]] > 0, na.rm = TRUE) == 1){
#       pos = which(Lpsol[[i]] == 1)
#       Aplus = c(Aplus, ifelse(pos==length(Lpsol[[i]]), 0, 
#                               (coeflist1[[i]][2, pos+1]-coeflist1[[i]][2, pos])/(coeflist1[[i]][1, pos+1]-coeflist1[[i]][1, pos])))
#       Aminus = c(Aminus, ifelse(pos ==1, Inf, 
#                                 (coeflist1[[i]][2, pos]-coeflist1[[i]][2, pos-1])/(coeflist1[[i]][1, pos]-coeflist1[[i]][1, pos-1]))) 
#     }else{
#       Aplus = c(Aplus, 0)
#       Aminus = c(Aminus, Inf)
#       s = i
#       a = Lpsol[[i]]
#       a[a>0] = c(1,0)
#       Lpsol[[i]] = a 
#     }
#   }
#   cw = p - sum(LB_w)
#   cls = 1:n
#   cplus = vector(length = n)
#   cminus = cplus
#   cplus[1 + n - rank(Aplus, ties.method = "first")] = cls
#   cminus[rank(Aminus, ties.method = "first")] = cls
#   Aplus = c(Aplus, 0)
#   Aminus = c(Aminus, Inf)  
#   z_l = 0
#   a = 1
#   b = 1
#   lst = RC_C(coeflist2[[s]],Lpres$obj-LB_p[s]+alpha*(cw+LB_w[s]),alpha,z_l)
#   Cs = vector(length = n)
#   Cs[s] = s
#   C = Cs[Cs]
#   t1 = 0
#   t2 = 0
#   t3 = 0
#   start = Sys.time()
#   repeat{
#     val = c(z_l, p, sum(LB_w[-C]),sum(LB_p[-C]))
#     start.time = Sys.time() 
#     temp = RS_C(lst,C,Aplus,Aminus,val)
#     end.time = Sys.time() 
#     t1 = t1 +difftime(end.time,start.time, units = "secs")
#     lst = temp$lst
#     z_l = temp$z
#     if(sum(C) == sum(1:length(coeflist2))|ncol(lst) == 0){break}
#     i = cplus[a]
#     a = a + 1
#     if(Cs[i] == 0){
#       Cs[i] = i
#       C = rev(Cs[Cs])
#       start.time = Sys.time() 
#       R = RC_C(coeflist2[[i]],Lpres$obj-LB_p[i]+alpha*(cw+LB_w[i]),alpha,z_l)
#       end.time = Sys.time() 
#       t2 = t2 +difftime(end.time,start.time, units = "secs")
#       
#       if(ncol(R)>1){
#         start.time = Sys.time() 
#         lst = add_C(lst, R, p)
#         end.time = Sys.time() 
#         t3 = t3 +difftime(end.time,start.time, units = "secs")
#       }
#     }
#     val = c(z_l, p, sum(LB_w[-C]),sum(LB_p[-C]))
#     start.time = Sys.time() 
#     temp = RS_C(lst,C,Aplus,Aminus,val)
#     end.time = Sys.time() 
#     t1 = t1 +difftime(end.time,start.time, units = "secs")
#     lst = temp$lst
#     z_l = temp$z
#     if(sum(C) == sum(1:length(coeflist2))|ncol(lst) == 0){break}
#     i = cminus[b]
#     b = b + 1
#     if(Cs[i] == 0){
#       Cs[i] = i
#       C = rev(Cs[Cs])
#       start.time = Sys.time() 
#       R = RC_C(coeflist2[[i]],Lpres$obj-LB_p[i]+alpha*(cw+LB_w[i]),alpha,z_l)
#       end.time = Sys.time() 
#       t2 = t2 +difftime(end.time,start.time, units = "secs")
#       
#       if(ncol(R)>1){
#         start.time = Sys.time() 
#         lst = add_C(lst, R, p)
#         end.time = Sys.time() 
#         t3 = t3 +difftime(end.time,start.time, units = "secs")
#       }
#     }
#   }
#   end = Sys.time() 
#   t4 = difftime(end,start, units = "secs")
#   return(list(obj = sm - z_l))
# }


Gurobi_sol = function(coeflist, p, exact = TRUE){
  model = list()
  Q = NULL
  B = length(coeflist)
  nb = vector(length = B)
  for(i in 1:B){
    Q = c(Q, coeflist[[i]][2,])
    nb[i] = ncol(coeflist[[i]])
  }
  n = length(Q)
  A = matrix(0, nrow = B+1, ncol = n)
  indx = c(0, cumsum(nb))
  for(i in 1:B){
    A[i,(indx[i]+1):indx[i+1]] = 1
    A[B+1,(indx[i]+1):indx[i+1]] = coeflist[[i]][1,]
  }
  model$A = A
  model$obj = Q
  model$modelsense = "min"
  model$rhs = c(rep(1, B), p)
  model$sense = c(rep("=", B), "<=")
  if(exact){
    model$vtype = "B"
  }
  params <- list(OutputFlag=0)
  result = gurobi::gurobi(model, params)
  return(list(sol = result$x, obj = result$objval))
}
Gurobi_sol_PL = function(coeflist, p, exact = TRUE){
  model = list()
  B = length(coeflist)
  model$A = matrix(rep(1, B), nrow =  1)
  model$obj = rep(0, B)
  model$rhs = p
  model$sense = "<="
  model$pwlobj = list()
  for(i in 1:B){
    pwl = list()
    pwl$var = i
    pwl$x = coeflist[[i]][1,]
    pwl$y = coeflist[[i]][2,]
    model$pwlobj[[i]] = pwl
  }
  if(exact){
    model$vtype = "I"
  }
  
  model$modelsense = "min"
  params <- list(OutputFlag=0)
  result = gurobi::gurobi(model, params)
  return(list(sol = result$x, obj = result$objval))
}


library(lpSolveAPI)
Lpsolve_sol <- function(coeflist, k, exact = TRUE){
  B = length(coeflist)
  A = NULL
  for(i in 1:B){
    A = c(A, coeflist[[i]][2,])
  }
  
  flag = 1
  con.sum = NULL
  
  lprec = make.lp(0, ncol = length(A)) 
  set.objfn(lprec, A)
  
  if(exact){
    set.type(lprec, columns = 1:length(A), type = 'binary')
  }else{
    set.type(lprec, columns = 1:length(A), type = 'real')
    set.bounds(lprec, lower = rep(0,length(A)), upper = rep(1,length(A)), columns = 1:length(A))
    
  }
  for(i in 1:B){
    conb = rep(0, length(A))
    conb[flag:(flag+ncol(coeflist[[i]])-1)] = 1
    con.sum = c(con.sum, coeflist[[i]][1,] )
    flag = flag+ncol(coeflist[[i]])
    add.constraint(lprec, conb, type = '=', rhs = 1)
  }
  add.constraint(lprec, con.sum, type = '<=', rhs = k)
  lp.control(lprec, sense = 'min')
  solve(lprec)
  
  return(get.objective(lprec))
}