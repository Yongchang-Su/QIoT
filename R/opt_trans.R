#' opt_trans
#' 
#' A function to get optimal monotone transformation
#' @param a Numeric vector that needs to be transformed.
#' @keywords Optimal monotone transformation

### transform the sequence to a monotone decreasing one with larger cumulative sums
opt_trans = function(a){
  n = length(a)
  if(n >= 2){
    b = rep(NA, n)
    b[1] = max( cumsum(a)/c(1:n) )
    for(k in 2:n){
      b[k] = max( ( cumsum(a) - sum(b[1:(k-1)]) )[-c(1:(k-1))]/c(1:(n-k+1)) )
    }
  }
  else{
    b = a
  }
  return(b) 
}
