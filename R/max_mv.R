#' max_mv
#' @noRd
#' @keywords internal

max_mv = function(phi, gam){
  m = NULL
  v = NULL
  phi = sort(phi)
  for(i in 1:length(phi)){
    m = c(m, (gam*sum(phi)-(gam - 1)*sum(phi[1:i]))/(i+(length(phi)-i)*gam))
  }
  maxm = max(m)
  M = (1:length(phi))[m == maxm]
  phi2 = phi^2
  for(i in M){
    v = c(v, (gam*sum(phi2)-(gam - 1)*sum(phi2[1:i]))/(i+(length(phi)-i)*gam) - maxm^2)
  }
  maxv = max(v)
  return(list(u = maxm, v = maxv))
}