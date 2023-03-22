#' null_dist_block
#' 
#' A function to calculate to calculate test statistics for null hypothesis: kth smallest treatment effect is less or equal to c.
#' @param Z A numeric vector of treatment assignment. 0 indicates control and 1 treated.
#' @param block A factor vector indicates to which blocks the units belong. 
#' @param method.list.all A list that contains the method of the rank scores. If you use the same methods across all strata, method.list.all will only include 1 element stating that score. Otherwise, method.list.all[[s]] should be the method for stratum s. Available score functions: Wilcoxon, list(name = "Wilcoxon); Stephenson, list(name = "Stephenson", s = ?).  
#' @param Z_perm Permutations of Z (treatment assignments) used to generate null distribution. If NULL, then it will be randomly generated.
#'

null_dist_block = function(Z, block, method_list_all, null_max = 1e5, Z_perm = NULL){
  if(!is.factor(block)){
    block = as.factor(block)
  } 
  levels(block) = 1:length(levels(block))
  return(null_dist_block_C(Z, block, method_list_all, null_max = 1e5, Z_perm))
}
