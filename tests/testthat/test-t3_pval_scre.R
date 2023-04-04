test_that("valid input for opt.method", {
  data("cadmium")
  Y = cadmium$cadmium
  block = as.factor(cadmium$mset)
  Z = cadmium$z
  method.list.all = list()
  method.list.all[[1]] = list(name = "Wilcoxon")
  expect_error(pval_quantile_scre(Z=Z,Y=Y,block=block,k=floor(0.9*length(Y)),c=0, alternative = "two.sided",opt.method = "gd",method.list.all=method.list.all,switch=TRUE), "Invalid input for opt.method")
})
