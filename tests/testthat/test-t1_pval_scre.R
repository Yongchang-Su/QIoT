test_that("valid input for Z", {
  data("cadmium")
  Y = cadmium$cadmium
  block = as.factor(cadmium$mset)
  Z = cadmium$z
  method.list.all = list()
  method.list.all[[1]] = list(name = "Wilcoxon")
  expect_error(pval_quantile_scre(Z=0.5,Y=Y,block=block,k=floor(0.9*length(Y)),c=0, method.list.all=method.list.all,switch=TRUE), "Z should be a binary vector")
})
