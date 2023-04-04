test_that("function works", {
  data("cadmium")
  Y = cadmium$cadmium
  block = as.factor(cadmium$mset)
  Z = cadmium$z
  method.list.all = list()
  method.list.all[[1]] = list(name = "Wilcoxon")
  expect_equal(QIoT::pval_quantile_scre(Z=Z,Y=Y,block=block,k=floor(0.9*length(Y)),c=0, method.list.all=method.list.all,switch=TRUE), list(upper = 0, lower = 0, fixed = 0))
})
