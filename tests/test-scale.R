context("scale")

test_that("Parameters: Center, Scale", {
  scaled <- do.call (scale, list(flu))
  expect_that(base::scale(flu@data$spc), equals (scaled@data$spc))
  expect_that(base::scale(flu@data$spc), equals (scaled@data$spc))
  
  mean_centered <- do.call (scale, list(flu, scale = FALSE))
  expect_that(base::scale(flu@data$spc, scale = FALSE), equals (mean_centered@data$spc))
  
  tmp <- sweep (flu, 1, mean, `/`)
  custom_center <- do.call(scale, list(tmp, center = quantile (tmp, .05), scale = FALSE))
  expect_that(base::scale(tmp@data$spc, center = as.matrix(quantile (tmp, .05)), scale = FALSE), equals (custom_center@data$spc))
})