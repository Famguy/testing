context("scale")

test_that("CHONDRO", {
  scaled <- do.call (scale, list(chondro))
  expect_that(base::scale(chondro@data$spc), equals (scaled@data$spc))
  expect_that(base::scale(chondro@data$spc), equals (scaled@data$spc))
  
  mean_centered <- do.call (scale, list(chondro, scale = FALSE))
  expect_that(base::scale(chondro@data$spc, scale = FALSE), equals (mean_centered@data$spc))
  
  tmp <- sweep (chondro, 1, mean, `/`)
  custom_center <- do.call(scale, list(tmp, center = quantile (tmp, .05), scale = FALSE))
  expect_that(base::scale(tmp@data$spc, center = as.matrix(quantile (tmp, .05)), scale = FALSE), equals (custom_center@data$spc))
})