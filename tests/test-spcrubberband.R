context("spc.rubberband")

test_that("PARACETAMOL", {
  p <- paracetamol [,, 175 ~ 1800]
  pv <- as.vector(p@data$spc)
  
  # Lower rubberband baseline
  disp <- par(mfrow=c(1, 2))
  bll <- do.call(spc.rubberband,list(p, noise = 300))
  plot(p)
  plot(bll, add = TRUE, col=3)
  # check withrespect to lower limit of data
  abline(h=min(pv),col = 2)
  # Check corrected data
  plot(p-bll)
  par(disp)
  
})