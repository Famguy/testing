context("spc.rubberband")

test_that("Parameters: ", {
  p <- paracetamol [,, 175 ~ 1800]
  pv <- as.vector(p@data$spc)
  
  x <- p@wavelength
  y <- p@data$spc
  
  # Lower rubberband baseline
  disp <- par(mfrow=c(1, 2))
  bll <- spc.rubberband(p, noise = 300)
  plot(p)
  plot(bll, add = TRUE, col=3)
  # check withrespect to lower limit of data
  abline(h=min(pv),col = 2)
  # Check corrected data
  plot(p-bll)
  par(disp)
  
})