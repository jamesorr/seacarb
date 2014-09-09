# Convert xCO2 (ppm) to pCO2 (uatm)
# James Orr, LSCE/IPSL, CEA-CNRS-UVSQ, France, 8 Sep 2014

x2pCO2 <- function(T=25, S=35, Patm=1.0, xCO2){
  source("vapress.R")
  pH20 <- vapress(T=T, S=S, form="d2007")
  pCO2 <- (Patm - pH20) * xCO2

return(pCO2)
}
