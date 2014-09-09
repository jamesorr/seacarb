# Convert pCO2 (uatm) to xCO2 (ppm) 
# James Orr, LSCE/IPSL, CEA-CNRS-UVSQ, France, 8 Sep 2014

p2xCO2 <- function(T=25, S=35, Patm=1.0, pCO2){
# Comment out or delete line just below, if both vapress.R is included in seacarb
  source("vapress.R")
  pH20 <- vapress(T=T, S=S, form="d2007")
  xCO2 <- pCO2 / (Patm - pH20) 

return(xCO2)
}
