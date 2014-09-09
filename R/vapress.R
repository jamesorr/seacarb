# Compute vapor pressure of seawater (atm) following preocedure from either Weiss and Price (1980) or Dickson et al. (2007)
# The latter also computed the vapor pressure of pure water
# Both should give the same result when rounded to the 4th digit after the decimal
#
# James Orr
# LSCE/IPSL, CEA-CNRS-UVSQ, France
# 26 August 2014

vapress <- 
function(S=35, T=25, form="d2007"){

RES <- data.frame()

#-------Constants----------------

  tk = 273.15;           # [K] (for conversion [deg C] <-> [K])
  TK = T + tk;           # TK [K]; T[C]

  if (form=="wp1980") {
#   Formula from Weiss & Prince (1980, equation 10)
#   ln pH20 = 24.4543 - 67.4509 (100/T) - 4.8489 ln (T/100) - 0.000544 S

    vpsw <- exp(24.4543 - 67.4509*(100/TK) - 4.8489*log(TK/100) - 0.000544*S)
#   col <- c("VPseawater")
    RES <- data.frame(vpsw)
  } 
  else if(form=="d2007") {

#   Compute vapor pressure of water and seawater following recommendations in best-practices guide (Dickson et al., 2007)
#   Constants to compute water & seawater vapor pressure [kPa]: Dickson et al. (2007) Best Practices Guide
    cv1 =  -7.85951783 
    cv2 =   1.84408259
    cv3 = -11.7866497
    cv4 =  22.6807411
    cv5 = -15.9618719
    cv6 =   1.80122502 
    co0 =   0.90799   
    co1 =  -0.08992  
    co2 =   0.18458 
    co3 =  -0.07395
    co4 =  -0.00221

#   Vapor pressure of water
    ztc = 647.096
    zpc = 22.064 / 101325.0e-6     #Convert 22.064 MPa to atmospheres)
  
    zrt = 1 - TK/ztc
    zrt05 = sqrt(zrt)
    zrt15 = zrt*zrt05
    zrt2  = zrt*zrt
    zrt3  = zrt*zrt2
    zrt35 = zrt3*zrt05
    zrt4  = zrt2*zrt2
    zrt75 = zrt4*zrt35
#   vpwat  = exp( -LOG(ztc) + (ztc/TK)* (cv1*zrt + cv2*zrt15 + cv3*zrt3 + cv4*zrt35 + cv5*zrt4 + cv6*zrt75) )
    vpwat  = zpc * exp((ztc/TK)* (cv1*zrt + cv2*zrt15 + cv3*zrt3 + cv4*zrt35 + cv5*zrt4 + cv6*zrt75) )

#   Vapor pressure of seawater
    zsumb = 31.998 * S / ( 1.0E+03 - 1.005*S )
    zsmh  = zsumb * 0.5
    zsmh2 = zsmh*zsmh
    zsmh3 = zsmh*zsmh2
    zsmh4 = zsmh2*zsmh2
    zosmo = co0 + co1*zsmh + co2*zsmh2 + co3*zsmh3 + co4*zsmh4
#   vpsw_kpa    = vpwat * exp(-0.018 * zosmo * zsumb)      # Vapor pressure for seawater [kPa] - NO (already in atm)
#   vpsw =  vpsw_kpa / 101.325                             # Vapor pressure for seawater [atm] - unnecessary
    vpsw = vpwat * exp(-0.018 * zosmo * zsumb)             # Vapor pressure for seawater [atm]

#   col <- c("VPwater", "VPseawater")
#   RES <- data.frame(vpwat, vpsw)
    RES <- data.frame(vpsw)
  }

return(RES)
}


