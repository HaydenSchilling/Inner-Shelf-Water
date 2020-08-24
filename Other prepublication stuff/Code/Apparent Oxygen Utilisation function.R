### Apparent Oxygen Potential

### FROM MATLAB CODE

# % aou.m                                            by:  Edward T Peltzer, MBARI
# %                                                  revised:  2007 Apr 26.
# %
# % CALCULATE OXYGEN CONCENTRATION AT SATURATION
# %
# % Source:  The solubility of nitrogen, oxygen and argon in water and
# %         seawater - Weiss (1970) Deep Sea Research V17(4): 721-735.
# %
# % Molar volume of oxygen at STP obtained from NIST website on the
# %          thermophysical properties of fluid systems:
#   %
# %          http://webbook.nist.gov/chemistry/fluid/
#   %
# %
# % CALCULATE AOU BY DIFFERENCE:
#   %
# %         AOU (umol/kg) = sat O2 (umol/kg) - obs o2 (umol/kg).
# %
# %
# % Input:       S = Salinity (pss-78)
# %              T = Potential Temp (deg C)
# %              O2 = Meas'd Oxygen Conc (umol/kg)
# %
# % Output:      Apparant Oxygen Utilization (umol/kg).
# %
# %                        AOU = aou(S,T,O2).
# 
# function [AOU]=aou(S,T,O2)
# 
# 
# % DEFINE CONSTANTS, ETC FOR SATURATION CALCULATION
# 
# %    The constants -177.7888, etc., are used for units of ml O2/kg.
# 
#   T1 = (T + 273.15) ./ 100;
# 
#   OSAT = -177.7888 + 255.5907 ./ T1 + 146.4813 .* log(T1) - 22.2040 .* T1;
#   OSAT = OSAT + S .* (-0.037362 + T1 .* (0.016504 - 0.0020564 .* T1));
#   OSAT = exp(OSAT);
# 
# 
# % CONVERT FROM ML/KG TO UM/KG
# 
#   OSAT = OSAT * 1000 ./ 22.392;
# 
# 
# % CALCULATE AOU
# 
#   AOU = OSAT - O2;


## R Code - Hayden Schilling (SIMS/UNSW) 9/12/2019) adapted from above Matlab code

# requires Salinity (S; psu), Temperature (T; deg C), Oxygen (O2; umol / kg)
# I think my O2 is in the correct format - currently uM/L which should be equivalent to umol/kg

aou <- function(S, T, O2){ # does predicted minus observed
  T1 = (T + 273.15) / 100;
  OSAT = -177.7888 + 255.5907 / T1 + 146.4813 * log(T1) - 22.2040 * T1
  OSAT = OSAT + S * (-0.037362 + T1 * (0.016504 - 0.0020564 * T1))
  OSAT = exp(OSAT)
  OSAT = OSAT * 1000 / 22.392
  AOU = OSAT - O2
  return(AOU)
}

