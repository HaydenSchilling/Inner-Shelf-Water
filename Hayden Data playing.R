mydata <- read.csv("Data/CleanedData_220519.csv", header = T)

table(is.na(mydata))

str(mydata)
table(mydata$site, mydata$Vbin)

# Y variables
# •	Zooplankton biomass # Biomass
# •	Zooplankton abundance # Abundance
# •	NBSS slope # NBSSSlope
# •	NBSS intercept #NBSSIntercept
# •	Mean ESD # GeoMn

cor.test(mydata$Biomass, mydata$NBSSIntercept)
### Biomass and Intercept are highly correlated (r = 0.75) - maybe don't need 2 separate models

# X variables
# •	Temperature - no na
# •	Chl_a
# •	Bathymetry - no na
# •	Sample depth bin (categorical), no na
# •	Transect site (categorical), no na
# •	Oxygen

# Biomass
fit1 <- lm(Biomass ~ Temp + chla + GA_depth2 + Depth + site, data = mydata)
plot(fit1) # assumptions not met

fit2 <- glm(Biomass ~ Temp + chla + GA_depth2 + Depth + site + oxygen, family =quasipoisson(link = "log"), data = mydata)
plot(fit2)
AIC(fit2)
summary(fit2)

library(glmmTMB)
library(DHARMa)
library(effects)
fit3 <- glmmTMB(Biomass ~ Temp + log10(chla) + GA_depth2 + Depth + site + oxygen, family =nbinom2(link = "log"), data = mydata)

simulationOutput <- simulateResiduals(fittedModel = fit3, n = 250)
plot(simulationOutput)                

hist(residuals(fit3))

summary(fit3)
plot(allEffects(fit3))

plot(mydata$Temp, log(mydata$Biomass))

## Abundance
fit4 <- glmmTMB(Abundance ~ Temp + log10(chla) + GA_depth2 + Depth + site + oxygen, family =nbinom2(link = "log"), data = mydata)

simulationOutput <- simulateResiduals(fittedModel = fit4, n = 250)
plot(simulationOutput)                

hist(residuals(fit4))

summary(fit4)
plot(allEffects(fit4))

## NBSS Slope
fit4 <- glmmTMB(NBSSSlope ~ Temp + log10(chla) + GA_depth2 + Depth + site + oxygen, family =gaussian, data = mydata)

simulationOutput <- simulateResiduals(fittedModel = fit4, n = 250)
plot(simulationOutput)                

hist(residuals(fit4))

summary(fit4)
plot(allEffects(fit4))

## NBSS intercept #NBSSIntercept
fit5 <- glmmTMB(NBSSIntercept ~ Temp + log10(chla) + GA_depth2 + Depth + site + oxygen, family =gaussian, data = mydata)

simulationOutput <- simulateResiduals(fittedModel = fit5, n = 250)
plot(simulationOutput)                

hist(residuals(fit5))

summary(fit5)
plot(allEffects(fit5))

# Mean ESD # GeoMn ## no real effects (or the model is completely wrong)

fit6 <- glmmTMB(GeoMn ~ Temp + log10(chla) + GA_depth2 + Depth + site + oxygen, family =nbinom1(link = "log"), data = mydata)

simulationOutput <- simulateResiduals(fittedModel = fit6, n = 250)
plot(simulationOutput)                

hist(residuals(fit6))

summary(fit6)
plot(allEffects(fit6))



#### Now without O2 and Chla (does all sites)
fit3 <- glmmTMB(Biomass ~ Temp + GA_depth2 + Depth + site, family =Gamma, data = mydata)

simulationOutput <- simulateResiduals(fittedModel = fit3, n = 250)
plot(simulationOutput)                

hist(residuals(fit3))

summary(fit3)
plot(allEffects(fit3))

plot(mydata$Temp, log(mydata$Biomass))
