library(ggplot2)
library(splines)
rm(list=ls())
setwd("~/Desktop/1A School/1A Winter 2021/STAT330/HW7")
snow_dat = read.csv("snow_core.csv")
snow_dat$Core = factor(snow_dat$Core)

#For problem 3
linear_mod = lm(Density ~ Depth + Core, data = snow_dat)
linear_mod_int = lm(Density ~ Depth * Core, data = snow_dat)
AIC(linear_mod)
AIC(linear_mod_int)

#For problem 4
poly_mod = lm(Density ~ poly(Depth, degree = 3) + Core, data = snow_dat)
poly_mod_int = lm(Density ~ poly(Depth, degree = 3) * Core, data = snow_dat)
AIC(poly_mod)
AIC(poly_mod_int)

#For Problem 5
knot_location = quantile(snow_dat$Depth,0.5)
knot_location = median(snow_dat$Depth)
spline_mod = lm(Density ~ bs(Depth, degree=3, knots = knot_location) + Core, data = snow_dat)
spline_mod_int = lm(Density ~ bs(Depth, degree=3, knots = knot_location) * Core, data = snow_dat)
AIC(spline_mod)
AIC(spline_mod_int)

# for problem 6
AIC(linear_mod)
AIC(linear_mod_int)
AIC(poly_mod)
AIC(poly_mod_int)
AIC(spline_mod)
AIC(spline_mod_int)

#plot for problem 8
coef(spline_mod_int)[1]
core1_fit = predict.lm(spline_mod_int)[snow_dat$Core == 1]
plot(x = snow_dat$Depth[snow_dat$Core == 1], y = snow_dat$Density[snow_dat$Core==1],
     pch = 19, cex = 0.6, xlab = "Depth (m)", cex.lab = 1.4, main = "Predicted vs Observed",
     ylab = "Density (g/cm^3)")
lines(snow_dat$Depth[snow_dat$Core == 1], core1_fit, col = "blue", lwd = 3)

