

library(gamlss)
library(gamlss.dist)
library(gamlss.add)


fitdistro <- fitDist(dat2$Severity, k = 2, type = "counts", 
               trace = T, try.gamlss = TRUE)

summary(fitdistro)
