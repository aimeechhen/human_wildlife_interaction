# Model

#............................................................
# Load packages ----
#............................................................

library(readr)
library(lubridate)       #date formats
library(zoo)             #date format 'year-month'
library(dplyr)           #data wrangling
library(tidyr)           #data wrangling
library(ggplot2)
library(khroma)          #colour blind friendly palette
library(viridis)
library(marginaleffects) #predictions()
library(gridExtra)       #arrange multi-plot
library(ggh4x)           #to fill in facet wrap title boxes
#model structure
library(mgcv)            #gam
library(MuMIn)           #AICc()
#remotes::install_github('burnett-m/climatenaR', build_vignettes = FALSE)
library(climatenaR)

#............................................................
# Model Distribution Specification ----
#............................................................

data$park <- as.factor(data$park)
data$year_month <- as.Date(data$year_month, format = "%Y-%m-%d")

## Poisson distribution ----
model <- gam(HWI ~
               s(park, bs = 'fs') +
               s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5) +        #random intercept & slope effect of park level trend
               s(avgtemp, park, bs = 'fs', k = 5) +                           #global effect of temperature, specify k explicitly (even if it is the default)
               s(log(avgprecip + 1e-10), park, bs = 'fs', k = 5) +            #global effect of precipitation, add a tiny number to avgprecip so we don't take log of 0
               ti(avgtemp, log(avgprecip + 1e-10), park, bs = c('tp', 'tp', 'fs'), k = 5) + #response to snow
               ti(month, avgtemp, k = 5, bs = c('cc', 'tp')) +                 #what is hot in january is cold in july
               ti(month, log(avgprecip + 1e-10), bs = c('cc', 'tp'), k = 5),
             family = poisson(link = "log"),                                   #indicate distribution family, poisson because count data
             data = data,
             method = "REML",
             control = gam.control(nthreads = 8, trace = TRUE),
             knots = list(month = c(0.5, 12.5)))
saveRDS(model, file = "RDS/model.RDS")
summary(model) #deviance 65.8%
par(mfrow = c(3,2))
plot(model)
par(mfrow = c(1,1))

## Negative binomial distribution ----
model2 <- gam(HWI ~
                s(park, bs = 're') +
                s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5) +        #random intercept & slope effect of park level trend
                s(avgtemp, park, bs = 'fs', k = 10) +                           #global effect of temperature, specify k explicitly (even if it is the default)
                s(log(avgprecip + 1e-10), park, bs = 'fs', k = 10) +            #global effect of precipitation, add a tiny number to avgprecip so we don't take log of 0
                ti(avgtemp, log(avgprecip + 1e-10), park, bs = c('tp', 'tp', 're'), k = 5) + #response to snow
                ti(month, avgtemp, k = 5, bs = c('cc', 'tp')) +                 #what is hot in january is cold in july
                ti(month, log(avgprecip + 1e-10), bs = c('cc', 'tp'), k = 5),
              family = nb(link = "log"),                                    #indicate distribution family, negative binomial because count data
              data = data,
              method = "REML",
              control = gam.control(nthreads = 8, trace = TRUE))
saveRDS(model2, file = "RDS/model2.RDS")
summary(model2) #deviance 62.2%
par(mfrow = c(3,2))
plot(model2)
par(mfrow = c(1,1))

#............................................................
## Model diagnostics ----
#............................................................

#comparing distributions
AICc(model, model2)
#Based on AICc model selection, a poisson distribution was a better fit

#diagnostics for a fitted gam model
par(mfrow = c(2,2))
gam.check(model)
gam.check(model2)
par(mfrow = c(1,1))

#dispersion check
#install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution

#check for for over/under dispersion
#assessing the goodness of fit of model
root_pois <- rootogram(model, style = "hanging", plot = FALSE)
root_nb   <- rootogram(model2, style = "hanging", plot = FALSE)
#plot rootogram
autoplot(root_pois)
autoplot(root_nb)
#calculate the Pearson estimate for the dispersion parameter using the Pearson residuals of each model
#values should be 1
sum(residuals(model, type = "pearson")^2) / df.residual(model) #above 1 = overdispersion
sum(residuals(model2, type = "pearson")^2) / df.residual(model2) #above 1 = overdispersion

#tests for temporal autocorrelation in the residuals
model_acf <- acf(residuals(model, type = "deviance"))
model2_acf <- acf(residuals(model2, type = "deviance"))