
### animal interaction full code with details ####

library("readr")
library("ggplot2")

## PRE-REGISTRATION ####
# exploring and visualization of the data

ggplot() +
  geom_point(aes(x = data$`Incident Date`, y = data$`Animal Behaviour`), alpha = 0.1) + theme_bw()

ggplot() +
  geom_point(aes(x = data$`Incident Date`, y = data$`Species Common Name`), alpha = 0.1) + theme_bw()

ggplot() +
  geom_point(aes(x = data$`Incident Date`, y = data$`Incident Type`), alpha = 0.1) + theme_bw()


##Noonan
#Some subsetting and carpentry
data2 <- data[which(data$`Incident Type` == "Human Wildlife Interaction"),]
names(data2)[2] <- "date"
names(data2)[6] <- "species"
names(data2)[4] <- "park"
names(data2)[5] <- "interaction"

test <- aggregate(interaction ~ date + park, data = data2, FUN = "length")
ggplot(test) +
  geom_point(aes(x = date, y = interaction, col = park), alpha = 0.2) + 
  theme_bw()
# file = preregtest_interaction_date-park.png, size = 2500x1200

test2 <- aggregate(interaction ~ date + species, data = data2, FUN = "length")
ggplot(test2) +
  geom_point(aes(x = date, y = interaction, col = species), alpha = 0.2) + theme_bw()
  # file = preregtest2_interaction_date-species.png, size = 2500x1200


###################################


#### WORKFLOW ####

#Load packages
library("readr")
library("ggplot2")
library("nlme") #non-linear mixed-effects models, nested effects
library("lme4") #linear mixed model, lmer(), glmer()
library("ClimateNAr") #' requires ClimateNA app to be installed on local computer for the `ClimateNAr` R package to work
  #' if necessary, install the `climatenaR` package with `remotes::install_github('burnett-m/climatenaR', build_vignettes = TRUE)`
  #See https://register.climatena.ca/ to download it


#set working directory
setwd("C:/Users/achhen/OneDrive - UBC/BIOL 520C Statistical Modelling/Github/animal-interaction")

#Import Parks Canada dataset
PCA <- read_csv("data/pca-human-wildlife-coexistence-animals-involved-detailed-records-2010-2021.csv")
  # Data obtained on Feb. 3, 2023
#data <- read_csv("https://open.canada.ca/data/en/datastore/dump/b2a9f7e4-7c49-471d-8337-0192c15dd52a?bom=True")
  # REFERENCE: https://open.canada.ca/data/en/dataset/cc5ea139-c628-46dc-ac55-a5b3351b7fdf/resource/b2a9f7e4-7c49-471d-8337-0192c15dd52a?inner_span=True

# Data subsetting and carpentry
coexistence <- PCA[which(PCA$`Incident Type` == "Human Wildlife Interaction"),] #extracting items of interest (conditional subsetting)
coexistence <- PCA[which(PCA$`Protected Heritage Area` %in% c("Banff National Park of Canada",
                                    "Elk Island National Park of Canada",
                                    "Glacier National Park of Canada",
                                    "Jasper National Park of Canada",
                                    "Kootenay National Park of Canada",
                                    "Mount Revelstoke National Park of Canada",
                                    "Pacific Rim National Park Reserve of Canada",
                                    "Waterton Lakes National Park of Canada",
                                    "Wood Buffalo National Park of Canada",
                                    "Yoho National Park of Canada")),] #extract parks of interest (BC, AB)
#do i need to set as factor??????
coexistence$`Incident Date` <- as.Date(coexistence$`Incident Date`)
coexistence$`Protected Heritage Area.f` <- as.factor(coexistence$`Protected Heritage Area`)
coexistence$`Species Common Name.f` <- as.factor(coexistence$`Species Common Name`)
# Renaming columns in the dataset to make it easier for coding purposes.
names(coexistence)[5] <- "interaction"
names(coexistence)[2] <- "date"
names(coexistence)[4] <- "park"
names(coexistence)[6] <- "species"
names(coexistence)[15] <- "park.f" #set as factor object
names(coexistence)[16] <- "species.f" #set as factor object


###EXPLORE THE DATA
unique(coexistence$interaction) #exploring the response variable
table(coexistence$interaction) #counts of each type of interaction
(table(coexistence$interaction) / nrow(data)) * 100 #percent of each type of interaction
filter(coexistence, interaction == 'Wildlife Sighting') #filter() requires dplyr package
sort(unique(coexistence$park))
(table(coexistence$park) / nrow(coexistence)) * 100 #percent of samples from each

## Main response variable: 
# Human Wildlife Interactions (Incident type)

## Explanatory variables:
# A: Incident date
# B: Protected Heritage Area
# C: Species common name
# D: Temperature (supplementry dataset) ClimateNA package (NOTE: grouped by monthly)
# E: Precipitation (supplementry dataset) ClimateNA package (grouped by monthly) 
#' *NOTE:* for temperature and preciptation, the data was grouped by monthly instead of daily because ClimateNA does not have daily data and to obtain daily data is troublesome and will be used for the projects outside of BIOL 520C

events <- data.frame(aggregate(interaction ~ date, data = coexistence, FUN = "length")) #interaction(s) grouped/organized by date
data <- data.frame(aggregate(interaction ~ date + park + species, data = coexistence, FUN = "length")) #interaction(s), species and park grouped/organized by dates
data.f <- data.frame(aggregate(interaction ~ date + park.f + species.f, data = coexistence, FUN = "length")) #interaction(s), species and park grouped/organized by dates
#date_park <- aggregate(interaction ~ date + park, data = data, FUN = "length") #dates grouped together based on parks 
#date_species <- aggregate(interaction ~ date + species, data = data, FUN = "length") #dates grouped together based on species

# VISUALIZATION OF DATA (P04) ####

# Before deciding what type of model to fit, and how to structure the parameters, it's important to visually explore the data to get a feel for what the structure is. This can help on how to approach the modelling process and guide the modelling process. (P04)

## Plot the predictors against the response (e1)
plot(data$interaction ~ data$date)
boxplot(data$interaction ~ data$park)
boxplot(data$interaction ~ data$species)
#temperature
#precipitation

## Visualize the sampling (e2)
# Next, understand how balanced/unbalanced the sampling was. Especially for nested data. If certain groups are over represented, there is a chance that they can outweigh others and vice versa for underrepresented groups.
#Note: use table() function to make it easier

barplot(table(coexistence$interaction), ylab = "Frequencies", main = "Interactions", las = 2)
barplot(table(coexistence$date), ylab = "Frequencies", main = "Date", las = 2)
barplot(table(coexistence$park), ylab = "Frequencies", main = "Park", las = 2)
barplot(table(coexistence$species), ylab = "Frequencies", main = "Species", las = 2)

#combined multiple counts on same date together
barplot(table(data$date), ylab = "Frequencies", main = "Date", las = 2) 
barplot(table(data$park), ylab = "Frequencies", main = "Park", las = 2)
barplot(table(data$species), ylab = "Frequencies", main = "Species", las = 2)

## Inspect the response variable in relation to the sampling (e3)
# If the data is nested/hierarchical and has some balancing issues, then it is useful to check to see if the response variable differs across the sampling groups to get a feel for how balancing might influence the modelling process. This can help tell us if we need to worry about fitting mixed-effect models to our data, and how partial shrinkage might come into play.
# If a heavily over sampled park also has higher number of counts than any other park, it can pull the regression towards itself.
unique(data$park)
boxplot(data$interaction ~ data$park, xlab = "park", ylab = "number of interactions", main = "park", col = rainbow(10))
  #sampling counts per park were not sampled equally
#plot mean number of interactions in each park vs the amount of data for each park
MEANS <- aggregate(interaction ~ park, data = data, FUN = "mean")
COUNTS <- aggregate(interaction ~ park, data = data, FUN = "length")
test <- lm(MEANS$interaction ~ COUNTS$park)
plot(MEANS[,2], xlab = "mean number of interactions in each park", ylab = "Frequency", col = rainbow(10))
abline(test, lty = 2)
legend("topright", fill = rainbow(10), legend = unique(data$park), horiz = F, cex = 0.6)

ggplot(data) +
  geom_point(aes(x = date, y = interaction, col = park)) +
  xlab("date") +
  ylab("number of interactions") +
  theme_bw()

ggplot(data, aes(x = date, y = interaction, col = park)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  xlab("date") +
  ylab("number of interactions") +
  theme_bw()
#it looks like random slope will be needed because each park has different slopes, therefore the model will allow for each group (park) to have a different slope from each others
#it looks like might need a random intercept for each park, it will allow the model to have different intercept for each group (park) because a random slope is required therefore having a random intercept will allow the slope to vary, if you do not have a random intercept but you have a random slope, the random effect of not having a random intercept will affect the slope themselves
#however you have a random intercept effect without a random slope effect, and very seldom vice versa
#' *therefore, I need a mixed model effect + i want to group by parks, with number of interactions vary by date????????* 


#### What kind of model, and What is my distribution?
# is it linear, linear mixed model, generalized linear model, generalized mixed model?

#### Practical 05
## LINEAR initial model fit and inspection
model_lm <- lm(interaction ~ date + park + species, data = data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm), xlab = "fitted values", ylab = "residuals", main = "lm model") #residuals look like shit

## Choose a random effect structure (P05e3)
model_lme <- lme(interaction ~ date + park + species, random = ~date | park, data = data) #random date intercept, random park slope
summary(model_lme)
plot(fitted(model_lme), residuals(model_lm), xlab = "fitted values", ylab = "residuals", main = "lme model")

model_lme2 <- lme(interaction ~ date + park + species, random = ~species | park, data = data)
summary(model_lme2)
plot(fitted(model_lme2), residuals(model_lm2), xlab = "fitted values", ylab = "residuals", main = "lme model2")

#### NEED TO DO AUTOCORRELATION, HETEROSCEDASTICITY BEFORE MODEL SELECTION ####


#### Generalized Linear Model (GLM) ####
#exponential family: class of nonnormal errors that generalized linear models can handle (Bolker p.309)
#each distribution has standard link function:
#log link = standard for Poisson
#logit link = standard for Binomial distribution
#' GLMs are fit by a `iteratively reweighted least squares` process, which overcomes the basic problem that transforming the data to make them linear also changes the variance
#Poisson regression used for count data
#Logistic regression used for survival/failure data

#modelling count data using GLMs (P08)
#Working with count data, what we're looking for is a discrete distribution with support between 0 and infinity. Given these requirements, the Poison distribution is a good candidate for modelling these data
#In order to model these data using a Poisson distribution to describe the model's stochastic component, we need to switch to a GLM workflow.
#To do this, we need to carry out 3 steps before fitting our GLM:
#1. make a distribution assumption on the response variable
#2. Specify the deterministic part of the model
#3. Specify explicitly the 'link' betweeen the deterministic part based on your distributional assumption.

#Poisson distribution: log link, Poisson error (Y ~ Poisson(ae^bx))
#(count data + time/space)
#glm1 <- glm(y ~ x, family = "poisson") (Bolker, 0.309)
model_glm <- glm(interaction ~ date + park.f + species.f, family = poisson(link = "log"), data = data)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm), xlab = "fitted values", ylab = "residuals", main = "glm model")

#equivalent likelihood function (Bolker, p.309)
# poisregfun = function(a,b) {
#   Y.pred = exp(a + b * x)
#   -sum(dpois(y, lambda = Y.pred, log = TRUE))
# }

#### FULL MODEL: Generalized linear model + random effects + Poisson distribution + log link function ####
#modelling count data using GLMs with random effects

#Standard GLMs framework can be extended to handle nested structures similar to mixed effect models did for Gaussian linear regression (L18s38)
#Refer to http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#model-definition
#There are a number of different link functions you can use when fitting GLMs
#Note: modifying the off-diagonals of the correlation matrix can correct for various form of autocorrelation in Gaussian linear regression
#HOWEVER, because we are working with different distributions now, those approaches do NOT translate cleanly :(

#random intercept of park
model_rint <- glmer(interaction ~ date + park + species, (1 | park), family = poisson(link = "log"), data = data)
#random intercept of group -> (1|group)
summary(model_rint)
plot(fitted(model_rint), residuals(model_rint), xlab = "fitted values", ylab = "residuals", main = "random intercept model")

#no variation in intercept of species but random slope of species within park
model_rslope <- glmer(interaction ~ date + park + species, (0 + date | park), family = poisson(link = "log"), data = data)
#random slope of x within group -> (0+x|group) or (-1+x|group)
summary(model2)
plot(fitted(model2), residuals(model2), xlab = "fitted values", ylab = "residuals", main = "random slope model")

#random intercept and random slope of date within park (because number of counts in a date can vary?????)
model_rint.slope <- glmer(interaction ~ date + (date|species) + (date | park), family = poisson(link = "log"), data = data)
#random slope of x within group -> (1+x|group) or (x|group)
summary(model_rint.slope)
plot(fitted(model_rint.slope), residuals(model_rint.slope), xlab = "fitted values", ylab = "residuals", main = "random intercept & random slope model")

acf(residuals(model_rint.slope))





#random intercept and random slope of date within park
model3 <- glmer(interaction ~ date + park + species, (1))

#random slope of species within park
model4 <- glm(interaction ~ date + park + species, (~species | park), family = "poisson", data = data)
summary(model3)
plot(fitted(model3), residuals(model3), xlab = "fitted values", ylab = "residuals", main = "model3")







MEANS <- aggregate()

test <- aggregate(interaction ~ date + park, data = data2, FUN = "length")



data2 <- data[which(data$`Incident Type` == "Human Wildlife Interaction"),]
names(data2)[2] <- "date"
names(data2)[6] <- "species"
names(data2)[4] <- "park"
names(data2)[5] <- "interaction"