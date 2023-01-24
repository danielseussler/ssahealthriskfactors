#
#
#
#

library(mboost)
library(gamboostLSS)
library(gamlss.dist)
library(data.table)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "mali", "surveydata.rda"))
load(file = file.path("data", "processed", "mali", "geodata.rda"))
source(file = file.path("src", "analysis", "mali", "formula.R"))

# data preparation, add log transformed population
# construct (number of pos cases, number of neg cases) outcome matrix
training_data = merge(cl, cloc)
training_data$log_pop = log(training_data$pop + 1)

grid$log_pop = log(grid$pop + 1)
pos = matrix(data = c(training_data$npos, training_data$nneg), ncol = 2L) # binomial outcome

# distributional boosting model with beta binomial distribution
# resampling based on survey stratified 10-fold cross validation the data
fitted = gamboostLSS(
  formula = frml_base
  , data = training_data
  , families = as.families("BB")
  , method = "noncyclic"
  , control = boost_control(mstop = 2000L, nu = 0.1, trace = TRUE)
)

# resampling due to limited data size with 10-fold cross-validation stratified by region
# due to the limited number of clusters stratification by strata is not feasible
cv_risk = cvrisk(
  object = fitted
  , grid = seq(from = 25L, to = mstop(fitted), by = 10L)
  , folds = cv(model.weights(fitted), type = "kfold", B = 10L, strata = cl$region)
)
plot(cv_risk)

# apply early stopping
fitted[mstop(cv_risk)]

# predictions on country level grid and population to create subnational estimates later
pred = predict(fitted, newdata = grid, type = "response")
pred = data.table(h3_index = grid$h3_index, mu = pred$mu, sigma = c(pred$sigma), pop = grid$pop)

# save results
fwrite(pred, file = file.path("results", "predictions", "mali_malaria_risk.csv"))
save(training_data, pos, fitted, cv_risk, pred, grid, file = file.path("models", "mali", "9dkw7wyn.rda"))
