# analysis
# provide an uncertainty quantification of the estimated partial effect via bootstrap samples
# I draw bootstraps from raw survey data before cluster-level aggregation and use 50 samples to keep
# the computational burden manageable, then ggregate bootstrap sample to cluster-level disease counts

# distributional boosting model with beta binomial distribution and base formula
# risk resampling based on survey region stratified 10-fold cross validation the data

library(mboost)
library(gamboostLSS)
library(gamlss.dist)
library(data.table)
library(rsample)
library(dplyr)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "mali", "surveydata.rda"))
load(file = file.path("data", "processed", "mali", "geodata.rda"))
source(file = file.path("src", "analysis", "mali", "formula.R"))

cloc$log_pop = log(cloc$pop + 1)
grid$log_pop = log(grid$pop + 1)

rsp = bootstraps(data = sv, times = 50L, strata = cluster)
prds = data.table(h3_index = grid$h3_index, pop = grid$pop)

for (i in 1:nrow(rsp)) {
  resampled_data = analysis(rsp$splits[[i]])

  tmp_data = group_by(resampled_data, cluster, strata, region, urban, lon, lat)
  tmp_data = summarise(tmp_data, n = n(), npos = sum(mtest), nneg = n - npos, intercept = 1, .groups = "drop")
  tmp_data = merge(tmp_data, cloc)
  pos = matrix(data = c(tmp_data$npos, tmp_data$nneg), ncol = 2L)

  mod = gamboostLSS(
    formula = frml_base
    , data = tmp_data
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 3000L, nu = 0.1, trace = TRUE)
  )

  cv_risk = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(model.weights(mod), type = "kfold", B = 10L, strata = tmp_data$region)
  )

  mod[mstop(cv_risk)]

  tmp_prds = predict(mod, newdata = grid, type = "response")
  prds = cbind(prds, tmp_prds$mu)

  fname = paste(stringi::stri_rand_strings(n = 1, length = 6, pattern = "[a-z0-9]"), ".rda", sep = "")
  save(mod, file = file.path("models", "mali", "main", fname))
}

save(prds, file = file.path("models", "mali", "prds.rda"))
save(rsp, file = file.path("models", "mali", "rsp.rda"))
