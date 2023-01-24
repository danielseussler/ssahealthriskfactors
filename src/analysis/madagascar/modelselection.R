# analysis
# compare models discussed in the report with (approx) 70/20/10 train/validation/test split

# the simulation is run in loops because the mboost functions evaluate the arguments passed in the wrong
# environment, therefore unfortunately does not find the data frames in the
# fits the model, stops on validation set and returns the predictions on the test set

library(parallel)
library(mboost)
library(data.table)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))

n_rep = 50L
outer_folds = cv(weights = rep(1, nrow(surveydata)), type = "subsampling", B = n_rep, prob = 0.9, strata = surveydata$strata)

dim(outer_folds)

# save the hold-out predictions and save them
results = data.table()


# linear
for (i in 1:n_rep) {
  training_data = surveydata[outer_folds[, i] == 1, ]
  test_data = surveydata[outer_folds[, i] == 0, ]
  use_to_train = c(cv(weights = rep(1, nrow(training_data)), type = "subsampling", B = 1L, prob = 0.8, strata = training_data$strata))

  mod = glmboost(
    frml_linear
    , data = training_data
    , weights = use_to_train
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, risk = "oobag", stopintern = FALSE)
  )

  mstop = which.min(mod$risk())
  res = data.table(model = "Linear", iter = i, mstop = mstop, actual = test_data$moderatelyx,
                   pred = c(predict(object = mod[mstop], newdata = test_data, type = "response")))

  results = rbindlist(list(results, res))
}


# linear with interactions
for (i in 1:n_rep) {
  training_data = surveydata[outer_folds[, i] == 1, ]
  test_data = surveydata[outer_folds[, i] == 0, ]
  use_to_train = c(cv(weights = rep(1, nrow(training_data)), type = "subsampling", B = 1L, prob = 0.8, strata = training_data$strata))

  mod = glmboost(
    frml_linearplus
    , data = training_data
    , weights = use_to_train
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, risk = "oobag", stopintern = FALSE)
  )

  mstop = which.min(mod$risk())
  res = data.table(model = "Linear\n+ interactions", iter = i, mstop = mstop, actual = test_data$moderatelyx,
                   pred = c(predict(object = mod[mstop], newdata = test_data, type = "response")))

  results = rbindlist(list(results, res))
}


# additive base model
for (i in 1:n_rep) {
  training_data = surveydata[outer_folds[, i] == 1, ]
  test_data = surveydata[outer_folds[, i] == 0, ]
  use_to_train = c(cv(weights = rep(1, nrow(training_data)), type = "subsampling", B = 1L, prob = 0.8, strata = training_data$strata))

  mod = gamboost(
    formula = frml_base
    , data = training_data
    , weights = use_to_train
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, risk = "oobag", stopintern = FALSE)
  )

  mstop = which.min(mod$risk())
  res = data.table(model = "Base model", iter = i, mstop = mstop, actual = test_data$moderatelyx,
                   pred = c(predict(object = mod[mstop], newdata = test_data, type = "response")))

  results = rbindlist(list(results, res))
}


# additive base model + gender interactions
for (i in 1:n_rep) {
  training_data = surveydata[outer_folds[, i] == 1, ]
  test_data = surveydata[outer_folds[, i] == 0, ]
  use_to_train = c(cv(weights = rep(1, nrow(training_data)), type = "subsampling", B = 1L, prob = 0.8, strata = training_data$strata))

  mod = gamboost(
    formula = frml_basebycsex
    , data = training_data
    , weights = use_to_train
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, risk = "oobag", stopintern = FALSE)
  )

  mstop = which.min(mod$risk())
  res = data.table(model = "Base model\n + gender interaction", iter = i, mstop = mstop, actual = test_data$moderatelyx,
                   pred = c(predict(object = mod[mstop], newdata = test_data, type = "response")))

  results = rbindlist(list(results, res))
}


# additive base model + urbanicity interactions
for (i in 1:n_rep) {
  training_data = surveydata[outer_folds[, i] == 1, ]
  test_data = surveydata[outer_folds[, i] == 0, ]
  use_to_train = c(cv(weights = rep(1, nrow(training_data)), type = "subsampling", B = 1L, prob = 0.8, strata = training_data$strata))

  mod = gamboost(
    formula = frml_basebyurban
    , data = training_data
    , weights = use_to_train
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, risk = "oobag", stopintern = FALSE)
  )

  mstop = which.min(mod$risk())
  res = data.table(model = "Base model\n + urbanicity interaction", iter = i, mstop = mstop, actual = test_data$moderatelyx,
                   pred = c(predict(object = mod[mstop], newdata = test_data, type = "response")))

  results = rbindlist(list(results, res))
}


# boosted trees
for (i in 1:n_rep) {
  training_data = surveydata[outer_folds[, i] == 1, ]
  test_data = surveydata[outer_folds[, i] == 0, ]
  use_to_train = c(cv(weights = rep(1, nrow(training_data)), type = "subsampling", B = 1L, prob = 0.8, strata = training_data$strata))

  mod = blackboost(
    formula = frml_tree
    , data = training_data
    , weights = use_to_train
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 2000L, nu = 0.1, risk = "oobag", stopintern = FALSE)
    , tree_controls = partykit::ctree_control(maxdepth = 4L, saveinfo = FALSE)
  )

  mstop = which.min(mod$risk())
  res = data.table(model = "Boosted trees", iter = i, mstop = mstop, actual = test_data$moderatelyx,
                   pred = c(predict(object = mod[mstop], newdata = test_data, type = "response")))

  results = rbindlist(list(results, res))
}


save(results, file = file.path("models", "madagascar", "lvcw0a2q.rda"))
