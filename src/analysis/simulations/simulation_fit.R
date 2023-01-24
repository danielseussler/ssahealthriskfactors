# simulation study
#
#
#

library(data.table)
library(mboost)
library(surveyCV)
library(Metrics)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))
source(file = file.path("src", "utils", "func_purrr_progress.R"))


# holdout folds: 1/3
# ideally one should not drop one strata from the analysis, but to conduct the
# resampling at least three clusters within a strata are required

ITER = 50L # number of simulation interations

sv = subset(sv, strata != 2L) |> droplevels()
sv = transform(sv, household = paste(cluster, household, sep = "_"))

holdout.A = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))
holdout.B = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = NULL))


# define simulation, estimate holdout risk and cv risk for sampled boosting iteration and fixed
# resampling technique, evaluate cv at iteration only for comp. efficiency

sv$moderatelyx = with(sv, ifelse(haz < -200, 1L, 0L))
sv$moderatelyf = factor(sv$moderatelyx, levels = c(0L, 1L), labels = c("no", "yes"))

sim = function(.type, .iter, .holdout) {

  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)

  trainingData = sv[which(initialSplit[, .iter] != 3L), ]
  testData = sv[which(initialSplit[, .iter] == 3L), ]

  mod = gamboost(
    formula = update(frml.1, moderatelyf ~ .)
    , data = trainingData
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 3000L, nu = 0.25, trace = FALSE)
  )

  cvFolds = switch(
    .type
    , "A" = cv(weights = model.weights(mod), type = "kfold", B = 5L, strata = NULL)
    , "B" = cv(weights = model.weights(mod), type = "kfold", B = 5L, strata = trainingData$strata)
    , "C" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = NULL)
    , "D" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = trainingData$strata)
    , "E" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL)
    , "F" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata)
    , "G" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL)
    , "H" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata)
    , "I" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL)
    , "J" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = trainingData$strata)
    , "K" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL, prob = 0.8)
    , "L" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = trainingData$strata, prob = 0.8)
    , "M" = 1L * as.matrix(1L == replicate(25, folds.svy(trainingData, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
  )

  # extract  estimate and true risk over holdout set
  grid = seq(from = 25L, to = mstop(mod), by = 25L)
  cv_risk = cvrisk(object = mod, grid = grid, folds = cvFolds)

  if (.type == "G" | .type == "H") {
    # .632 rule for bootstrap
    training_risk = risk(mod)[grid]
    cv_risk = unname(colMeans(cv_risk))
    cv_risk = 0.632 * cv_risk + 0.368 * (training_risk / nrow(trainingData))

    stopping_time = grid[which.min(cv_risk)]
  } else {
    stopping_time = mstop(cv_risk)
  }

  dt = data.table(
    holdout = .holdout
    , type = .type
    , iter = .iter
    , mstop = stopping_time
    , ncoef = length(names(coef(mod[stopping_time])))
    , strata = testData$strata
    , actual = testData$moderatelyx
    , pred = c(predict(mod[stopping_time], newdata = testData, type = "response"))
  )

  return(dt)
}

bench = data.frame(
  .type = rep(c("B", "D", "F", "J", "L", "M"), each = ITER)
  , .iter = rep(1:ITER, 6L)
)

res_a = pmap_with_progress(bench, ~ sim(..1, ..2, "A")) |> rbindlist()
res_b = pmap_with_progress(bench, ~ sim(..1, ..2, "B")) |> rbindlist()

res_moderately = rbindlist(list(res_a, res_b))
save(res_moderately, file = file.path("models", "88qlclkf.rda"))


# repeat the same for severely stunted children
sv$severelyx = with(sv, ifelse(haz < -300, 1L, 0L))
sv$severelyf = factor(sv$severelyx, levels = c(0L, 1L), labels = c("no", "yes"))

sim = function(.type, .iter, .holdout) {

  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)

  trainingData = sv[which(initialSplit[, .iter] != 3L), ]
  testData = sv[which(initialSplit[, .iter] == 3L), ]

  mod = gamboost(
    formula = update(frml.1, severelyf ~ .)
    , data = trainingData
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 3000L, nu = 0.25, trace = FALSE)
  )

  cvFolds = switch(
    .type
    , "A" = cv(weights = model.weights(mod), type = "kfold", B = 5L, strata = NULL)
    , "B" = cv(weights = model.weights(mod), type = "kfold", B = 5L, strata = trainingData$strata)
    , "C" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = NULL)
    , "D" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = trainingData$strata)
    , "E" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL)
    , "F" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata)
    , "G" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL)
    , "H" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata)
    , "I" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL)
    , "J" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = trainingData$strata)
    , "K" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL, prob = 0.8)
    , "L" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = trainingData$strata, prob = 0.8)
    , "M" = 1L * as.matrix(1L == replicate(25, folds.svy(trainingData, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
  )

  # extract  estimate and true risk over holdout set
  grid = seq(from = 25L, to = mstop(mod), by = 25L)
  cv_risk = cvrisk(object = mod, grid = grid, folds = cvFolds)

  if (.type == "G" | .type == "H") {
    # .632 rule for bootstrap
    training_risk = risk(mod)[grid]
    cv_risk = unname(colMeans(cv_risk))
    cv_risk = 0.632 * cv_risk + 0.368 * (training_risk / nrow(trainingData))

    stopping_time = grid[which.min(cv_risk)]
  } else {
    stopping_time = mstop(cv_risk)
  }

  dt = data.table(
    holdout = .holdout
    , type = .type
    , iter = .iter
    , mstop = stopping_time
    , ncoef = length(names(coef(mod[stopping_time])))
    , strata = testData$strata
    , actual = testData$severelyx
    , pred = c(predict(mod[stopping_time], newdata = testData, type = "response"))
  )

  return(dt)
}

bench = data.frame(
  .type = rep(c("B", "D", "F", "J", "L", "M"), each = ITER)
  , .iter = rep(1:ITER, 6L)
)

res_a = pmap_with_progress(bench, ~ sim(..1, ..2, "A")) |> rbindlist()
res_b = pmap_with_progress(bench, ~ sim(..1, ..2, "B")) |> rbindlist()

res_severely = rbindlist(list(res_a, res_b))
save(res_severely, file = file.path("models", "r7zhq0pi.rda"))
