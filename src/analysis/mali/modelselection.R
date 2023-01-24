#
#
#
#

library(data.table)
library(gamboostLSS)
library(gamlss.dist)
library(rsample)
library(party)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "mali", "surveydata.rda"))
load(file = file.path("data", "processed", "mali", "geodata.rda"))
source(file = file.path("src", "analysis", "mali", "formula.R"))

cl = merge(cl, cloc)
cl$log_pop = log(cl$pop + 1.0)


# define outer folds on which models are evaluated
# the issue with repeated kfolds is that sometimes to few obs in small strata are observed
# therefore the analysis is limited to one repeat
rsp = vfold_cv(data = cl, v = 10L, repeats = 1L, strata = region)
res = data.table()


# the complete script could be implemented way better, but mboost/gamboostLSS seems to have a problem
# finding the boundary matrix in the correct environment, this is therefore implemented over loops

# main model
# distributional regression with beta-binomial distributed outcome
for (i in 1:nrow(rsp)) {
  trainingData = analysis(rsp$splits[[i]])
  testData = assessment(rsp$splits[[i]])
  pos = matrix(data = c(trainingData$npos, trainingData$nneg), ncol = 2L)

  mod = gamboostLSS(
    formula = frml_base
    , data = trainingData
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 2000L, nu = 0.1, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(model.weights(mod), type = "kfold", B = 10L, strata = trainingData$region)
  )

  pred = predict(object = mod[mstop(cv)], newdata = testData, type = "response")

  temp_result = data.table(model = "Base", id = i, mstop = mstop(cv), k = testData$npos, n = testData$n,
                           mu = c(pred$mu), sigma = c(pred$sigma))

  res = rbindlist(list(res, temp_result))
}

for (i in 1:nrow(rsp)) {
  trainingData = analysis(rsp$splits[[i]])
  testData = assessment(rsp$splits[[i]])
  pos = matrix(data = c(trainingData$npos, trainingData$nneg), ncol = 2L)

  mod = glmboostLSS(
    formula = frml_linear
    , data = trainingData
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 2000L, nu = 0.1, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(model.weights(mod), type = "kfold", B = 10L, strata = trainingData$region)
  )

  pred = predict(object = mod[mstop(cv)], newdata = testData, type = "response")
  temp_result = data.table(model = "Linear", id = i, mstop = mstop(cv), k = testData$npos,
                           n = testData$n, mu = c(pred$mu), sigma = c(pred$sigma))

  res = rbindlist(list(res, temp_result))
}


for (i in 1:nrow(rsp)) {
  trainingData = analysis(rsp$splits[[i]])
  testData = assessment(rsp$splits[[i]])
  pos = matrix(data = c(trainingData$npos, trainingData$nneg), ncol = 2L)

  mod = gamboostLSS(
    formula = frml_linearwspatial
    , data = trainingData
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 2000L, nu = 0.1, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(model.weights(mod), type = "kfold", B = 10L, strata = trainingData$region)
  )

  pred = predict(object = mod[mstop(cv)], newdata = testData, type = "response")
  temp_result = data.table(model = "Linear\n + spatial", id = i, mstop = mstop(cv), k = testData$npos,
                           n = testData$n, mu = c(pred$mu), sigma = c(pred$sigma))

  res = rbindlist(list(res, temp_result))
}


for (i in 1:nrow(rsp)) {
  trainingData = analysis(rsp$splits[[i]])
  testData = assessment(rsp$splits[[i]])
  pos = matrix(data = c(trainingData$npos, trainingData$nneg), ncol = 2L)

  mod = gamboostLSS(
    formula = frml_linearwspatialbyclimate
    , data = trainingData
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 2000L, nu = 0.1, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(model.weights(mod), type = "kfold", B = 10L, strata = trainingData$region)
  )

  pred = predict(object = mod[mstop(cv)], newdata = testData, type = "response")
  temp_result = data.table(model = "Linear by climate\n + spatial", id = i, mstop = mstop(cv), k = testData$npos,
                           n = testData$n, mu = c(pred$mu), sigma = c(pred$sigma))

  res = rbindlist(list(res, temp_result))
}

# binomial outcome distribution (i.e. no cluster overdispersion)
# this specification is expected to lack in coverage of the prediction intervals
for (i in 1:nrow(rsp)) {
  trainingData = analysis(rsp$splits[[i]])
  testData = assessment(rsp$splits[[i]])
  pos = matrix(data = c(trainingData$npos, trainingData$nneg), ncol = 2L)

  mod = gamboost(
    formula = frml_base$mu
    , data = trainingData
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 2000L, nu = 0.1, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(model.weights(mod), type = "kfold", B = 10L, strata = trainingData$region)
  )

  temp_result = data.table(model = "Base\n (Binomial)", id = i, mstop = mstop(cv), k = testData$npos,
                           n = testData$n, mu = c(predict(object = mod[mstop(cv)], newdata = testData, type = "response")), sigma = 0)

  res = rbindlist(list(res, temp_result))
}


# tree boosting to assess if higher order interactions are present in the data
# conditional inference trees with maximum depth of 4
for (i in 1:nrow(rsp)) {
  trainingData = analysis(rsp$splits[[i]])
  testData = assessment(rsp$splits[[i]])
  pos = matrix(data = c(trainingData$npos, trainingData$nneg), ncol = 2L)

  mod = blackboostLSS(
    formula = frml_linear
    , data = trainingData
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 1000L, nu = 0.1, trace = FALSE)
    , tree_controls = partykit::ctree_control(maxdepth = 4L, saveinfo = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(model.weights(mod), type = "kfold", B = 10L, strata = trainingData$region)
  )

  pred = predict(object = mod[mstop(cv)], newdata = testData, type = "response")
  temp_result = data.table(model = "Boosted trees", id = i, mstop = mstop(cv), k = testData$npos,
                           n = testData$n, mu = c(pred$mu), sigma = c(pred$sigma))

  res = rbindlist(list(res, temp_result))
}

for (i in 1:nrow(rsp)) {
  trainingData = analysis(rsp$splits[[i]])
  testData = assessment(rsp$splits[[i]])
  pos = matrix(data = c(trainingData$npos, trainingData$nneg), ncol = 2L)

  mod = gamboostLSS(
    formula = frml_spacevarying
    , data = trainingData
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 2000L, nu = 0.1, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(model.weights(mod), type = "kfold", B = 10L, strata = trainingData$region)
  )

  pred = predict(object = mod[mstop(cv)], newdata = testData, type = "response")

  temp_result = data.table(model = "Space-\nvarying", id = i, mstop = mstop(cv), k = testData$npos,
                           n = testData$n, mu = c(pred$mu), sigma = c(pred$sigma))

  res = rbindlist(list(res, temp_result))
}

save(res, file = file.path("models", "mali", "4r35hoz4.rda"))
