# analysis
# draw 50 replications of 80/20 splits and train the model on the first, select stopping iteration on latter
# the holdout risk (predicitve risk) is computed by mboost for observation with zero weights
# save the models to models/madagascar/main/

library(parallel)
library(mboost)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))

train_folds = cv(weights = rep(1, times = nrow(surveydata)), type = "subsampling", B = 50L, prob = 0.8, strata = surveydata$strata)
save(train_folds, file = file.path("models", "madagascar", "train_folds.rda"))

dim(train_folds)

for (i in 1:50) {
  mod = gamboost(
    formula = frml_base
    , data = surveydata
    , weights = train_folds[, i]
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 10000L, nu = 0.1, risk = "oobag", stopintern = FALSE)
  )
  mod[which.min(mod$risk())]

  fname = paste(stringi::stri_rand_strings(n = 1, length = 6, pattern = "[a-z0-9]"), ".rda", sep = "")
  save(mod, file = file.path("models", "madagascar", "main", fname))
}
