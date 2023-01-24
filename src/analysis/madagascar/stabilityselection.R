# stability selection
# (not included in the discussion of results and of exploratory nature only)
#
#

library(stabs)
library(mboost)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))
source(file = file.path("src", "utils", "func_tidy_baselearner.R"))

mod = gamboost(
  formula = frml_base
  , data = surveydata
  , family = Binomial(type = "glm", link = "logit")
  , control = boost_control(mstop = 5000L, nu = 0.1)
)


# set the expected number of relevant terms as 10, per family error rate (expected) 2 and 4
# MB is for the original more conservative bound, SS uses complementary sampling and is additional
# assumptions on the distribution of 'noise' variables to derive bounds without the initial assumptions
# is thereby only controlling the number of *expected number of selected variables with low selection probability*

# as expected, region, age, cityaccess and gender are stable covariates

# check parameter configuration beforehand
stabsel_parameters(x = mod, q = 10L, PFER = 2L, sampling.type = "SS")
stabsel_parameters(x = mod, q = 10L, PFER = 4L, sampling.type = "MB")

stabsel_mb = stabsel(
  x = mod
  , q = 10L
  , PFER = 4L
  , grid = 0:5000
  , sampling.type = "MB"
  , folds = subsample(weights = model.weights(mod), B = 100L, strata = surveydata$strata)
)

stabsel_ss = stabsel(
  x = mod
  , q = 10L
  , PFER = 2L
  , grid = 0:5000
  , sampling.type = "SS"
  , assumption = "unimodal"
  , folds = subsample(weights = model.weights(mod), B = 50L , strata = surveydata$strata)
)

# create plot
pdf(
  file = file.path("results", "figures", "madagascar_stabilityselection.pdf")
  , width = 7.87402, height = 10.2362, pointsize = 8
)
par(mfrow = c(2, 1))

plot(
  x = stabsel_mb
  , type = "maxsel"
  , main = "Stability Selection\nMB original error bounds\nq = 10, PFER = 4"
  , labels = tidy_baselearner_names_str(names(stabsel_mb$max))
)

plot(
  x = stabsel_ss
  , type = "maxsel"
  , main = "Stability Selection\n SS complementary pair sampling\nq = 10, PFER* = 2, assumption = unimodal"
  , labels = tidy_baselearner_names_str(names(stabsel_mb$max))
)

dev.off()
