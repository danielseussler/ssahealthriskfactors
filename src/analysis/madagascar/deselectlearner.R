# deselectBoost
# (not included in the discussion of results and of exploratory nature only)
#
#

library(mboost)
library(surveyCV)
library(data.table)
library(ggplot2)
library(viridis)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))
source(file = file.path("src", "utils", "func_tidy_baselearner.R"))
source(file = file.path("src", "utils", "DeselectBoost.R"))

mod = gamboost(
  formula = frml_base
  , data = surveydata
  , family = Binomial(type = "glm", link = "logit")
  , control = boost_control(mstop = 5000L, nu = 0.1)
)

cv_risk = cvrisk(
  object = mod
  , folds = cv(weights = model.weights(mod), type = "kfold", strata = surveydata$strata)
  , grid = seq(from = 10L, to = mstop(mod), by = 10L)
)

mod[mstop(cv_risk)] # apply early stopping


# deselects base learners that did not achieve the required threshold in risk reduction and refits model
# note: a warning thrown is because the function searches for the response var in the data frame,
# and converts to numerics which throws this error converting region char, no issue with the estimation

deselect_mod_attr = DeselectBoost(
  object = mod
  , data = surveydata
  , fam = Binomial(type = "glm", link = "logit")
  , tau = 0.01
  , method = "attributable"
)

deselect_mod_cum = DeselectBoost(
  object = mod
  , data = surveydata
  , fam = Binomial(type = "glm", link = "logit")
  , tau = 0.01
  , method = "cumulative"
)


# extract metrics on risk reduction
# check variable importance before using deselection approach
variable_importance = rbind(
  cbind(as.data.frame(varimp(mod)), type = "none")
  , cbind(as.data.frame(varimp(deselect_mod_attr)), type = "attributable")
  , cbind(as.data.frame(varimp(deselect_mod_cum)), type = "cumulative")
)

variable_importance$name = tidy_baselearner_names_str(char = variable_importance$blearner)
variable_importance$name = as.factor(variable_importance$name)
variable_importance$name = forcats::fct_reorder(variable_importance$name, variable_importance$reduction)


# in this case study, the approach is clearly problematic, as no clear cut distinction between
# signal and noise values can be drawn. it is probably more useful when component-wise boosting
# is done in a quasi interpretable ML approach to enforce sparsity where inference over model parameters
# is not as relevant as in the proposed case study
plt = ggplot(data = variable_importance, mapping = aes(x = name, y = reduction, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = viridis(n = 3, alpha = 0.8, begin = 0.3, end = 0.7)) +
  coord_flip() +
  labs(x = "Base learner", y = "Risk reduction", fill = "Deselection Approach") +
  theme(legend.position = c(.8,.1))

ggsave(
  plot = plt
  , filename = file.path("results", "figures", "madagascar_deselectlearner.png")
  , width = 200
  , height = 260
  , units = "mm"
)
