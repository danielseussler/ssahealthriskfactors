#
#
#
#

library(data.table)
library(ggplot2)
library(viridis)
library(Metrics)

load(file = file.path("models", "88qlclkf.rda")) # moderately stunted simulation
load(file = file.path("models", "r7zhq0pi.rda")) # severely stunted simulation

res_moderately[, predc := as.integer(pred >= 0.5)]
res_severely[, predc := as.integer(pred >= 0.5)]

c("B", "D", "F", "J", "L", "M")

fnc_rename = function(x) switch(
  x
  , "B" = "CV\n5-fold"
  , "D" = "CV\n10-fold"
  , "F" = "Bootstrap\n25-fold"
  , "J" = "Subsampling\n25-fold p = 0.5"
  , "L" = "Subsampling\n25-fold p = 0.8"
  , "M" = "Cluster subsampling\n25-fold p = 0.5"
)


res_moderately[, type := sapply(type, fnc_rename)]
res_moderately[, type := factor(type, levels = c("CV\n5-fold", "CV\n10-fold", "Bootstrap\n25-fold", "Subsampling\n25-fold p = 0.5", "Subsampling\n25-fold p = 0.8", "Cluster subsampling\n25-fold p = 0.5"))]

res_severely[, type := sapply(type, fnc_rename)]
res_severely[, type := factor(type, levels = c("CV\n5-fold", "CV\n10-fold", "Bootstrap\n25-fold", "Subsampling\n25-fold p = 0.5", "Subsampling\n25-fold p = 0.8", "Cluster subsampling\n25-fold p = 0.5"))]


# aggregate metrics for each iteration
metr_moderately = res_moderately[
  , .(ncoef = min(ncoef), mstop = min(mstop), loss = logLoss(actual, pred), acc = accuracy(actual, predc),
      ce = ce(actual, predc), auc = auc(actual, pred), brier = mse(actual, pred),
      rec = recall(actual, predc), pre = precision(actual, predc))
  , by = list(holdout, type, iter)
]

metr_severely = res_severely[
  ,.(ncoef = min(ncoef), mstop = min(mstop), loss = logLoss(actual, pred), acc = accuracy(actual, predc),
     ce = ce(actual, predc), auc = auc(actual, pred), brier = mse(actual, pred),
     rec = recall(actual, predc), pre = precision(actual, predc))
  , by = list(holdout, type, iter)
]

summary(metr_moderately)
summary(metr_severely)

# k-fold fails to stop for some iterations, in total these are only a selected few
# since this is probably due to convergence and not overfitting, no further corrections are pursued
metr_moderately[mstop == 3000]
metr_severely[mstop == 3000]

# results model complexity generalization error (true)
plt = ggplot(
  data = melt(
    metr_moderately, id.vars = c("holdout", "type", "iter"),
    measure.vars = c("ncoef", "mstop", "loss", "ce", "auc", "brier", "rec", "pre"))
  , aes(x = type, y = value)
) +
  geom_boxplot(outlier.alpha = 0.5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_grid(variable ~ holdout, scales = "free") +
  labs(x = "", y = "") +
  theme_gray()

ggsave(
  plot = plt,
  filename = "madagascar_simulation_fit_m.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 260
  , units = "mm", device = png
)

# results for the severely stunted regression
plt = ggplot(
  data = melt(
    metr_severely, id.vars = c("holdout", "type", "iter"),
    measure.vars = c("ncoef", "mstop", "loss", "ce", "auc", "brier", "rec", "pre"))
  , aes(x = type, y = value)
) +
  geom_boxplot(outlier.alpha = 0.5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_grid(variable ~ holdout, scales = "free") +
  labs(x = "", y = "") +
  theme_gray()

ggsave(
  plot = plt,
  filename = "madagascar_simulation_fit_s.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 260
  , units = "mm", device = png
)
