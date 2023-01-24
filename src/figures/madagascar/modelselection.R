# model selection
# compare hold-out metrics for model selection
#
#

library(data.table)
library(Metrics)
library(ggplot2)

load(file = file.path("models", "madagascar", "lvcw0a2q.rda"))

results[, class := as.integer(pred >= 0.5)]

results_metrics = results[
  , .(loss = logLoss(actual, pred)
      , acc = accuracy(actual, class)
      , ce = ce(actual, class)
      , auc = auc(actual, pred)
      , brier = mse(actual, pred)
      , rec = recall(actual, class)
      , pre = precision(actual, class)
      , mstop = min(mstop))
  , by = list(model, iter)
]

results_metrics

plt = ggplot(data = results_metrics, mapping = aes(x = model, y = loss)) +
  geom_line(mapping = aes(group = iter), color = "grey") +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  labs(x = "Specification", y = "Average hold-out risk")

ggplot(data = results_metrics, mapping = aes(x = model, y = ce)) +
  geom_line(mapping = aes(group = iter), color = "grey") +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  labs(x = "Specification", y = "Hold-out Classification Error")

ggplot(data = results_metrics, mapping = aes(x = model, y = brier)) +
  geom_line(mapping = aes(group = iter), color = "grey") +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  labs(x = "Specification", y = "Holdout Brier Score")

ggsave(
  plot = plt
  , filename = "madagascar_modelselection.png"
  , path = file.path("results", "figures")
  , dpi = 600
  , width = 200
  , height = 80
  , units = "mm"
  , device = png
)
