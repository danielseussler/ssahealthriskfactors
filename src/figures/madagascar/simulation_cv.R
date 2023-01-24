#
#
#
#

library(data.table)
library(Metrics)
library(ggplot2)
library(patchwork)

load(file = file.path("models", "k30w39r3.rda"))

res_moderately[, regression := "moderately or severely"]
res_moderately[, risk_diff := true_risk - cv_risk]
res_moderately[, as.list(summary(risk_diff)), by = .(type)]
res_moderately[, as.list(sd(risk_diff)), by = .(type)]

res_severly[, regression := "severely"]
res_severly[, risk_diff := true_risk - cv_risk]
res_severly[, as.list(summary(risk_diff)), by = .(type)]
res_severly[, as.list(sd(risk_diff)), by = .(type)]

fnc_rename = function(x) switch(
  x
  , "A" = "CV 5-fold "
  , "B" = "CV 5-fold stratified"
  , "C" = "CV 10-fold "
  , "D" = "CV 10-fold stratified"
  , "E" = "Bootstrap 25-fold"
  , "F" = "Bootstrap 25-fold stratified"
  , "G" = "Bootstrap 0.632 25-fold"
  , "H" = "Bootstrap 0.632 25-fold stratified"
  , "I" = "Subsampling 25-fold p = 0.5"
  , "J" = "Subsampling 25-fold stratified p = 0.5"
  , "K" = "Subsampling 25-fold p = 0.8"
  , "L" = "Subsampling 25-fold stratified p = 0.8"
  , "M" = "Cluster subsampling 25-fold p = 0.5"
)

levels = c("CV 5-fold ", "CV 5-fold stratified", "CV 10-fold ", "CV 10-fold stratified", "Bootstrap 25-fold",
           "Bootstrap 25-fold stratified", "Bootstrap 0.632 25-fold", "Bootstrap 0.632 25-fold stratified",
           "Subsampling 25-fold p = 0.5", "Subsampling 25-fold stratified p = 0.5", "Subsampling 25-fold p = 0.8",
           "Subsampling 25-fold stratified p = 0.8", "Cluster subsampling 25-fold p = 0.5")

res_moderately[, type := sapply(type, fnc_rename)]
res_moderately[, type := factor(type, levels = levels)]

res_severly[, type := sapply(type, fnc_rename)]
res_severly[, type := factor(type, levels = levels)]


# add true values of simulation
tmp = res_moderately[
  , .(mean = mean(cv_risk), sd = sd(cv_risk), true_mean = mean(true_risk), true_sd = sd(true_risk))
  , by = .(holdout, type, regression)
]

tmp
add1 = data.table(holdout = "A", type = "true", regression = "moderately or severely", true_mean = 0.6381456 , true_sd = 0.005575042, mean = 0.6381456, sd = 0.005575042)
add2 = data.table(holdout = "B", type = "true", regression = "moderately or severely", true_mean = 0.6385714 , true_sd = 0.005901664, mean = 0.6385714, sd = 0.005901664)
res_moderately = rbindlist(list(tmp, add1, add2), fill=TRUE)
res_moderately[, lower := mean - qnorm(0.975) * sd]
res_moderately[, upper := mean + qnorm(0.975) * sd]

res_moderately[, bias := true_mean - mean]
res_moderately[is.na(bias), bias := 0]


tmp = res_severly[
  , .(mean = mean(cv_risk), sd = sd(cv_risk), true_mean = mean(true_risk), true_sd = sd(true_risk))
  , by = .(holdout, type, regression)
]

tmp
add1 = data.table(holdout = "A", type = "true", regression = "severely", true_mean = 0.3675347 , true_sd = 0.01286589, mean = 0.3675347, sd = 0.01286589)
add2 = data.table(holdout = "B", type = "true", regression = "severely", true_mean = 0.3662259 , true_sd = 0.01064278, mean = 0.3662259, sd = 0.01064278)
res_severly = rbindlist(list(tmp, add1, add2), fill=TRUE)
res_severly[, lower := mean - qnorm(0.975) * sd]
res_severly[, upper := mean + qnorm(0.975) * sd]

res_severly[, bias := true_mean - mean]
res_severly[is.na(bias), bias := 0]


# plot figures
plt = ggplot(data = rbindlist(list(res_moderately, res_severly)), mapping = aes(x = type, y = mean)) +
  geom_pointrange(mapping = aes(ymin = lower, ymax = upper)) +
  facet_grid(regression ~ holdout, scales = "free") +
  labs(x = "", y = expression(italic(hat(R)))) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(
  plot = plt
  , filename = "madagascar_simulation_cv_1.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 200
  , units = "mm", device = png
)

# bias
plt = ggplot(data = rbindlist(list(res_moderately, res_severly)), mapping = aes(x = type, y = bias)) +
  geom_point() +
  geom_segment(mapping = aes(x = type, xend = type, y = 0, yend = bias)) +
  geom_abline(intercept = 0, slope = 0, color = "grey") +
  facet_grid(regression ~ holdout, scales = "free") +
  labs(x = "", y = expression(italic(hat(R)[true]) - italic(hat(R)))) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(
  plot = plt
  , filename = "madagascar_simulation_cv_2.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 200
  , units = "mm", device = png
)

# standard deviation
plt = ggplot(data = rbindlist(list(res_moderately, res_severly)), mapping = aes(x = type, y = sd)) +
  geom_point() +
  geom_segment(mapping = aes(x = type, xend = type, y = 0, yend = sd)) +
  geom_abline(intercept = 0, slope = 0, color = "grey") +
  facet_grid(regression ~ holdout, scales = "free") +
  labs(x = "", y = expression(sd(italic(hat(R))))) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(
  plot = plt
  , filename = "madagascar_simulation_cv_3.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 200
  , units = "mm", device = png
)
