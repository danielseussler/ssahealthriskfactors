#
#
#
#

library(ggplot2)
library(survey)
library(dplyr)
library(data.table)
library(gamboostLSS)

load(file = file.path("models", "mali", "prds.rda"))
load(file = file.path("models", "mali", "9dkw7wyn.rda"))
load(file = file.path("data", "processed", "mali", "geodata.rda"))
mali_gadm = readRDS(file = file.path("data", "processed", "mali", "regions.rds"))

# data prep
grid$log_pop = log(grid$pop + 1)
grid$pred = c(predict(fitted, newdata = grid, parameter = "mu", type = "response"))

# get design-based estimates
variables = c("hv000", "hv001", "hv002", "hv005", "hv023", "hv024", "hv025", "hml32", "hml35", "hv042", "hv103", "hc1")

sv = readRDS(file = file.path("data", "raw", "rdhs", "MLPR81FL.rds"))
sv = sv[variables]
sv = subset(sv, hv042 == 1 & hv103 == 1 & hc1 %in% 6:59 & hml35 %in% 0:1)
sv = mutate(sv, hv024 = labelled::to_character(hv024), hv025 = labelled::to_character(hv025))

# survey design - two stage sampling, clusters are first stage psu (no sampling weights)
# second stage is inverse proportional probability sampling pps
# national estimates can also be extracted by DHS survey package
# weights are household level / 1e6 by DHS Guide
design = svydesign(ids = ~hv001+hv002, strata = ~hv023, data = sv, weights = ~hv005)
estimates_design = svyby(formula = ~hml35, by = ~hv024, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")
estimates_design$admin1 = stringr::str_to_title(estimates_design$hv024)
estimates_design$type = "design-based"

# comparison of mean and 95% confidence intervals of the mean
# shift wide data table, weighted average over locations, the mean and standard error over iterations
colnames(prds)[3:52] = paste0("iter", 1:50)
est_admin1 = cbind(admin1 = grid$admin1, admin2 = grid$admin2, pred = grid$pred, prds)
est_admin1 = melt(est_admin1, id.vars = c("h3_index", "admin1", "admin2", "pop", "pred"))

est_admin1[, pred := weighted.mean(pred, w = pop, na.rm = TRUE), by = admin1]
est_admin1 = est_admin1[, .(value = weighted.mean(value, w = pop, na.rm = TRUE)), by = .(admin1, pred, variable)]
est_admin1 = est_admin1[order(admin1), .(hml35 = mean(value), se = sd(value), ci_l = quantile(value, prob = 0.025), ci_u = quantile(value, prob = 0.975)), by = .(admin1, pred)]
est_admin1[, type := "model-based"]

plot_data = rbindlist(list(est_admin1, estimates_design), fill = TRUE)

plt = ggplot(data = plot_data, mapping = aes(x = admin1, y = hml35, group = type, shape = type, color = type)) +
  geom_pointrange(mapping = aes(ymin = ci_l, ymax = ci_u), position = position_dodge(width = 0.4)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.4)) +
  scale_color_viridis_d(begin = 0.3, end = 0.7) +
  labs(x = "", y = "Prevalence", shape = "Method", color = "Method") +
  theme(legend.position = "bottom")

ggsave(
  plot = plt
  , filename = "mali_estimatesadmin1.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 100
  , units = "mm", device = png
)


# urban percentage per admin
# cf. report Tableau A.1 Répartition de la population dans la base de sondage
grid[, urban1 := ifelse(urban == "urban", 1, 0)]
pop_comparison = grid[order(admin1), (mean = weighted.mean(urban1, pop)), by = admin1]

pop_comparison[admin1 == "Bamako", V2 := 1.0]
pop_comparison[admin1 == "Gao", V2 := 0.21]
pop_comparison[admin1 == "Kayes", V2 := 0.14]
pop_comparison[admin1 == "Kidal", V2 := 0.383]
pop_comparison[admin1 == "Koulikoro", V2 := 0.055]
pop_comparison[admin1 == "Mopti", V2 := 0.098]
pop_comparison[admin1 == "Segou", V2 := 0.086]
pop_comparison[admin1 == "Sikasso", V2 := 0.164]
pop_comparison[admin1 == "Tombouctou", V2 := 0.128]

ggplot(data = pop_comparison, mapping = aes(x = V2, y = V1, label = admin1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "grey") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1), labels = scales::percent_format()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), labels = scales::percent_format()) +
  labs(
    x = "Share of urban-population\nPopulation census 2009"
    , y = "Share of urban-population\nGHSL-derived"
    , caption = "Source: 4ème Recensement Général de la Population et de l’Habitat du Mali, 2009 (RGPH-2009),\nInstitut National de la Statistique (INSTAT)"
  ) +
  geom_text()


# Koulikoro, Segou, Sikasso have the highest differences in estimated shares of urban population
# by most then ten percentage points
pop_comparison[, diff := V1 - V2][order(-diff)]

# admin1        V1    V2         diff
# 1:  Koulikoro 0.4167638 0.055  0.361763841
# 2:      Segou 0.2220625 0.086  0.136062470
# 3:    Sikasso 0.2557511 0.164  0.091751126
# 4:      Kayes 0.2236682 0.140  0.083668166
# 5:      Mopti 0.1669608 0.098  0.068960808
# 6:        Gao 0.2650273 0.210  0.055027322
# 7: Tombouctou 0.1743629 0.128  0.046362869
# 8:     Bamako 0.9915807 1.000 -0.008419341
# 9:      Kidal 0.2952910 0.383 -0.087708967


# create admin 2 estimates for the github repository
est_admin2 = cbind(admin1 = grid$admin1, admin2 = grid$admin2, pred = grid$pred, prds)
est_admin2 = melt(est_admin2, id.vars = c("h3_index", "admin1", "admin2", "pop", "pred"))

est_admin2[, pred := weighted.mean(pred, w = pop, na.rm = TRUE), by = admin2]
est_admin2 = est_admin2[, .(value = weighted.mean(value, w = pop, na.rm = TRUE)), by = .(admin2, pred, variable)]
est_admin2 = est_admin2[order(admin2), .(se = sd(value), ci_l = quantile(value, prob = 0.025, na.rm = T), ci_u = quantile(value, prob = 0.975, na.rm = T)), by = .(admin2, pred)]
est_admin2[, type := "model-based"]

fwrite(est_admin2, file.path("results", "predictions", "mali_malaria_risk_admin2.csv"))
