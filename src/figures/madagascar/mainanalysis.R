#
#
#
#

library(data.table)
library(mboost)
library(ggplot2)
library(patchwork)
library(sf)

# load data and helper functions
source(file = file.path("src", "utils", "func_tidy_baselearner.R"))
source(file = file.path("src", "utils", "func_plot_partial_effects.R"))
load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
dhs_shp = readRDS(file = file.path("data", "processed", "madagascar", "dhsboundaries.rds"))

# extract the defaults
fnames = list.files(path = file.path("models", "madagascar", "main"))

# plot the smooth effect for the continuous variables
vars = c("cage", "mage", "mbmi", "medu", "hmembers", "healthaccess", "cityaccess")

# extract for all models
plot_data = data.table()

for (i in 1:length(fnames)) {
  load(file = file.path(file.path("models", "madagascar", "main", fnames[i])))

  for (var in vars) {
    tmp_bls = extract(mod, what = "bnames", which = paste0("(", var))
    tmp_x = as.data.frame(surveydata[, var])
    names(tmp_x) = var

    tmp_y = rowSums(predict(mod, which = tmp_bls, newdata = tmp_x, type = "link"))
    tmp_name = switch(
      var
      , "cage" = "Children's age"
      , "mage" = "Mother's age"
      , "mbmi" = "Mother's BMI"
      , "medu" = "Mother's education in years"
      , "hmembers" = "No. of household members"
      , "healthaccess" = "Access to health facilites"
      , "cityaccess" = "Access to cities")

    tmp_plot_data = data.table(vars = tmp_name, id = i, x = tmp_x[, 1], y = tmp_y)
    plot_data = rbindlist(list(plot_data, tmp_plot_data))
  }
}

plot_data2 = plot_data[, .(y = mean(y)), by = .(vars, x)] # aggregate to point wise mean

plt = ggplot() +
  geom_line(data = plot_data, mapping = aes(x = x, y = y, group = id), alpha = 0.2) +
  geom_line(data = plot_data2, mapping = aes(x = x, y = y), color = "red") +
  geom_rug(data = plot_data2, mapping = aes(x = x, y = y), sides = "b", position = "jitter") +
  labs(x = "", y = expression(f[partial])) +
  facet_wrap(~ vars, scales = "free")

ggsave(
  plot = plt
  , filename = "madagascar_smootheffects.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 200L
  , units = "mm", device = png, bg = "transparent"
)


# plot the markov random field effect
mdg_regions = levels(surveydata$dhsregion)
dhs_shp = dplyr::arrange(dhs_shp, DHSREGEN)

# extract partial effects
plot_data = data.table()
for (i in 1:length(fnames)) {
  load(file = file.path(file.path("models", "madagascar", "main", fnames[i])))
  tmp_coef = coef(mod, which = "dhsregion")[[1]]
  plot_data = rbindlist(list(plot_data, data.table(name = names(tmp_coef), y = tmp_coef)))
}

plot_data = plot_data[order(name), .(mean = mean(y), sd = sd(y)), by = name]

any(dhs_shp$DHSREGEN != plot_data$name)

dhs_shp$mean = plot_data$mean
dhs_shp$sd = plot_data$sd

plt_A = ggplot(data = dhs_shp) +
  geom_sf(aes(fill = mean), color = NA) +
  scale_fill_viridis_c(name = expression(bar(f)[MRF])) +
  theme_void()

plt_B = ggplot(data = dhs_shp) +
  geom_sf(aes(fill = sd), color = NA) +
  scale_fill_viridis_c(option = "cividis", name = expression(sigma[f[MRF]]), limits = c(0, 0.11)) +
  theme_void()

ggsave(
  plot = plt_A + plt_B +
    plot_annotation(tag_levels = "A") +
    plot_layout(widths = c(1, 1))
  , filename = "madagascar_spatialeffects.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 120
  , units = "mm", device = png
)


# plot the categorical effects (probably for the appendix)
vars = c("csex", "ctwin", "cbord", "memployed", "mreligion", "nodead", "watersource", "sanitation",
         "wealth", "electricity", "radio", "television", "bicycle", "motorcycle", "car", "urban", "fews")

plot_data = data.table()
for (i in 1:length(fnames)) {
  load(file = file.path(file.path("models", "madagascar", "main", fnames[i])))
  tmp_intercept = mod$offset

  for (var in vars) {
    tmp_coef = coef(mod, which = var)[[1]]
    # note: this is the intercept, while it could be interpreted as the partial effect of the
    # reference category, we plot the treatment variables and do not consider the intercept
    tmp_intercept = tmp_intercept + tmp_coef[1]
    tmp_coef[1] = 0
    tmp_coef = unname(tmp_coef)
    tmp_labels = levels(surveydata[, var])

    tmp_name = switch(
      var
      , "csex" = "Children's gender"
      , "ctwin" = "Children has twins"
      , "cbord" = "Children's birth order"
      , "memployed" = "Mother's employment status"
      , "mreligion" = "Mother's religion"
      , "nodead" = "No. of dead siblings"
      , "watersource" = "Watersource"
      , "sanitation" = "Sanitation"
      , "wealth" = "Wealth Index"
      , "electricity" = "Household has electricity"
      , "radio" = "Household owns radio"
      , "television" = "Household owns television"
      , "bicycle" = "Household owns bicycle"
      , "motorcycle" = "Household owns motorcycle"
      , "car" = "Household owns car"
      , "urban" = "Urbanicity"
      , "fews" = "FEWS Food Security Classification"
    )

    tmp_plot_data = data.table(vars = tmp_name, id = i, level = tmp_labels, y = tmp_coef)
    plot_data = rbindlist(list(plot_data, tmp_plot_data))
  }

  tmp_plot_data_intercept = data.table(vars = "Intercept", id = i, level = "Intercept", y = tmp_intercept)
  plot_data = rbindlist(list(plot_data, tmp_plot_data_intercept))
}

plot_data[level == "fjkm/flm/anglikana", level := "fjkm/flm\n/anglikana"]
plot_data[level == "other christian", level := "other\nchristian"]

plot_data2 = plot_data[, .(y = mean(y)), by = .(vars, level)] # aggregate to point wise mean

ordered_levels = c("Intercept", "no", "yes", "male", "female", "0", "1", "2", "3", "4", "5", "6", "7", "8",
                   "catholic", "other", "fjkm/flm\n/anglikana", "other\nchristian",
                   "unimproved", "improved", "piped", "rural", "urban",
                   "poorest", "poorer", "middle", "richer", "richest")
plot_data$level = factor(plot_data$level, levels = ordered_levels, ordered = TRUE)
plot_data2$level = factor(plot_data2$level, levels = ordered_levels, ordered = TRUE)

plt = ggplot() +
  geom_line(data = plot_data, mapping = aes(x = level, y = y, group = id), alpha = 0.2) +
  geom_boxplot(data = plot_data, mapping = aes(x = level, y = y), width = 0.5) +
  geom_point(data = plot_data2, mapping = aes(x = level, y = y), color = "red") +
  labs(x = "", y = expression(f[partial])) +
  facet_wrap(~ vars, scales = "free", drop = TRUE, ncol = 3)

ggsave(
  plot = plt
  , filename = "madagascar_categoricaleffects.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 260L
  , units = "mm", device = png, bg = "transparent"
)
