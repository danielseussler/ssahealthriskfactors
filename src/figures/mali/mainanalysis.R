#
#
#
#

library(sf)
library(h3)
library(gamboostLSS)
library(gamlss.dist)
library(ggplot2)
library(viridis)
library(patchwork)
library(data.table)

# load custom plot functions
# load preprocessed data
source(file = file.path("src", "utils", "func_tidy_baselearner.R"))
source(file = file.path("src", "utils", "func_plot_partial_effects.R"))
load(file = file.path("data", "processed", "mali", "surveydata.rda"))
load(file = file.path("data", "processed", "mali", "geodata.rda"))
dhs_shp = readRDS(file = file.path("data", "processed", "mali", "dhsboundaries.rds"))
gadm_shp = readRDS(file = file.path("data", "processed", "mali", "regions.rds"))

# data prep
cloc = as.data.frame(cloc)
cloc$log_pop = log(cloc$pop + 1)

# load the main model (named fitted instead of mod)
load(file = file.path("models", "mali", "9dkw7wyn.rda"))

# load the predictions from the bootstrap replicates (prds instead of pred)
load(file = file.path("models", "mali", "prds.rda"))

# plot smooth effects
# extract for all models then overlay fit main model
fnames = list.files(path = file.path("models", "mali", "main"))
vars = c("elev", "precip", "evi", "ndvi", "lstnight", "lstday", "log_pop")
plot_data = data.frame()

for (i in 1:length(fnames)) {
  load(file = file.path(file.path("models", "mali", "main", fnames[i])))
  tmp_mod = mod[["mu"]]

  for (var in vars) {
    tmp_bls = extract(tmp_mod, what = "bnames", which = paste0("(", var))
    tmp_x = as.data.frame(cloc[, var])
    names(tmp_x) = var

    tmp_y = rowSums(predict(tmp_mod, which = tmp_bls, newdata = tmp_x, type = "link"))
    tmp_name = switch(
      var
      , "elev" = "Elevation"
      , "precip" = "Precipitation"
      , "evi" = "Enhanced Vegetation Index (EVI)"
      , "ndvi" = "Normalized Difference Vegetation\nIndex (NDVI)"
      , "lstnight" = "Land surface temperature (night)"
      , "lstday" = "Land surface temperature (day)"
      , "log_pop" = "Population (log)"
    )

    tmp_plot_data = data.table(vars = tmp_name, id = i, x = tmp_x[, 1], y = tmp_y)
    plot_data = rbindlist(list(plot_data, tmp_plot_data))
  }
}

plot_data2 = data.frame()
tmp_mod = fitted[["mu"]]

for (var in vars) {
  tmp_bls = extract(tmp_mod, what = "bnames", which = paste0("(", var))
  tmp_x = as.data.frame(training_data[, var])
  names(tmp_x) = var

  tmp_y = rowSums(predict(tmp_mod, which = tmp_bls, newdata = tmp_x, type = "link"))
  tmp_name = switch(
    var
    , "elev" = "Elevation"
    , "precip" = "Precipitation"
    , "evi" = "Enhanced Vegetation Index (EVI)"
    , "ndvi" = "Normalized Difference Vegetation\nIndex (NDVI)"
    , "lstnight" = "Land surface temperature (night)"
    , "lstday" = "Land surface temperature (day)"
    , "log_pop" = "Population (log)"
  )

  tmp_plot_data = data.frame(vars = tmp_name, x = tmp_x[, 1], y = tmp_y)
  plot_data2 = rbind(plot_data2, tmp_plot_data)
}


plt = ggplot() +
  geom_line(data = plot_data, mapping = aes(x = x, y = y, group = id), alpha = 0.2) +
  geom_line(data = plot_data2, mapping = aes(x = x, y = y), color = "red") +
  geom_rug(data = plot_data2, mapping = aes(x = x, y = y), sides = "b", position = "jitter") +
  labs(x = "", y = expression(f[partial])) +
  facet_wrap(~ vars, scales = "free")

ggsave(
  plot = plt
  , filename = "mali_maineffects.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 200L
  , units = "mm", device = png, bg = "transparent"
)


# plot spatial effect of the main model
crop_to = st_bbox(dhs_shp)
crop_to["ymax"] = 20

plt = plt_spatialsmooth(
  .mod = fitted
  , .data = training_data
  , .parameter = "mu"
  , .shp = st_crop(dhs_shp, crop_to)
  , .limscol = c(-4, 4)
  ) + theme_void()

ggsave(
  plot = plt
  , filename = "mali_spatialeffect.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 150L
  , units = "mm", device = png, bg = "transparent"
)


# plot standard error of the spatial effect from the bootstrap refits
plot_data = data.table()

for (i in 1:length(fnames)) {
  load(file = file.path(file.path("models", "mali", "main", fnames[i])))
  tmp_mod = mod[["mu"]]

  tmp_bls = unique(c(
    extract(tmp_mod, what = "bnames", which = paste0("lon")),
    extract(tmp_mod, what = "bnames", which = paste0("lat"))
  ))

  tmp_locs = polyfill(polygon = st_union(st_crop(dhs_shp, crop_to)), res = 5L)
  tmp_coords = h3_to_geo(tmp_locs)
  tmp_coords = data.frame("lon" = tmp_coords[, 2], "lat" = tmp_coords[, 1])
  tmp_z = rowSums(predict(tmp_mod, which = tmp_bls, newdata = tmp_coords))

  tmp_plot_data = data.table(h3_index = tmp_locs, z = tmp_z)
  plot_data = rbindlist(list(plot_data, tmp_plot_data))
}

plot_data = plot_data[, .(se = sd(z)), by = h3_index]
plot_data_sf = h3_to_geo_boundary_sf(plot_data$h3_index)
plot_data_sf$se = plot_data$se

plt = ggplot(data = plot_data_sf) +
  geom_sf(aes(fill = se), color = NA) +
  scale_fill_viridis_c(option = "cividis", name = expression(sigma[f[partial]]), limits = c(0, 0.65)) +
  theme_void()

ggsave(
  plot = plt
  , filename = "mali_spatialeffectuncertainty.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 150L
  , units = "mm", device = png, bg = "transparent"
)


# plot categorical covariates
plot_data = data.frame(id = integer(), parameter = character(), effect = character(),
                       level = character(), value = numeric())

for (i in 1:length(fnames)) {
  load(file = file.path(file.path("models", "mali", "main", fnames[i])))

  tmp_data = data.frame(id = i, parameter = c("mu", "mu", "mu", "sigma", "sigma", "sigma"),
                        effect = c("Intercept", "Urbanicity", "Urbanicity", "Intercept", "Urbanicity", "Urbanicity"),
                        level = c("Intercept", "rural", "urban", "Intercept", "rural", "urban"), value = 0)

  # extract intercepts which is offset + intercept from urbanicity
  tmp_data[1, 5] = mod$mu$offset + coef(mod, parameter = "mu", which = "urban")$`bols(urban)`[1]
  tmp_data[4, 5] = mod$sigma$offset + coef(mod, parameter = "sigma", which = "urban")$`bols(urban)`[1]

  tmp_data[3, 5] = coef(mod, parameter = "mu", which = "urban")$`bols(urban)`[2]
  tmp_data[6, 5] = coef(mod, parameter = "sigma", which = "urban")$`bols(urban)`[2]

  plot_data = rbind(plot_data, tmp_data)
}

plot_data$level = factor(plot_data$level)

plot_data2 = data.frame(
  parameter = c("mu", "mu", "mu", "sigma", "sigma", "sigma")
  , effect = c("Intercept", "Urbanicity", "Urbanicity", "Intercept", "Urbanicity", "Urbanicity")
  , level = c("Intercept", "rural", "urban", "Intercept", "rural", "urban")
  , value = c(
    fitted$mu$offset + coef(fitted, parameter = "mu", which = "urban")$`bols(urban)`[1]
    , 0
    , coef(fitted, parameter = "mu", which = "urban")$`bols(urban)`[2]
    , fitted$sigma$offset + coef(fitted, parameter = "sigma", which = "urban")$`bols(urban)`[1]
    , 0
    , coef(fitted, parameter = "sigma", which = "urban")$`bols(urban)`[2]
  ))

plt = ggplot(data = plot_data) +
  geom_line(mapping = aes(x = level, y = value, group = id), alpha = 0.2) +
  geom_boxplot(mapping = aes(x = level, y = value), width = 0.5) +
  geom_point(data = plot_data2, mapping = aes(x = level, y = value), color = "red") +
  geom_line(data = plot_data2, mapping = aes(x = level, y = value), color = "red") +
  labs(x = "", y = expression(f[partial])) +
  facet_wrap(. ~ parameter + effect, scales = "free", drop = TRUE, ncol = 4, labeller = label_parsed)

ggsave(
  plot = plt
  , filename = "mali_categoricaleffects.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 80L
  , units = "mm", device = png, bg = "transparent"
)


# very few observations (5) got lost by matching with administrative regions
# to remove pixel wise errors from the plotted maps these are added as 0 only here
sf::sf_use_s2(FALSE)
missings = st_buffer(st_crop(gadm_shp$admin0, crop_to), dist = -0.5)
missings = polyfill(missings, res = 7)
missings = setdiff(missings, prds$h3_index)
missings = h3_to_geo_boundary_sf(missings)
missings$fill = 0
sf::sf_use_s2(TRUE)



# plot predictions (estimated mean of the conditional distribution)
# since northern mali is mostly extrapolated the predictions are not reliable
# the most northern observation is at the 18th latitude, crop correspondingly
pred_coords = h3_to_geo(h3_index = pred$h3_index)
plot_data = h3_to_geo_boundary_sf(pred$h3_index)
plot_data = cbind(plot_data, pred)
plot_data = plot_data[pred_coords[, 1] < 20, ]

plt = ggplot() +
  geom_sf(data = plot_data, mapping = aes(fill = mu), color = NA) +
  geom_sf(data = missings, mapping = aes(fill = fill), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "viridis"
    , name = expression(hat(mu))
    , limits = c(0, 1)
  ) +
  theme_void()

ggsave(
  plot = plt
  , filename = "mali_predictions.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 150L
  , units = "mm", device = png, bg = "transparent"
)



# do the quantile plots of the 5 and 95 quantile
#
prds = melt(prds, id.vars = c("h3_index", "pop"))
prds = prds[, .(lower = quantile(value, probs = 0.10),
                upper = quantile(value, probs = 0.90),
                se = sd(value)), by = h3_index]


tmp_coords = h3_to_geo(h3_index = prds$h3_index)
plot_data = h3_to_geo_boundary_sf(pred$h3_index)
plot_data = cbind(plot_data, prds)
plot_data = plot_data[tmp_coords[, 1] < 20, ]

list_of_plt = list()
list_of_plt[[1]] = ggplot() +
  geom_sf(data = plot_data, mapping = aes(fill = lower), color = NA) +
  geom_sf(data = missings, mapping = aes(fill = fill), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "viridis"
    , name = expression(Q[0.1](hat(mu)))
    , limits = c(0, 1)
  ) +
  theme_void()

list_of_plt[[2]] = ggplot() +
  geom_sf(data = plot_data, mapping = aes(fill = upper), color = NA) +
  geom_sf(data = missings, mapping = aes(fill = fill), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "viridis"
    , name = expression(Q[0.9](hat(mu)))
    , limits = c(0, 1)
  ) +
  theme_void()

list_of_plt[[3]] = ggplot() +
  geom_sf(data = plot_data, mapping = aes(fill = se), color = NA) +
  geom_sf(data = missings, mapping = aes(fill = fill), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "cividis"
    , name = expression(se(hat(mu)))
    , limits = c(0, 0.1)
  ) +
  theme_void()

ggsave(
  plot = wrap_plots(list_of_plt, ncol = 1) +
    plot_annotation(tag_levels = "A") +
    plot_layout(widths = c(1))
  , filename = "mali_predictionuncertainty.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 260L
  , units = "mm", device = png, bg = "transparent"
)
