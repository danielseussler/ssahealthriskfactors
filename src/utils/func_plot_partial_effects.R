# plot estimated partial effects, inspired by
# https://github.com/adamdsmith/NanSound_EcolEvol/blob/master/R/ggplot_effects.R
#
#

# plot continuous covariates with possibly smooth effects
plt_smooth = function(.mod, .data, .parameter = NULL, .var, .ylim = NULL, .rugged = FALSE,
                      .title = NULL, .xlab = NULL) {

  require(ggplot2)
  require(mboost)

  if (!is.null(.parameter)) .mod <- .mod[[.parameter]]

  bls = extract(.mod, what = "bnames", which = paste0("(", .var))
  x = as.data.frame(.data[, .var])
  names(x) = .var
  y = rowSums(predict(.mod, which = bls, newdata = x, type = "link"))
  tmp = data.frame(x = x[, .var], y = y)

  plt = ggplot(data = tmp, mapping = aes(x = x, y = y)) +
    geom_line(color = "black", linewidth = 1L) +
    labs(y = expression(f[partial]))

  if (!is.null(.xlab)) plt = plt + xlab(.xlab)
  if (!is.null(.ylim)) plt = plt + ylim(.ylim)
  if (isTRUE(.rugged)) plt = plt + geom_rug(sides = "b", position = "jitter")
  if (!is.null(.title)) plt = plt + ggtitle(.title) + theme(plot.title = element_text(hjust = 0.5))

  return(plt)
}


# plot bivariate smooth effects
plt_spatialsmooth = function(.mod, .data, .parameter = NULL, .var = c("lon", "lat"), .shp, .res = 5L,
                             .limscol = NULL, .title = NULL) {

  require(ggplot2)
  require(h3)
  require(sf)
  require(mboost)
  require(viridis)

  if (!is.null(.parameter)) .mod <- .mod[[.parameter]]

  bls = unique(c(
    extract(.mod, what = "bnames", which = paste0(.var[1])),
    extract(.mod, what = "bnames", which = paste0(.var[2]))
  ))

  locs = polyfill(polygon = st_union(.shp), res = .res)
  coords = h3_to_geo(locs)
  coords = data.frame("lon" = coords[, 2], "lat" = coords[, 1])
  z = rowSums(predict(.mod, which = bls, newdata = coords))
  dt_sf = h3_to_geo_boundary_sf(locs)
  dt_sf$z = z

  plt = ggplot(data = dt_sf) +
    geom_sf(aes(fill = z), color = NA) +
    scale_fill_viridis_c(name = expression(f[partial]), limits = .limscol) +
    labs(x = "Longitude", y = "Latitude")

  if (!is.null(.title)) plt = plt + ggtitle(.title) + theme(plot.title = element_text(hjust = 0.5))

  return(plt)
}


# plot estimated GMRF effects
plt_gmrf = function(.mod, .data, .parameter = NULL, .var, .shp, .limscol = NULL, .title = NULL) {

  require(ggplot2)
  require(sf)
  require(mboost)
  require(viridis)

  if (!is.null(.parameter)) .mod <- .mod[[.parameter]]

  labels = levels(.data[, .var])
  coefs = coef(.mod, which = .var)[[1]]
  shp = .shp
  shp = dplyr::arrange(shp, DHSREGEN)
  coefs = coefs[order(names(coefs))]

  if(any(shp$DHSREGEN != names(coefs))) stop("region names not compatible")

  shp$coef = coefs

  plt = ggplot(data = shp) +
    geom_sf(aes(fill = coef), color = NA) +
    scale_fill_viridis_c(name = expression(f[GMRF]), limits = .limscol) +
    labs(x = "Longitude", y = "Latitude")

  if (!is.null(.title)) plt = plt + ggtitle(.title) + theme(plot.title = element_text(hjust = 0.5))

  return(plt)
}
