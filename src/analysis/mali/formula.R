# formulas
#
#
#

frml_terms = c("urban", "climate", "elev", "lstday", "lstnight", "ndvi", "evi", "precip", "log_pop", "lon", "lat")


# linear for glmboost
frml_linear = list(
  mu = pos ~ intercept + urban + climate + elev + lstday + lstnight + ndvi + evi + precip + log_pop + lon + lat,
  sigma = pos ~ intercept + urban
)


# linear with spatial effect
frml_linearwspatial = list(
  mu = pos ~ bols(urban) + bols(climate) + bols(elev) + bols(lstday) + bols(lstnight) + bols(ndvi) +
    bols(evi) + bols(precip) + bols(log_pop) + bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE),

  sigma = pos ~ bols(urban)
)


# add additional interactions with climate to each base learner
frml_linearwspatialbyclimate = list(
  mu = pos ~ bols(urban) + bols(climate) + bols(elev) + bols(lstday) + bols(lstnight) + bols(ndvi) +
    bols(evi) + bols(precip) + bols(log_pop) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE) +

    bols(urban, by = climate) + bols(elev, by = climate) + bols(lstday, by = climate) + bols(lstnight, by = climate) +
    bols(ndvi, by = climate) + bols(evi, by = climate) + bols(precip, by = climate) + bols(log_pop, by = climate) +

    bols(lon, by = climate) + bols(lat, by = climate) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1, center = TRUE, by = climate),

  sigma = pos ~ bols(urban)
)


# smooth effects for continuous and additional spatial smooth effects
frml_base = list(
  mu = pos ~ bols(urban) + bols(elev) + bols(lstday) + bols(lstnight) + bols(ndvi) +
    bols(evi) + bols(precip) + bols(log_pop) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE) +

    bbs(elev, center = TRUE, df = 1L) + bbs(lstday, center = TRUE, df = 1L) +
    bbs(lstnight, center = TRUE, df = 1L) + bbs(ndvi, center = TRUE, df = 1L) +
    bbs(evi, center = TRUE, df = 1L) + bbs(precip, center = TRUE, df = 1L) +
    bbs(log_pop, center = TRUE, df = 1L),

  sigma = pos ~ bols(urban)
)


# space-varying coefficient models (with centered spatial effects)
frml_spacevarying = list(
  mu = pos ~ bols(urban) + bols(climate) + bols(elev) + bols(lstday) + bols(lstnight) + bols(ndvi) +
    bols(evi) + bols(precip) + bols(log_pop) + bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE) +

    bspatial(lon, lat, by = urban, df = 1L, knots = 12L, center = TRUE) +
    bspatial(lon, lat, by = elev, df = 1L, knots = 12L, center = TRUE) +
    bspatial(lon, lat, by = lstday, df = 1L, knots = 12L, center = TRUE) +
    bspatial(lon, lat, by = lstnight, df = 1L, knots = 12L, center = TRUE) +
    bspatial(lon, lat, by = ndvi, df = 1L, knots = 12L, center = TRUE) +
    bspatial(lon, lat, by = evi, df = 1L, knots = 12L, center = TRUE) +
    bspatial(lon, lat, by = precip, df = 1L, knots = 12L, center = TRUE) +
    bspatial(lon, lat, by = log_pop, df = 1L, knots = 12L, center = TRUE),

  sigma = pos ~ bols(urban)
)
