# formulas
#
#
#

frml_terms = c("cage", "csex", "ctwin", "cbord", "mbmi", "mage", "medu", "memployed", "mreligion", "nodead",
               "hmembers", "watersource", "sanitation", "wealth", "electricity", "radio", "television",
               "bicycle", "motorcycle", "car", "dhsregion", "urban", "healthaccess", "cityaccess",
               "fews")

# linear models w/ and w/o first order interactions
frml_linear = as.formula(paste("moderatelyf ~ ", paste(frml_terms, collapse = " + ")))

frml_linearplus = as.formula(paste(
  "moderatelyf ~ "
  , paste(frml_terms, collapse = " + ")
  , "+"
  , paste(combn(frml_terms, 2, FUN = paste, collapse = ":"), collapse = " + ")
))


# main model with spatial gmrf and smooth effects for continuous covariates
# smooth decomposition to allow unbiased effect selection
frml_base = moderatelyf ~ bols(cage) + bols(csex) + bols(ctwin) + bols(cbord) + bols(mbmi) + bols(mage) +
  bols(medu) + bols(memployed) + bols(mreligion) + bols(nodead) + bols(hmembers) + bols(watersource) +
  bols(sanitation) + bols(wealth) + bols(electricity) + bols(radio) + bols(television) + bols(bicycle) +
  bols(motorcycle) + bols(car) + bols(urban) + bols(healthaccess) + bols(cityaccess) + bols(fews) +

  bbs(cage, center = TRUE, df = 1) + bbs(mage, center = TRUE, df = 1) + bbs(mbmi, center = TRUE, df = 1) +
  bbs(medu, center = TRUE, df = 1) + bbs(hmembers, center = TRUE, df = 1) + bbs(healthaccess, center = TRUE, df = 1) +
  bbs(cityaccess, center = TRUE, df = 1) +

  bmrf(dhsregion, bnd = neighbourhood)


frml_basebycsex = moderatelyf ~ bols(cage) + bols(csex) + bols(ctwin) + bols(cbord) + bols(mbmi) + bols(mage) +
  bols(medu) + bols(memployed) + bols(mreligion) + bols(nodead) + bols(hmembers) + bols(watersource) +
  bols(sanitation) + bols(wealth) + bols(electricity) + bols(radio) + bols(television) + bols(bicycle) +
  bols(motorcycle) + bols(car) + bols(urban) + bols(healthaccess) + bols(cityaccess) + bols(fews) +

  bbs(cage, center = TRUE, df = 1) + bbs(mage, center = TRUE, df = 1) + bbs(mbmi, center = TRUE, df = 1) +
  bbs(medu, center = TRUE, df = 1) + bbs(hmembers, center = TRUE, df = 1) + bbs(healthaccess, center = TRUE, df = 1) +
  bbs(cityaccess, center = TRUE, df = 1) +

  bmrf(dhsregion, bnd = neighbourhood) +

  bols(cage, by = csex) + bols(ctwin, by = csex) + bols(cbord, by = csex) + bols(mbmi, by = csex) +
  bols(mage, by = csex) + bols(medu, by = csex) + bols(memployed, by = csex) + bols(mreligion, by = csex) +
  bols(nodead, by = csex) + bols(hmembers, by = csex) + bols(watersource, by = csex) +
  bols(sanitation, by = csex) + bols(wealth, by = csex) + bols(electricity, by = csex) +
  bols(radio, by = csex) + bols(television, by = csex) + bols(bicycle, by = csex) +
  bols(motorcycle, by = csex) + bols(car, by = csex) + bols(urban, by = csex) + bols(healthaccess, by = csex) +
  bols(cityaccess, by = csex) + bols(fews, by = csex) + bbs(cage, center = TRUE, df = 1, by = csex) +
  bbs(mage, center = TRUE, df = 1, by = csex) + bbs(mbmi, center = TRUE, df = 1, by = csex) +
  bbs(medu, center = TRUE, df = 1, by = csex) + bbs(hmembers, center = TRUE, df = 1, by = csex) +
  bbs(healthaccess, center = TRUE, df = 1, by = csex) + bbs(cityaccess, center = TRUE, df = 1, by = csex) +
  bmrf(dhsregion, bnd = neighbourhood, by = csex)


frml_basebyurban = moderatelyf ~ bols(cage) + bols(csex) + bols(ctwin) + bols(cbord) + bols(mbmi) + bols(mage) +
  bols(medu) + bols(memployed) + bols(mreligion) + bols(nodead) + bols(hmembers) + bols(watersource) +
  bols(sanitation) + bols(wealth) + bols(electricity) + bols(radio) + bols(television) + bols(bicycle) +
  bols(motorcycle) + bols(car) + bols(urban) + bols(healthaccess) + bols(cityaccess) + bols(fews) +

  bbs(cage, center = TRUE, df = 1) + bbs(mage, center = TRUE, df = 1) + bbs(mbmi, center = TRUE, df = 1) +
  bbs(medu, center = TRUE, df = 1) + bbs(hmembers, center = TRUE, df = 1) + bbs(healthaccess, center = TRUE, df = 1) +
  bbs(cityaccess, center = TRUE, df = 1) +

  bmrf(dhsregion, bnd = neighbourhood) +

  bols(cage, by = urban) + bols(csex, by = urban) + bols(ctwin, by = urban) + bols(cbord, by = urban) + bols(mbmi, by = urban) +
  bols(mage, by = urban) + bols(medu, by = urban) + bols(memployed, by = urban) + bols(mreligion, by = urban) +
  bols(nodead, by = urban) + bols(hmembers, by = urban) + bols(watersource, by = urban) +
  bols(sanitation, by = urban) + bols(wealth, by = urban) + bols(electricity, by = urban) +
  bols(radio, by = urban) + bols(television, by = urban) + bols(bicycle, by = urban) +
  bols(motorcycle, by = urban) + bols(car, by = urban) + bols(healthaccess, by = urban) +
  bols(cityaccess, by = urban) + bols(fews, by = urban) + bbs(cage, center = TRUE, df = 1, by = urban) +
  bbs(mage, center = TRUE, df = 1, by = urban) + bbs(mbmi, center = TRUE, df = 1, by = urban) +
  bbs(medu, center = TRUE, df = 1, by = urban) + bbs(hmembers, center = TRUE, df = 1, by = urban) +
  bbs(healthaccess, center = TRUE, df = 1, by = urban) + bbs(cityaccess, center = TRUE, df = 1, by = urban) +
  bmrf(dhsregion, bnd = neighbourhood, by = urban)


# for boosted trees (blackboost)
frml_tree = frml_linear
