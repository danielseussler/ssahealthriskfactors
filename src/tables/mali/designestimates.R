# table
# region/urban disaggregation of design based estimates of malaria prevalence
#
#

library(here)
library(dplyr)
library(survey)
require(rdhs)


# Coverage:
# Population base: De facto children age 6-59 months (PR file)
# Time period: Current status at the time the blood sample was taken
# Numerators:
# 1) Number of de facto children tested using RDT who are positive for malaria (hml35 = 1)
# 2) Number of de facto children tested using microscopy who are positive for malaria (hml32 = 1)
# Denominators:
# 1) Number of de facto children tested using RDT (hv042 = 1 & hv103 = 1 & hc1 in 6:59 & hml35 in 0,1)
# 2) Number of de facto children tested using microscopy (hv042 = 1 & hv103 = 1 & hc1 in 6:59 & hml32 in 0,1,6)

indicators = rdhs::dhs_indicators()
cindicator = indicators[grepl("malaria", Label)]
statcomp = rdhs::dhs_data(indicatorIds = "ML_CMLT_C_RDT", countryIds = "ML") # FIXME
statcomp[, .(Indicator, CountryName, SurveyYear, Value, DenominatorWeighted)]



# survey data and extract data
variables = c("hv000", "hv001", "hv002", "hv005", "hv023", "hv024", "hv025", "hml32", "hml35", "hv042", "hv103", "hc1")

sv = readRDS(file = here("data", "raw", "rdhs", "MLPR81FL.rds"))
sv = sv[variables]
sv = subset(sv, hv042 == 1 & hv103 == 1 & hc1 %in% 6:59 & hml35 %in% 0:1)
sv = mutate(sv, hv024 = labelled::to_character(hv024), hv025 = labelled::to_character(hv025))



# survey design - two stage sampling, clusters are first stage psu (no sampling weights)
# second stage is inverse proportional probability sampling pps
# national estimates can also be extracted by DHS survey package
# weights are household level / 1e6 by DHS Guide

design = svydesign(ids = ~hv001 + hv002, strata = ~hv023, data = sv, weights = ~hv005)

results = vector(mode = "list")
results[[1]] = svyby(formula = ~hml35, by = ~hv024, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")
results[[2]] = svyby(formula = ~hml35, by = ~ hv024 + hv025, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")
results[[3]] = svyby(formula = ~hml35, by = ~hv025, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")
results[[4]] = svyby(formula = ~hml35, by = ~hv000, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")

results = lapply(results, as.data.frame)
results = do.call(bind_rows, results)

results$Region = stringr::str_to_title(results$hv024)
results$urban = stringr::str_to_title(results$hv025)
results$Region[is.na(results$Region)] = "Mali"
results$urban[is.na(results$urban)] = "Total"

results = tidyr::pivot_wider(results, id_cols = "Region", names_from = "urban", values_from = "hml35")
results = results[, c("Region", "Urban", "Rural", "Total")]

print(xtable::xtable(results, type = "latex", digits = 3), include.rownames = FALSE)

