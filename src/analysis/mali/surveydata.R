# data
# compile cluster level prevalence of malaria
#
#

library(here)
library(dplyr)
library(sp)
library(sf)
require(rdhs)

sv = readRDS(file = here("data", "raw", "rdhs", "MLPR81FL.rds"))
shp = readRDS(file = here("data", "processed", "mali", "dhsboundaries.rds"))

# Coverage:
# Population base: De facto children age 6-59 months (PR file)
# Time period: Current status at the time the blood sample was taken
# Numerators:
# 1) Number of de facto children tested using RDT who are positive for malaria (hml35 = 1)
# 2) Number of de facto children tested using microscopy who are positive for malaria (hml32 = 1)
# Denominators:
# 1) Number of de facto children tested using RDT (hv042 = 1 & hv103 = 1 & hc1 in 6:59 & hml35 in 0,1)
# 2) Number of de facto children tested using microscopy (hv042 = 1 & hv103 = 1 & hc1 in 6:59 & hml32 in 0,1,6)

# create neighbourhood matrix
nb = bamlss::neighbormatrix(x = as(shp, "Spatial"), names = "DHSREGEN")

# overview of questionnaire
label = rdhs::data_and_labels(sv)$variable_names
write.csv2(cbind("#", label), file = here("src", "analysis", "mali", "questions.csv"))

# extract data
variables = c("hv001", "hv002", "hv005", "hv023", "hv024", "hv025", "hml32", "hml35", "hv042", "hv103", "hc1")
questions = rdhs::search_variables(dataset_filenames = "MLPR81FL", variables = variables)

sv = rdhs::extract_dhs(questions = questions, add_geo = TRUE)
sv = sv[[1]]

# restrict to applicable observations
sv$hv024 = labelled::to_factor(sv$hv024)
levels(sv$hv024) = colnames(nb)
sv$hv025 = labelled::to_factor(sv$hv025)
sv$hv023 = labelled::to_factor(sv$hv023)
sv = haven::zap_labels(sv)

sv = subset(sv, LATNUM != 0)
sv = subset(sv, hv042 == 1 & hv103 == 1 & hc1 %in% 6:59 & hml35 %in% c(0, 1))
sv = select(sv, cluster = hv001, strata = hv023, region = hv024, urban = hv025, mtest = hml35, lon = LONGNUM, lat = LATNUM)

# aggregate to cluster level and add h3 index
cl = group_by(sv, cluster, strata, region, urban, lon, lat)
cl = summarise(cl, n = n(), npos = sum(mtest), nneg = n - npos, intercept = 1, .groups = "drop")
cl$h3_index = h3::geo_to_h3(c(cl$lat, cl$lon), res = 7L)

sv$urban = forcats::fct_relevel(sv$urban, "rural")
cl$urban = forcats::fct_relevel(cl$urban, "rural")

save(sv, cl, nb, file = here("data", "processed", "mali", "surveydata.rda"))
