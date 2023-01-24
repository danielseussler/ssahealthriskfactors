# climatezones in madagascar
# note: not used in the report
#
#

library(sf)
library(terra)
library(ggplot2)

# data
shp = readRDS(file = file.path("data", "processed", "madagascar", "regions.rds"))

climzone = rast(x = file.path("data", "raw", "Beck_KG_V1", "Beck_KG_V1_present_0p083.tif"))
climzone = crop(x = climzone, y = shp$admin0)
climzone = mask(x = climzone, mask = shp$admin0)
climzone = as.factor(climzone)

# plot
plot(climzone)
levels(climzone)
