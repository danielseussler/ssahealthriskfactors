# country figures of admin1 (as designated by DHS) and DHS cluster locations
#
#
#

library(sf)
library(ggplot2)
library(viridis)

# load data and shapefile
shp = readRDS(file = file.path("data", "processed", "mali", "dhsboundaries.rds"))
loc = readRDS(file = file.path("data", "raw", "rdhs",  "MLGE81FL.rds")) |>
  st_as_sf() |>
  subset(LONGNUM != 0)

# plot figure
plt = ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = loc, size = 0.7) +
  theme_void()

ggsave(
  plot = plt
  , filename = "mali_clusterlocations.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 200
  , units = "mm", device = png
)
