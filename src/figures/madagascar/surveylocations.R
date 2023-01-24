# country figures of admin1 (as designated by DHS) and DHS cluster locations
#
#
#

library(sf)
library(ggplot2)
library(viridis)

shp = readRDS(file = file.path("data", "processed", "madagascar", "dhsboundaries.rds"))
loc = readRDS(file = file.path("data", "raw", "rdhs",  "MDGE81FL.rds")) |>
  st_as_sf() |>
  subset(LONGNUM != 0)

plt = ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = loc, size = 0.7) +
  theme_void()

ggsave(
  plot = plt
  , filename = "madagascar_clusterlocations.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 200
  , units = "mm", device = png
)
