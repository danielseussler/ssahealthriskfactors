#
#
#
#

library(sf)
library(terra)
library(ggplot2)

# load data
load(file = file.path("data", "processed", "mali", "surveydata.rda"))
mali_shp = readRDS(file = file.path("data", "processed", "mali", "dhsboundaries.rds"))
mali_gadm = readRDS(file = file.path("data", "processed", "mali", "regions.rds"))

# prepare data
cl_sf = st_as_sf(cl, coords = c("lon", "lat"), crs = st_crs(4326))
cl_sf$prev = cl_sf$npos / cl_sf$n

climzone = rast(x = file.path("data", "raw", "Beck_KG_V1", "Beck_KG_V1_present_0p083.tif"))
climzone = crop(x = climzone, y = mali_shp)
climzone = mask(x = climzone, mask = mali_shp)
climzone = as.factor(climzone)

val = data.frame(ID = c(3, 4, 6), label = c("Tropical, savannah", "Arid, desert, hot", "Arid, steppe, hot"))
climzone = categories(climzone, value = val, index = 2)
climzone_df = as.data.frame(climzone, xy = T)

# create line for 20° latitude
lat20 = data.frame(x = c(-7, 4),  y = c(20, 20))

# create plot of cluster level prevalences
plt1 = ggplot() +
  geom_sf(data = mali_shp) +
  geom_sf(data = cl_sf, aes(color = prev), shape = 20, size = 2) +
  scale_color_continuous(
    type = "viridis"
    , option = "inferno"
    , name = "Prevalence"
    , limits = c(0, 1)
    , guide = guide_colorbar(
      direction = "horizontal"
      , barheight = unit(2, units = "mm")
      , barwidth = unit(50, units = "mm")
      , label.hjust = 0
      , title.position = "top"
      , title.hjust = 0.5)
  ) +
  theme_void() +
  theme(legend.position = "bottom")

ggsave(
  plot = plt1
  , filename = "mali_clusterprevalence.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 200
  , units = "mm", device = png
)

# create plot of climate zones and cluster locations
plt2 = ggplot() +
  geom_raster(data = climzone_df, mapping = aes(x = x, y = y, fill = label)) +
  scale_fill_manual(values = c("#46AAFA", "#FF0000", "#F5A500"), na.value = "transparent", na.translate = F, name = "") +
  geom_sf(data = mali_shp, fill = NA) +
  geom_sf(data = cl_sf, size = 0.7) +
  geom_line(data = lat20, mapping = aes(x = x, y = y), color = "grey") +
  geom_text(data = data.frame(x = 5, y = 20, text = "20°N"), mapping = aes(x, y, label = text)) +
  theme_void() +
  theme(legend.position = "bottom")

ggsave(
  plot = plt2
  , filename = "mali_climatezones.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 200
  , units = "mm", device = png
)
