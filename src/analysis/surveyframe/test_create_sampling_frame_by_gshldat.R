#
#
#
#

library(here)
library(sf)
library(h3)
library(terra)
library(data.table)


# load spatial data
shp = readRDS(file = here("data", "processed", "mali", "regions.rds"))
plot(st_geometry(shp$admin2))


#dat = read_sf(here("data/ghslfiles/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg"))
urban = rast(here("data/raw/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
urban = crop(x = urban, y = st_transform(shp$admin0, crs = "+proj=moll"))
urban = project(x = urban, y = "epsg:4326", method = "near")


# GADM and DHS have two different naming styles, construct translation
name_match = data.frame(
  region = c("Bamako", "Gao", "Kayes", "Kidal", "Koulikoro", "Mopti", "Ségou", "Sikasso", "Tombouctou"),
  gadm_name = c("Bamako", "Gao", "Kayes", "Kidal", "Koulikoro", "Mopti", "Ségou", "Sikasso", "Timbuktu")
)


# create grid of population and add admin 1 regions
# 30: URBAN CENTRE GRID CELL
# 23: DENSE URBAN CLUSTER GRID CELL
# 22: SEMI-DENSE URBAN CLUSTER GRID CELL
# 21: SUBURBAN OR PERI-URBAN GRID CELL
# 13: RURAL CLUSTER GRID CELL
# 12: LOW DENSITY RURAL GRID CELL
# 11: VERY LOW DENSITY RURAL GRID CELL
# 10: WATER GRID CELL

tmp = data.frame(h3_index = polyfill(polygon = st_union(shp$admin0), res = 8))

coords = data.frame(h3_to_geo(tmp$h3_index))
coords$h3_index = tmp$h3_index
coords = st_join(st_as_sf(coords, coords = c("lng", "lat"), crs = st_crs(4326)), shp$admin1, join = st_within, left = FALSE)

grid = data.table(h3_index = coords$h3_index, gadm_name = coords$NAME_1, lon = sf::st_coordinates(coords)[,1], lat = sf::st_coordinates(coords)[,2])
grid$region = name_match$region[match(grid$gadm_name, name_match$gadm_name)]
grid = grid[!duplicated(grid), ]


grid$class = extract(x = urban, y = grid[, c("lon", "lat")], method = "simple", ID = FALSE)
grid$urban = grid$class > 20
grid$urban = factor(grid$urban, levels = c(FALSE, TRUE), labels = c("rural", "urban"))


pop = rast(x = here("data/rasterfiles/population/GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
pop = crop(x = pop, y = st_transform(shp$admin0, crs = "+proj=moll"))
pop = project(x = pop, y = "epsg:4326", method = "bilinear", threads = 10)
pop = mask(x = pop, mask = shp$admin0)

tmp = extract(x = pop, y = grid[, c("lon", "lat")], method = "bilinear", ID = FALSE)
grid$pop = tmp$GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0

grid = grid[complete.cases(grid)]


sum(grid$pop[grid$urban == "urban"]) / sum(grid$pop)



# draw our true sampling frame from the grid above
# sampling frame characteristics as employed in ET2019DHS
# draw for each strata the number as described in A2 of Report Appendix (source of file below)
sframe = data.frame(
  region = rep(c("Kayes", "Koulikoro", "Sikasso", "Ségou", "Mopti", "Tombouctou", "Gao", "Kidal", "Bamako"), each = 2),
  urban = rep(c("urban", "rural"), 9),
  value = c(294, 2629, 125, 3289, 406, 3142, 176, 3150, 197, 3079, 97, 1010, 146, 668, 48, 96, 1538, 0)
)

grid$selected = 0

for (i in 1:nrow(sframe)){ # FIXME

  tmp = subset(sframe, region == sframe[i, "region"] & urban == sframe[i, "urban"])
  numEAs = as.integer(tmp$value)
  if(numEAs == 0) next

  tmp = subset(grid, region == sframe[i, "region"] & urban == sframe[i, "urban"])

  print(paste(sframe[i, ]))
  print(nrow(tmp))
  cat("\n")

  #sampled_idx = sample(x = tmp$h3_index, size = numEAs, replace = FALSE, prob = tmp$pop / sum(tmp$pop))
  #sampled_idx = sample(x = tmp$h3_index, size = numEAs, replace = FALSE, prob = tmp$ghsl / sum(tmp$ghsl))
  #sampled_idx = sample(x = tmp$h3_index, size = numEAs, replace = FALSE, prob = tmp$gpwp / sum(tmp$gpwp))

  #grid[grid$h3_index %in% sampled_idx, "selected"] = 1
}
