# data
# I create zonal stats for h3 (centroid) locations of regular grid and survey locations
# used to predict malaria risk geographically disaggregated
#

library(terra)
library(sf)
library(h3)
library(data.table)
library(exactextractr)

# load data and create regular grid of locations
dhs_shp = readRDS(file = file.path("data", "processed", "mali", "dhsboundaries.rds"))
gadm_shp = readRDS(file = file.path("data", "processed", "mali", "regions.rds"))[]
indices = polyfill(st_union(dhs_shp), res = 7L) # average area of hexagon is 5.161293360
coords = h3_to_geo(indices)

# extract admin 1 region for each
coords_sf = st_as_sf(data.frame(coords), coords = c("lng", "lat"), crs = st_crs(dhs_shp))

admin1_intersect = as.integer(st_intersects(coords_sf$geometry, dhs_shp))
admin2_intersect = st_intersects(coords_sf$geometry, gadm_shp$admin2)
admin2_intersect[as.integer(lapply(admin2_intersect, length)) > 1] = NA # remove multiple matches, are only 2
admin2_intersect = as.integer(admin2_intersect)

grid = data.table(h3_index = indices, lon = coords[, 2L], lat = coords[, 1L])
grid[, admin1 := ifelse(is.na(admin1_intersect), NA, dhs_shp$DHSREGEN[admin1_intersect])]
grid[, admin2 := ifelse(is.na(admin2_intersect), NA, gadm_shp$admin2$NAME_2[admin2_intersect])]

# load grid clusters
cloc = readRDS(file = file.path("data", "raw", "rdhs", "MLGE81FL.rds"))
cloc = cloc |>
  st_as_sf() |>
  st_drop_geometry() |>
  as.data.table()
cloc = dplyr::select(cloc, cluster = DHSCLUST, urban = URBAN_RURA, lon = LONGNUM, lat = LATNUM)
cloc = dplyr::mutate(cloc, urban = factor(urban, levels = c("R", "U"), labels = c("rural", "urban")))
cloc = subset(cloc, lon != 0)

# elevation
elev = geodata::elevation_30s(country = "Mali", path = file.path("data", "raw", "geodata"))
plot(elev)

# population from global human settlement layers
pop = rast(x = file.path("data", "raw", "GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0", "GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
pop = crop(x = pop, y = st_transform(dhs_shp, crs = "+proj=moll"))
plot(log(mask(x = pop, mask = st_transform(dhs_shp, crs = "+proj=moll"))))

# self note: this is problematic if country total is to be kept equal
# only use it for cluster level, non-projected for grid-level
pop_proj = project(x = pop, y = "epsg:4326", method = "bilinear")

# global climate zones by köppen-geiger
climzone = rast(x = file.path("data", "raw", "Beck_KG_V1", "Beck_KG_V1_present_0p083.tif"))
climzone = crop(x = climzone, y = dhs_shp)

# climzone = as.factor(climzone)
# val = data.frame(ID = c(3, 4, 6), label = c("Tropical, savannah", "Arid, desert, hot", "Arid, steppe, hot"))
# climzone = categories(climzone, value = val, index = 2)
plot(mask(x = climzone, mask = dhs_shp))

# urbanicity from the global human settlement layers
urban = rast(x = file.path("data", "raw", "GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
urban = crop(x = urban, y = st_transform(dhs_shp, crs = "+proj=moll"))
urban = project(x = urban, y = "epsg:4326", method = "near") # here it should not be that problematic as with the population counts
plot(urban)

# load google earth engine raster files
fpath = list.files(path = file.path("data", "raw", "earthengine"), full.names = TRUE, pattern = "Mali")
ee_files = rast(x = fpath)
plot(mask(x = ee_files, mask = dhs_shp))

# extract data for the grid
grid$elev = extract(elev, grid[, .(lon, lat)], method = "bilinear", ID = FALSE)
grid$pop = exact_extract(x = pop, y = st_transform(h3_to_geo_boundary_sf(indices), crs = "+proj=moll"), fun = "sum")

grid$urban = extract(urban, grid[, .(lon, lat)], method = "simple", ID = FALSE)
grid$urban = grid$urban > 20L
grid$urban = factor(grid$urban, levels = c(FALSE, TRUE), labels = c("rural", "urban"))

grid$climate = extract(climzone, grid[, .(lon, lat)], ID = FALSE)
grid$climate = factor(grid$climate, levels = c(3L, 4L, 6L), labels = c("Tropical, savannah", "Arid, desert, hot", "Arid, steppe, hot"))

tmp = extract(x = ee_files, grid[, .(lon, lat)], method = "bilinear", ID = FALSE)
colnames(tmp) = c("evi", "lstday", "lstnight", "ndvi", "precip", "water_mask")
grid = cbind(grid, tmp)

grid = grid[complete.cases(grid)]

# extract data for the cluster locations
cloc$pop = extract(pop_proj, cloc[, .(lon, lat)], method = "bilinear", ID = FALSE)
cloc$elev = extract(elev, cloc[, .(lon, lat)], method = "bilinear", ID = FALSE)

cloc$climate = extract(climzone, cloc[, .(lon, lat)], ID = FALSE)
cloc$climate = factor(cloc$climate, levels = c(3L, 4L, 6L), labels = c("Tropical, savannah", "Arid, desert, hot", "Arid, steppe, hot"))

tmp = extract(x = ee_files, cloc[, .(lon, lat)], method = "bilinear", ID = FALSE)
colnames(tmp) = c("evi", "lstday", "lstnight", "ndvi", "precip", "water_mask")
cloc = cbind(cloc, tmp)

# alternative construct urban / rural indicator, I finally use the SMOD GHSL
# because of the (slighlty) higher share of regions classified as urban

# pop.sorted = sort(grid$pop)
# pop.cumsum = cumsum(pop.sorted) / sum(pop.sorted)
# pop.threshold = max(pop.sorted[pop.cumsum <= 1-0.226])
#
# grid$urban = grid$pop > pop.threshold
# grid$urban = factor(grid$urban, levels = c(FALSE, TRUE), labels = c("rural", "urban"))
# sum(grid$pop[grid$urban == "urban"]) / sum(grid$pop) # check if urban proportion was correct: Yes

# save data
save(cloc, grid, file = file.path("data", "processed", "mali", "geodata.rda"))
