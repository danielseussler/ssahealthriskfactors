# test the methodology as in Paige et al. 2022 to create a 'true' sampling frame
#
#
#

library(here)
library(sf)
library(h3)
library(terra)
library(ggplot2)
library(data.table)

set.seed(seed = 1)


# load spatial data
shp = readRDS(file = here("data", "processed", "mali", "dhsregions.rds"))
plot(st_geometry(shp$admin2))



# load population data from Gridded Population of the World (GPW), v4 density and WPP adjusted counts
# alternative source, GHSL data
pop1 = geodata::population(year = 2020, res = 2.5, path = here("data", "rasterfiles", "geodata"))
pop1 = crop(x = pop1, y = shp$admin0)
pop1 = mask(x = pop1, mask = shp$admin0)
plot(pop)


pop2 = rast(x = here("data/raw/GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0/GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
pop2 = crop(x = pop2, y = st_transform(shp$admin0, crs = "+proj=moll"))
pop2 = project(x = pop2, y = "epsg:4326", method = "bilinear", threads = 10)
pop2 = mask(x = pop2, mask = shp$admin0)
plot(log(pop2))


pop3 = rast(x = here("data/raw/population/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"))
pop3 = crop(x = pop3, y = shp$admin0)
pop3 = mask(x = pop3, mask = shp$admin0)
plot(log(pop3))



# GADM and DHS have two different naming styles, construct translation
name_match = data.frame(
  region = c("Bamako", "Gao", "Kayes", "Kidal", "Koulikoro", "Mopti", "Ségou", "Sikasso", "Tombouctou"),
  gadm_name = c("Bamako", "Gao", "Kayes", "Kidal", "Koulikoro", "Mopti", "Ségou", "Sikasso", "Timbuktu")
)



# create grid of population and add admin 1 regions
# create rural/urban indicator by threshold, 22.6% of the households are in urban areas

tmp = data.frame(h3_index = polyfill(polygon = st_union(shp$admin0), res = 8))

coords = data.frame(h3_to_geo(tmp$h3_index))
coords$h3_index = tmp$h3_index
coords = st_join(st_as_sf(coords, coords = c("lng", "lat"), crs = st_crs(4326)), shp$admin1, join = st_within, left = FALSE)

grid = data.table(h3_index = coords$h3_index, gadm_name = coords$NAME_1, lon = sf::st_coordinates(coords)[,1], lat = sf::st_coordinates(coords)[,2])
grid$region = name_match$region[match(grid$gadm_name, name_match$gadm_name)]
grid = grid[!duplicated(grid), ]


tmp = extract(x = pop1, y = grid[, c("lon", "lat")], method = "bilinear", ID = FALSE)
grid$pop1 = tmp$population_density
grid$pop1[is.na(grid$pop1)]  = 0

tmp = extract(x = pop2, y = grid[, c("lon", "lat")], method = "bilinear", ID = FALSE)
grid$pop2 = tmp$GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0
grid$pop2[is.na(grid$pop2)]  = 0

tmp = extract(x = pop3, y = grid[, c("lon", "lat")], method = "bilinear", ID = FALSE)
grid$pop3 = tmp$gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec
grid$pop3[is.na(grid$pop3)]  = 0

tmp = exactextractr::exact_extract(x = pop3, y = h3_to_geo_boundary_sf(grid$h3_index), fun = 'weighted_mean', weights = "area")
grid$pop4 = tmp
grid$pop4[is.na(grid$pop4)]  = 0



# calculate threshold value for each population (density) source and
# construct urbanisation variable for this grid cell
pop_sorted = sort(grid$pop1)
pop_cumsum = cumsum(pop_sorted) / sum(pop_sorted)
threshold.val1 = max(pop_sorted[pop_cumsum <= 1-0.226])

grid$urban1 = grid$pop1 > threshold.val1
grid$urban1 = factor(grid$urban1, levels = c(FALSE, TRUE), labels = c("rural", "urban"))
sum(grid$pop1[grid$urban1 == "urban"]) / sum(grid$pop1) # check if urban proportion was correct: Yes



pop_sorted = sort(grid$pop2)
pop_cumsum = cumsum(pop_sorted) / sum(pop_sorted)
threshold.val2 = max(pop_sorted[pop_cumsum <= 1-0.226])

grid$urban2 = grid$pop2 > threshold.val2
grid$urban2 = factor(grid$urban2, levels = c(FALSE, TRUE), labels = c("rural", "urban"))
sum(grid$pop1[grid$urban2 == "urban"]) / sum(grid$pop1) # check if urban proportion was correct: Yes
sum(grid$pop2[grid$urban2 == "urban"]) / sum(grid$pop2)



pop_sorted= sort(grid$pop3)
pop_cumsum = cumsum(pop_sorted) / sum(pop_sorted)
threshold.val3 = max(pop_sorted[pop_cumsum <= 1-0.226])

grid$urban3 = grid$pop3 > threshold.val3
grid$urban3 = factor(grid$urban3, levels = c(FALSE, TRUE), labels = c("rural", "urban"))
sum(grid$pop3[grid$urban3 == "urban"]) / sum(grid$pop3) # check if urban proportion was correct: Yes


pop_sorted= sort(grid$pop4)
pop_cumsum = cumsum(pop_sorted) / sum(pop_sorted)
threshold.val4 = max(pop_sorted[pop_cumsum <= 1-0.226])

grid$urban4 = grid$pop4 > threshold.val4
grid$urban4 = factor(grid$urban4, levels = c(FALSE, TRUE), labels = c("rural", "urban"))
sum(grid$pop4[grid$urban4 == "urban"]) / sum(grid$pop4) # check if urban proportion was correct: Yes



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

  tmp1 = subset(grid, region == sframe[i, "region"] & urban1 == sframe[i, "urban"])
  tmp2 = subset(grid, region == sframe[i, "region"] & urban2 == sframe[i, "urban"])
  tmp3 = subset(grid, region == sframe[i, "region"] & urban3 == sframe[i, "urban"])
  tmp4 = subset(grid, region == sframe[i, "region"] & urban4 == sframe[i, "urban"])

  print(paste(sframe[i, ]))
  print(paste(nrow(tmp1), nrow(tmp2), nrow(tmp3), nrow(tmp4)))
  cat("\n")

  #sampled_idx = sample(x = tmp$h3_index, size = numEAs, replace = FALSE, prob = tmp$pop / sum(tmp$pop))
  #sampled_idx = sample(x = tmp$h3_index, size = numEAs, replace = FALSE, prob = tmp$ghsl / sum(tmp$ghsl))
  #sampled_idx = sample(x = tmp$h3_index, size = numEAs, replace = FALSE, prob = tmp$gpwp / sum(tmp$gpwp))

  #grid[grid$h3_index %in% sampled_idx, "selected"] = 1
}


# doesnt' work either, same problems

# output:
# [1] "Kayes" "urban" "294"
# [1] "91 63 60 61"
#
# [1] "Kayes" "rural" "2629"
# [1] "184260 184288 184291 184290"
#
# [1] "Koulikoro" "urban"     "125"
# [1] "776 256 518 511"
#
# [1] "Koulikoro" "rural"     "3289"
# [1] "142359 142879 142617 142624"
#
# [1] "Sikasso" "urban"   "406"
# [1] "0 146 0 0"
#
# [1] "Sikasso" "rural"   "3142"
# [1] "120105 119959 120105 120105"
#
# [1] "Ségou" "urban" "176"
# [1] "93 91 56 57"
#
# [1] "Ségou" "rural" "3150"
# [1] "99329 99331 99366 99365"
#
# [1] "Mopti" "urban" "197"
# [1] "57 53 59 60"
#
# [1] "Mopti" "rural" "3079"
# [1] "131422 131426 131420 131419"
#
# [1] "Tombouctou" "urban"      "97"
# [1] "8 27 29 28"
#
# [1] "Tombouctou" "rural"      "1010"
# [1] "742735 742716 742714 742715"
#
# [1] "Gao"   "urban" "146"
# [1] "46 25 44 44"
#
# [1] "Gao"   "rural" "668"
# [1] "254673 254694 254675 254675"
#
# [1] "Kidal" "urban" "48"
# [1] "0 7 0 0"
#
# [1] "Kidal" "rural" "96"
# [1] "204664 204657 204664 204664"
#
# [1] "Bamako" "urban"  "1538"
# [1] "400 299 334 330"

