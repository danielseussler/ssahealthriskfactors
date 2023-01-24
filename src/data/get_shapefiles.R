# data
# shapefiles, source: GADM and DHS spatial data repositories
#
#

library(here)
library(purrr)
library(sf)
library(geodata)

madagascar = map(
  .x = c(0, 1, 2, 3),
  .f = ~ gadm(
    country = "Madagascar"
    , level = .
    , path = tempdir()
    , version = "4.1"
    , resolution = 2
  ) |>
    st_as_sf() |>
    st_transform(crs = 4326)
) |> setNames(c("admin0", "admin1", "admin2", "admin3"))


mali = map(
  .x = c(0, 1, 2),
  .f = ~ gadm(
    country = "Mali"
    , level = .
    , path = tempdir()
    , version = "4.1"
    , resolution = 2
  ) |>
    st_as_sf() |>
    st_transform(crs = 4326)
) |> setNames(c("admin0", "admin1", "admin2"))

saveRDS(object = madagascar, file = here("data", "processed", "madagascar", "regions.rds"))
saveRDS(object = mali, file = here("data", "processed", "mali", "regions.rds"))


# survey regions from spatial data repository
madagascar = read_sf(here("data/raw/sdr_subnational_boundaries_2022-11-26_madagascar_2021_dhs/sdr_subnational_boundaries.shp"))
mali = read_sf(here("data/raw/sdr_subnational_boundaries_2022-11-26_mali_2021_mis/sdr_subnational_boundaries.shp"))

saveRDS(object = madagascar, file = here("data", "processed", "madagascar", "dhsboundaries.rds"))
saveRDS(object = mali, file = here("data", "processed", "mali", "dhsboundaries.rds"))
