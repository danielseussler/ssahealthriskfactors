# script
# download additional data files for the analyses from public sites
# references are provided in the manuscript
#

library(sp)
library(sf)
library(purrr)

options(timeout = 500)


# GHS Population
tf = tempfile()
url = list(
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2022A/GHS_POP_E2020_GLOBE_R2022A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0_R8_C17.zip"
  , "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2022A/GHS_POP_E2020_GLOBE_R2022A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0_R8_C18.zip"
  , "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2022A/GHS_POP_E2020_GLOBE_R2022A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0_R8_C19.zip"
  , "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2022A/GHS_POP_E2020_GLOBE_R2022A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0_R7_C17.zip"
  , "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2022A/GHS_POP_E2020_GLOBE_R2022A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0_R7_C18.zip"
  , "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2022A/GHS_POP_E2020_GLOBE_R2022A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0_R7_C19.zip"
)

walk(
  .x = url,
  .f = function(link) {
    download.file(url = link, destfile = tf)
    unzip(tf, exdir = file.path("data", "raw", "GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0"))
  }
)


# GSH Urban
url = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_SMOD_GLOBE_R2022A/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000/V1-0/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0.zip"
download.file(url = url, destfile = tf, mode = "wb")
unzip(tf, exdir = file.path("data", "raw", "GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0"))


# GSH Population 1km
url = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2022A/GHS_POP_E2020_GLOBE_R2022A_54009_1000/V1-0/GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0.zip"
download.file(url = url, destfile = tf, mode = "wb")
unzip(tf, exdir = file.path("data", "raw", "GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0"))


# Köppen-Geiger climate classification
url = "https://ndownloader.figstatic.com/files/12407516"
download.file(url = url, destfile = tf, mode = "wb")
unzip(tf, exdir = file.path("data", "raw", "Beck_KG_V1"))


# FEWSNET
tf = tempfile()
url = "https://fdw.fews.net/api/ipcpackage/?country_code=ET&collection_date=2019-02-01"
download.file(url = url, destfile = tf, mode = "wb")
unzip(tf, exdir = file.path("data", "raw", "FEWSNET"))

url = "https://fdw.fews.net/api/ipcpackage/?country_code=MG&collection_date=2021-06-01"
download.file(url = url, destfile = tf, mode = "wb")
unzip(tf, exdir = file.path("data", "raw", "FEWSNET"))
