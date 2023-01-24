# data
# load all required dhs files for later use and access them directly via filename
#
#

library(here)
library(data.table)

source(file = here("src", "configs", "rdhs.R"))

mycountries = c("Madagascar", "Mali")
countries = rdhs::dhs_countries()
cselected = countries[CountryName %in% mycountries, DHS_CountryCode]

surveychar = rdhs::dhs_survey_characteristics()
surveychar[grepl("Anthropometry", SurveyCharacteristicName, ignore.case = TRUE)]

# fileType is PR Household Member Recode KR Children's Recode GE Geographic Data
# https://dhsprogram.com/data/File-Types-and-Names.cfm
surveys = rdhs::dhs_surveys(
  countryIds = cselected
  , surveyType = c("DHS", "MIS")
  , surveyYearStart = 2015
  , surveyYearEnd = 2022
)

surveys[, .(SurveyId, CountryName, SurveyYear, NumberOfWomen, SurveyNum, FieldworkEnd)]
surveys = surveys[SurveyId %in% c("MD2021DHS", "ML2021MIS")]

datasets = rdhs::dhs_datasets(surveyIds = surveys$SurveyId, fileType = c("PR", "KR", "GE"), fileFormat = "flat")
datasets[, .(SurveyId, FileType, SurveyNum, FileDateLastModified, FileName)]

rdhs::get_datasets(
  dataset_filenames = datasets$FileName
  , output_dir_root = here("data", "raw", "rdhs")
  , clear_cache = TRUE
)

rdhs::get_downloaded_datasets()
