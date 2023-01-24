# config
# login for dhs api with rdhs package
#
#

rdhs::set_rdhs_config(
  email = Sys.getenv("email")
  , project = Sys.getenv("project")
  , timeout = 100
  , verbose_download = TRUE
  , data_frame = "data.table::as.data.table"
)
