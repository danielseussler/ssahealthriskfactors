# credit to https://www.jamesatkins.net/posts/progress-bar-in-purrr-map-df/
#
#
#

map_with_progress = function(.x, .f, ..., .id = NULL) {
  .f = purrr::as_mapper(.f, ...)
  pb = progress::progress_bar$new(total = length(.x), force = TRUE)

  f = function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, f, ..., .id = .id)
}

pmap_with_progress = function(.x, .f, ..., .id = NULL) {
  .f = purrr::as_mapper(.f, ...)
  pb = progress::progress_bar$new(total = nrow(.x), force = TRUE)

  f = function(...) {
    pb$tick()
    .f(...)
  }
  purrr::pmap(.x, f, ..., .id = .id)
}
