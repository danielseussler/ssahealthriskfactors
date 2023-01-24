# create random string to facilitate model caching

stringi::stri_rand_strings(n = 1, length = 8, pattern = "[a-z0-9]")
