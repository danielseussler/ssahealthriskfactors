# tidy names for plots
# credit to https://github.com/adamdsmith/NanSound_EcolEvol/blob/master/R/tidy_baselearner_functions.R
#
#

tidy_baselearner_names = function(model, which = NULL) {
  txt = gsub('\"', "", extract(model, "bnames", which = which))

  txt = gsub("bols\\((.*), intercept = FALSE\\)", "\\1", txt)
  txt = gsub("bols\\((.*)\\)", "\\1", txt)
  txt = gsub("bols\\((.*), by = (.*)\\)", "\\1*\\2", txt)

  txt = gsub("bbs\\((.*), knots = (.*), df = (.*), center = (.*)\\)", "f(\\1)", txt)
  txt = gsub("bbs\\((.*), df = (.*), center = (.*)\\)", "f(\\1)", txt)

  txt = gsub("bmrf\\((.*), bnd = (.*)\\)", "f(\\1)", txt)
  txt = gsub("bmrf\\((.*), bnd = (.*), by = (.*)\\)", "f(\\1)*\\3", txt)
  txt = gsub("bspatial\\((.*), center = (.*), df = (.*)\\)", "f(\\1)", txt)

  txt
}


tidy_baselearner_names_str = function(char, which = NULL) {
  txt = char

  txt = gsub("bols\\((.*), intercept = FALSE\\)", "\\1", txt)
  txt = gsub("bols\\((.*)\\)", "\\1", txt)
  txt = gsub("bols\\((.*), by = (.*)\\)", "\\1*\\2", txt)

  txt = gsub("bbs\\((.*), knots = (.*), df = (.*), center = (.*)\\)", "f(\\1)", txt)
  txt = gsub("bbs\\((.*), df = (.*), center = (.*)\\)", "f(\\1)", txt)

  txt = gsub("bmrf\\((.*), bnd = (.*)\\)", "f(\\1)", txt)
  txt = gsub("bmrf\\((.*), bnd = (.*), by = (.*)\\)", "f(\\1)*\\3", txt)
  txt = gsub("bspatial\\((.*), center = (.*), df = (.*)\\)", "f(\\1)", txt)

  txt
}
