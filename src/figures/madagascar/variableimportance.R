#
#
#
#

library(data.table)
library(mboost)
library(ggplot2)

# load and extract the defaults
source(file = file.path("src", "utils", "func_tidy_baselearner.R"))
fnames = list.files(path = file.path("models", "madagascar", "main"))

# extract the defaults
load(file = file.path(file.path("models", "madagascar", "main", fnames[1])))

bls = extract(mod, what = "bnames", which = "")
bls_clean = tidy_baselearner_names_str(bls)

# define result objects
xselect = matrix(0, nrow = length(bls), ncol = length(fnames))
plot_data = data.table()

# iterate over the subsampling replications models
for (i in 1:length(fnames)) {
  load(file = file.path(file.path("models", "madagascar", "main", fnames[i])))

  # track selection frequency
  xselect[unique(selected(mod)), i] = 1

  # recall risk reduction
  tmp_data = as.data.frame(varimp(mod))
  tmp_data$iter = i
  plot_data = rbind(plot_data, tmp_data)
}

plot_data$name = tidy_baselearner_names_str(char = plot_data$blearner)
plot_data$name = forcats::fct_reorder(plot_data$name, plot_data$reduction, "mean")

plt = ggplot(data = plot_data, mapping = aes(x = name, y = reduction)) +
  geom_boxplot(width = 0.4, outlier.alpha = 0.2, fill = "#69b3a2") +
  coord_flip() +
  labs(x = "Base learner", y = "Risk reduction")

ggsave(
  plot = plt
  , filename = "madagascar_variableimportance.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 200L
  , units = "mm", device = png, bg = "transparent"
)
