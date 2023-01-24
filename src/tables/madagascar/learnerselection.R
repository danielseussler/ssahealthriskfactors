#
#
#
#

library(data.table)
library(mboost)
library(ggplot2)
library(xtable)

source(file = file.path("src", "utils", "func_tidy_baselearner.R"))

# extract the defaults
fnames = list.files(path = file.path("models", "madagascar", "main"))
load(file = file.path(file.path("models", "madagascar", "main", fnames[1])))

bls = extract(mod, what = "bnames", which = "")
bls_clean = tidy_baselearner_names_str(bls)

xselect = matrix(0, nrow = length(bls), ncol = length(fnames))

# extract for all models
for (i in 1:length(fnames)) {
  load(file = file.path(file.path("models", "madagascar", "main", fnames[i])))
  xselect[unique(selected(mod)), i] = 1
}

# print table
df = data.frame(name = factor(bls_clean), freq = rowMeans(xselect))
df$name = forcats::fct_reorder(df$name, df$freq, .desc = FALSE)

print(
  xtable(df, type = "latex")
  , file = file.path("results", "tables", "madagascar_selectionfreq.tex")
  , include.rownames = FALSE
)

ggplot(data = df, mapping = aes(x = name, y = freq)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x = "Base-learner", y = "Selection Frequency") +
  coord_flip()
