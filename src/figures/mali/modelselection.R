#
#
#
#

library(data.table)
library(gamlss.dist)
library(ggplot2)

# load results from the model selection file
load(file = file.path("models", "mali", "4r35hoz4.rda"))

# add negative log-likelihood score
res[model == "Base\n (Binomial)", score := - dBI(k, bd = n, mu = mu, log = TRUE)]
res[model != "Base\n (Binomial)", score := - dBB(k, bd = n, mu = mu, sigma = sigma, log = TRUE)]

# plot
pltdata = res[model != "Base\n (Binomial)", .(meanscore = mean(score)), by = .(model, id)]
plt = ggplot(data = pltdata, mapping = aes(x = model, y = meanscore)) +
  geom_line(mapping = aes(group = id), color = "grey") +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  labs(x = "Specification", y = "Average hold-out risk")

ggsave(
  plot = plt
  , filename = "mali_modelselection.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 80
  , units = "mm", device = png
)
