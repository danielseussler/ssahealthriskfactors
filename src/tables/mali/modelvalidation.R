#
#
#
#

library(data.table)
library(gamlss.dist)
library(Metrics)
library(xtable)

# load results from the model selection file
load(file = file.path("models", "mali", "4r35hoz4.rda"))

# in the table only compare the binomial and beta binomial specification with the base formula
res = res[model == "Base" | model == "Base\n (Binomial)"]

# compute lower and upper 90% prediction intervals for binomial and beta binomial distribution
res[model %in% c("Base\n (Binomial)"), q0025 := qBI(0.025, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base\n (Binomial)"), q005 := qBI(0.05, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base\n (Binomial)"), q010 := qBI(0.10, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base\n (Binomial)"), q025 := qBI(0.25, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base\n (Binomial)"), q075 := qBI(0.75, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base\n (Binomial)"), q090 := qBI(0.90, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base\n (Binomial)"), q095 := qBI(0.95, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base\n (Binomial)"), q0975 := qBI(0.975, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]

res[model %in% c("Base"), q0025 := qBB(0.025, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base"), q005 := qBB(0.05, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base"), q010 := qBB(0.10, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base"), q025 := qBB(0.25, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base"), q075 := qBB(0.75, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base"), q090 := qBB(0.90, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base"), q095 := qBB(0.95, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("Base"), q0975 := qBB(0.975, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]

res[, coverage50 := ifelse((q025 <= k) & (k <= q075), 1L, 0L)]
res[, coverage80 := ifelse((q010 <= k) & (k <= q090), 1L, 0L)]
res[, coverage90 := ifelse((q005 <= k) & (k <= q095), 1L, 0L)]
res[, coverage95 := ifelse((q0025 <= k) & (k <= q0975), 1L, 0L)]

res[model %in% c("Base\n (Binomial)"), score := - dBI(k, bd = n, mu = mu, log = TRUE)]
res[model %in% c("Base"), score := - dBB(k, bd = n, mu = mu, sigma = sigma, log = TRUE)]

res[model == "Base\n (Binomial)", model := "Binomial"]
res[model == "Base", model := "Beta binomial"]

# compute metrics
res = res[, .(bias = mean(k/n - mu), mae = mae(k/n, mu), rmse = rmse(k/n, mu), coverage80 = mean(coverage80),
              overage90 = mean(coverage90), coverage95 = mean(coverage95)), by = .(model)]

res

# print table
print(xtable(res, type = "latex", digits = 3), file = file.path("results", "tables", "mali_modelvalidation.tex"), include.rownames = FALSE)
