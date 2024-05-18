
############################################################
# Reproduction of the IRF from Lütkepohl, Shang, Uzeda, Woźniak (2024)
############################################################

# estimate the model
############################################################
library(bsvars)
data("us_fiscal_lsuw")
data("us_fiscal_ex")

set.seed(1234)
B         = matrix(TRUE, 3, 3)
spec      = specify_bsvar_sv$new(
  data = us_fiscal_lsuw,
  p    = 4,
  exogenous = us_fiscal_ex,
  B    = B
)

burn      = estimate(spec, 1e5, thin = 1e4)
post      = estimate(burn, 2e5, thin = 10)

save(post, spec, file = paste0("tax23.rda"))


# estimation results
############################################################
rm(list = ls())
library(bsvars)
load("tax23.rda")
bsvars_pink = "#ff69b4"
bsvars_yell = "#ffd700"
bsvars_grad = grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))(3)

plot(
  us_fiscal_lsuw, 
  col = bsvars_pink,
  lwd = 2,
  bty = "n"
)
plot(
  us_fiscal_ex, 
  col = bsvars_yell,
  lwd = 2,
  bty = "n"
)

post |> compute_impulse_responses(horizon = 20) |> plot(probability = 0.68)
post |> compute_variance_decompositions(horizon = 12) |> plot()
# post |> compute_historical_decompositions() |> plot()
post |> compute_fitted_values() |> plot(probability = 0.68)
post |> compute_structural_shocks() |> plot(probability = 0.68)
post |> compute_conditional_sd() |> plot(probability = 0.68)
