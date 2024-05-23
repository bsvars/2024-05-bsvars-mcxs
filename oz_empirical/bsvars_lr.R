
# lower-triangular
model = "lr_ex_p12"


# install packages
############################################################
# install.packages("devtools")
# devtools::install_github("bsvars/bsvars", force = TRUE)
# devtools::install_github("bsvars/bsvarTVPs", force = TRUE)

# data as in based on Groshenny, Javed (2023, WP)
# models as in Turnip (2017, ER)
############################################################
load("bsvars_data.rda")

# setup
############################################################
library(bsvars)
set.seed(123)

N       = ncol(y)
p       = 12
S_burn  = 1e4
S       = 2e4
thin    = 2

# structural matrix - extended model
############################################################
B = matrix(TRUE, N, N)
B[upper.tri(B)] = FALSE
B[3,4] = TRUE

# estimation - extended model - MLE prior for A
############################################################
spec_bsvar_lr   = specify_bsvar$new(as.matrix(y), p = p, B = B, exogenous = x)
A_mle           = t(solve(
  tcrossprod(spec_bsvar_lr$data_matrices$X),
  tcrossprod(spec_bsvar_lr$data_matrices$X, spec_bsvar_lr$data_matrices$Y)
))
spec_bsvar_lr$prior$A = A_mle

spec_bsvar_lr |> 
  estimate(S = S_burn) |> 
  estimate(S = S, thin = thin) -> soe_bsvar

# # estimation - extended MS heteroskedastic model
# ############################################################
# spec_bsvar_lr_msh = specify_bsvar_msh$new(as.matrix(y), p = p, B = B, M = 2, exogenous = x)
# # spec_bsvar_lr_msh$prior$A = A_mle
# 
# spec_bsvar_lr_msh |> 
#   estimate(S = S_burn) |> 
#   estimate(S = S, thin = thin) -> soe_bsvar_msh

# estimation - extended SV heteroskedastic model
############################################################
spec_bsvar_lr_sv = specify_bsvar_sv$new(as.matrix(y), p = p, B = B, exogenous = x)
spec_bsvar_lr_sv$prior$A = A_mle

spec_bsvar_lr_sv |>
  estimate(S = S_burn) |> 
  estimate(S = S, thin = thin) -> soe_bsvar_sv

# save the estimation results
############################################################
save(
  soe_bsvar,
  # soe_bsvar_msh,
  soe_bsvar_sv,
  file = paste0("bsvars_",model,".rda")
)


# estimation results
############################################################
rm(list = ls())
library(bsvars)
# model = "lr"
model = "lr_ex_p12"
# model = "lr_ex_rw"
# model = "lr_ex_p4"
load(paste0("bsvars_",model,".rda"))

bsvars_pink = "#ff69b4"
bsvars_yell = "#ffd700"
bsvars_grad = grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))(N)

plot(
  as.ts(as.matrix(y), start = c(1969, 7), frequency = 12), 
  main = "A small monetary system for Australia",
  col = bsvars_grad,
  lwd = 2,
  bty = "n"
)
plot(
  as.ts(as.matrix(x)[,1:4], start = c(1969, 7), frequency = 12), 
  main = "A small system for Australian foreign sector",
  col = bsvars_grad,
  lwd = 2,
  bty = "n"
)

soe_bsvar_sv |> compute_impulse_responses(horizon = 24) |> plot(probability = 0.68)
soe_bsvar_sv |> compute_variance_decompositions(horizon = 24) |> plot()
soe_bsvar_sv |> compute_historical_decompositions() |> plot()
soe_bsvar_sv |> compute_fitted_values() |> plot(probability = 0.68)
soe_bsvar_sv |> compute_structural_shocks() |> plot(probability = 0.68)
soe_bsvar_sv |> compute_conditional_sd() |> plot(probability = 0.68)
