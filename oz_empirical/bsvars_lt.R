
# lower-triangular
model = "lt_ex_rw"


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
p       = 6
S_burn  = 1e4
S       = 2e4
thin    = 2


# estimation - lower-triangular model
############################################################
spec_bsvar0     = specify_bsvar$new(as.matrix(y), p = p, exogenous = x)
spec_bsvar0 |> 
  estimate(S = S_burn) |> 
  estimate(S = S, thin = thin) -> soe_bsvar0

# estimation - lower-triangular model - MLE prior for A
############################################################
spec_bsvar      = specify_bsvar$new(as.matrix(y), p = p, exogenous = x)

# A_mle           = t(solve(
#   tcrossprod(spec_bsvar$data_matrices$X),
#   tcrossprod(spec_bsvar$data_matrices$X, spec_bsvar$data_matrices$Y)
# ))
# spec_bsvar$prior$A = A_mle

spec_bsvar |>
  estimate(S = S_burn) |>
  estimate(S = S, thin = thin) -> soe_bsvar

# estimation - lower-triangular MS heteroskedastic model
############################################################
spec_bsvar_msh    = specify_bsvar_msh$new(as.matrix(y), p = p, M = 2, exogenous = x)
# spec_bsvar_msh$prior$A = A_mle

spec_bsvar_msh |> 
  estimate(S = S_burn) |> 
  estimate(S = S, thin = thin) -> soe_bsvar_msh

# estimation - lower-triangular SV heteroskedastic model
############################################################
spec_bsvar_sv = specify_bsvar_sv$new(as.matrix(y), p = p, exogenous = x)
# spec_bsvar_sv$prior$A = A_mle

spec_bsvar_sv |>
  estimate(S = S_burn) |> 
  estimate(S = S, thin = thin) -> soe_bsvar_sv


# save the estimation results
############################################################
save(
  soe_bsvar0,
  soe_bsvar,
  soe_bsvar_msh,
  soe_bsvar_sv,
  file = paste0("bsvars_",model,".rda")
)


# # estimation results
# ############################################################
# rm(list = ls())
# library(bsvars)
# model = "lt"
# model = "lt_ex"
# load(paste0("bsvars_",model,".rda"))
# 
# soe_bsvar0 |> compute_impulse_responses(horizon = 24) |> plot()
# soe_bsvar  |> compute_impulse_responses(horizon = 24) |> plot()
# soe_bsvar_msh |> compute_impulse_responses(horizon = 24) |> plot()
# soe_bsvar_msh |> compute_conditional_sd() |> plot()
# soe_bsvar_msh |> compute_regime_probabilities() |> plot()
# soe_bsvar_sv |> compute_impulse_responses(horizon = 24) |> plot()
# soe_bsvar_sv |> compute_conditional_sd() |> plot()
