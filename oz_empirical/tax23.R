
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

burn      = estimate(spec, 1e4, thin = 1e4)
post      = estimate(burn, 2e4, thin = 10)

save(post, spec, file = paste0("tax23.rda"))
