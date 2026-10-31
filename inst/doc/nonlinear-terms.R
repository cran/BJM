## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BJM)
data(pbc3)
data_fit <- pbc3[pbc3$status3 == 1, ]

## ----poly-raw-----------------------------------------------------------------
fit_poly_raw <- longitudinalSub(
  data_fit, serBilir ~ poly(year, 2, raw = TRUE) + age + sex, ~ year | id
)

## ----explicit-transforms------------------------------------------------------
fit_explicit <- longitudinalSub(
  data_fit, serBilir ~ log(year + 1) + sqrt(year) + age + sex, ~ year | id
)

## ----interaction--------------------------------------------------------------
fit_interaction <- longitudinalSub(
  data_fit, serBilir ~ year + I(year^2) * age + sex, ~ year | id
)

## ----safe-in-fixed------------------------------------------------------------
fit_orthogonal_poly <- longitudinalSub(
  data_fit, serBilir ~ year + poly(age, 2) + factor(sex), ~ year | id
)

fit_ns <- longitudinalSub(
  data_fit, serBilir ~ splines::ns(year, df = 3) + age + sex, ~ year | id
)

## ----unsafe-warning, warning = TRUE-------------------------------------------
fit_ns_random <- longitudinalSub(
  data_fit, serBilir ~ year + age + sex, ~ splines::ns(year, df = 2) | id
)

## ----survival-trans-----------------------------------------------------------
trans <- survivalTrans(c(1, 3, 5, 7))
data_fit_trans <- data_fit
data_fit_trans$Tyears1 <- trans$survival_trans_function[[1]](data_fit_trans$year)

fit_trans <- longitudinalSub(
  data_fit_trans, serBilir ~ year + Tyears1 + age + sex, ~ year | id
)

## ----end-to-end---------------------------------------------------------------
fit_spline <- longitudinalSub(
  data_fit, serBilir ~ splines::ns(year, df = 3) + age + sex + years, ~ year | id
)

data_survival_fitting <- pbc3[!duplicated(pbc3$id), ]
survival_fit <- survivalSub(
  data_survival_fitting,
  form_marginal_surv = Surv(years, status3) ~ age + sex,
  form_conditional_cr = NULL
)

data_predict <- pbc3[pbc3$id == 2 & pbc3$year <= 5, ]
risk <- predictRisk(
  data_predict, fit_spline, survival_fit,
  prediction_time = 5, horizon = 1, time_variable = "year",
  survival_variable_all = list(), survival_trans_function = list(),
  bandcount1 = 10, bandcount2 = 20
)
risk

