## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BJM)
data(pbc3)

## ----survival-sub-------------------------------------------------------------
data_survival_fitting <- pbc3[!duplicated(pbc3$id), ]

survival_fit_all <- survivalSub(
  data_survival_fitting,
  form_marginal_surv = Surv(years, status3) ~ age + sex,
  form_conditional_cr = status4 ~ years + age + sex
)

survival_fit_all

## ----longitudinal-sub---------------------------------------------------------
long_sub_fixed <- list(
  "serBilir" = serBilir ~ year + age + sex + (years) + (years) * year,
  "albumin"  = albumin  ~ year + age + sex + (years) + (years) * year
)
long_sub_random <- list(
  "serBilir" = ~ year | id,
  "albumin"  = ~ year | id
)

data_fit_all <- list(pbc3[pbc3$status3 == 1, ], pbc3[pbc3$status3 == 1, ])

long_fit_all <- longitudinalSub(data_fit_all, long_sub_fixed, long_sub_random)

long_fit_all

## ----dynamic-prediction-------------------------------------------------------
survival_variable_all <- list("Tyears1", "Tyears2", "Tyears3", "Tyears4")
survival_trans_function <- list(
  fun1 = function(x) abs(x - 1),
  fun2 = function(x) abs(x - 3),
  fun3 = function(x) abs(x - 5),
  fun4 = function(x) abs(x - 7)
)

data_raw_predict <- pbc3[pbc3$id == 2, ]
data_predict_all <- list(data_raw_predict, data_raw_predict)

risk <- dynamicPrediction(
  data_predict_all, long_fit_all, survival_fit_all,
  prediction_time = 5, horizon = 1, time_variable = "year",
  survival_variable_all, survival_trans_function,
  bandcount1 = 10, bandcount2 = 20
)

risk

## ----survival-trans-helper----------------------------------------------------
trans <- survivalTrans(c(1, 3, 5, 7))
identical(trans$survival_variable_all, survival_variable_all)
trans$survival_trans_function[[1]](2)

## ----dynamic-prediction-bio---------------------------------------------------
bio_pred <- dynamicPredictionBio(
  bio_i = 1, data_predict_all, long_fit_all, survival_fit_all,
  prediction_time = 5, horizon = 1, time_variable = "year",
  survival_variable_all, survival_trans_function,
  bandcount2 = 20, bandcount3 = 50
)

bio_pred$Y_predict

## ----bandcount-convergence----------------------------------------------------
risk_default <- dynamicPrediction(
  data_predict_all, long_fit_all, survival_fit_all,
  prediction_time = 5, horizon = 1, time_variable = "year",
  survival_variable_all, survival_trans_function,
  bandcount1 = 10, bandcount2 = 20
)

risk_doubled <- dynamicPrediction(
  data_predict_all, long_fit_all, survival_fit_all,
  prediction_time = 5, horizon = 1, time_variable = "year",
  survival_variable_all, survival_trans_function,
  bandcount1 = 20, bandcount2 = 40
)

abs(risk_default$risk_prob_1 - risk_doubled$risk_prob_1)
abs(risk_default$risk_prob_2 - risk_doubled$risk_prob_2)

## ----validation-example, error = TRUE-----------------------------------------
try({
dynamicPrediction(
  data_predict_all[[1]], long_fit_all, survival_fit_all,
  prediction_time = 5, horizon = 1, time_variable = "year",
  survival_variable_all, survival_trans_function,
  bandcount1 = 10, bandcount2 = 20
)
})

## ----validation-example-2, error = TRUE---------------------------------------
try({
longitudinalSub(pbc3, serBilir ~ year + not_a_column, ~ year | id)
})

