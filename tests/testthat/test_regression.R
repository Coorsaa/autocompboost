context("regression")

test_that("regression-works", {
  task = mlr_tasks$get("boston_housing")
  acb = AutoCompBoost(
    task = task,
    enable_tuning = FALSE,
    param_values = list(
      iters_max_univariate = 500L,
      iters_max_interactions = 500L,
      iters_deeper_interactions = 50L
    )
  )

  acb$train()
  model = acb$model()

  expect_class(model, "list")
  expect_class(model$univariate, "Compboost")
  expect_class(model$interactions, "Compboost")
  expect_class(model$deeper_interactions, "distilledTree")

  p = acb$predict()
  expect_prediction(p)
  expect_prediction_regr(p)
})
