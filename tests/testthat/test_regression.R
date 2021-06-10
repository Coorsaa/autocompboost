context("regression")

test_that("regression-works", {
  task = mlr_tasks$get("boston_housing")
  acb = AutoCompBoost(task = task, tuning_time = 10L)

  acb$train()
  expect_class(acb$model(), "rpart") # FIXME: change to compboost when implemented
  p = acb$predict()
  expect_prediction(p)
  expect_prediction_regr(p)
})
