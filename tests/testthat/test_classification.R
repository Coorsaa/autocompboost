context("classification")

test_that("classification-works", {
  task = mlr_tasks$get("sonar")
  acb = AutoCompBoost(task = task, tuning_time = 10L)

  acb$train()
  expect_class(acb$model(), "rpart") # FIXME: change to compboost when implemented
  p = acb$predict()
  expect_prediction(p)
  expect_prediction_classif(p)
})

test_that("classification-multiclass", {
  task = mlr_tasks$get("iris")
  acb = AutoCompBoost(task = task, tuning_time = 10L)

  acb$train()
  expect_class(acb$model(), "rpart") # FIXME: change to compboost when implemented
  p = acb$predict()
  expect_prediction(p)
  expect_prediction_classif(p)
})