context("classification")

test_that("classification-works", {
  task = mlr_tasks$get("sonar")
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
  expect_class(model$univariate, c("Compboost", "R6"))
  expect_class(model$interactions, c("Compboost", "R6"))
  expect_class(model$deeper_interactions, "residualBooster")

  p = acb$predict()
  expect_prediction(p)
  expect_prediction_classif(p)
})

test_that("classification-multiclass", {
  task = mlr_tasks$get("iris")
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

  expect_class(model, "Multiplicity")
  expect_true(all(map_lgl(model, is.list)))
  expect_true(all(map_lgl(map(model, function(x) x$model$univariate), function(m) check_class(m, c("Compboost", "R6")))))
  expect_true(all(map_lgl(map(model, function(x) x$model$interactions), function(m) check_class(m, c("Compboost", "R6")))))
  expect_true(all(map_lgl(map(model, function(x) x$model$deeper_interactions), function(m) check_class(m, "residualBooster"))))

  p = acb$predict()
  expect_prediction(p)
  expect_prediction_classif(p)
})