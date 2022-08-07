context("resample")

test_that("resample-step", {
  task = mlr_tasks$get("sonar")
  acb = AutoCompBoost(
    task = task,
    enable_tuning = FALSE,
    param_values = list(
      iters_max_univariate = 100L,
      iters_max_interactions = 100L,
      add_deeper_interactions = TRUE
    )
  )
  acb$resample()
  rr = acb$resample_result()
  expect_resample_result(rr)

  scores = rr$score(msr("classif.ce"))
  expect_list(scores$prediction, "Prediction")
  expect_numeric(scores$classif.ce, any.missing = FALSE)
  expect_number(acb$aggregate(msr("classif.ce")))
})
