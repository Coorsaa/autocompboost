context("resample")

test_that("resample-step", {
  task = mlr_tasks$get("sonar")
  acb = AutoCompBoost(
    task = task, 
    enable_tuning = FALSE,
    param_values = list(
      iters_max_univariate = 100L,
      iters_max_interactions = 100L,
      iters_deeper_interactions = 10L
    )
  )
  acb$resample()
  rr = acb$resample_result()
  expect_resample_result(rr)
})
