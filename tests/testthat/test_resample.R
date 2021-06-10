context("resample")

test_that("resample-step", {
  task = mlr_tasks$get("sonar")
  acb = AutoCompBoost(task = task, tuning_time = 10L)
  rr = acb$resample()
  expect_resample_result(rr)
})
