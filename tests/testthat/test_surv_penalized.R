context("surv.penalized")

test_that("autotest", {
  learner = LearnerSurvPenalized$new()
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("unpenalized", {
  task = tgen("simsurv")$generate(5)
  learner = lrn("surv.penalized", unpenalized = c("height"))
  learner$train(task)
  expect_equal(names(learner$model@penalized), c("treatment", "weight"))
  expect_equal(names(learner$model@unpenalized), c("height"))
  expect_prediction_surv(learner$predict(task))
})
