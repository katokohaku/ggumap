test_that("multiplication works", {
  p <- ggRaincloud(iris)
  expect_true(ggplot2::is.ggplot(p))
})
