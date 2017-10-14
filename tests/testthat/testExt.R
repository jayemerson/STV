context("Basic extractions")

test_that("Toy example", {
  set.seed(1)
  data(ballots)
  cballots <- cleanBallots(ballots)
  result <- stv(cballots, seats = 4)
  ans <- result$elected

  expect_that(ans, equals(c("ATL_19", "ATL_10", "ATL_2", "ATL_54")))

})
