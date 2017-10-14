context("Basic extractions")

test_that("Toy example", {
  set.seed(1)
  data(ballots)
  cballots <- cleanBallots(ballots)
  result <- stv(cballots, seats = 4)
  ans <- result$elected
  
  expect_that(ans, equals(c("ATL_19", "ATL_10", "ATL_2", "ATL_54")))

  set.seed(4)
  result2 <- stv(cballots, seats = 4)
  an2 <- result2$elected
  
  expect_that(an2, equals(c("ATL_19", "ATL_10", "ATL_2", "ATL_27")))
  
  result3 <- stv(cballots, seats = 4, surplusMethod = "Fractional")
  ans3 <- result3$elected
  
  expect_that(ans3, equals(c("ATL_19", "ATL_10", "ATL_2", "ATL_54")))
  
})
