test_that("check output",{
  expect_equal(getcityid("linköping"), "0580")
  expect_equal(getcityid("StOckHoLm"), "0180")
  expect_equal(getcityid("linkoping"), "Oops, notfound. Check is the input correct and a full name")
})