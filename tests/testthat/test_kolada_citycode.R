test_that("get city id",{
  expect_equal(getcityid("linköping"), "0580")
})
test_that("case insensitive",{
  expect_equal(getcityid("StOckHoLm"), "0180")
})
test_that("city not found",{
  expect_equal(getcityid("linkoping"), "Oops, notfound. Check is the input correct and a full name")
})
test_that("error input",{
  expect_error(getcityid(132))
  expect_error(getcityid(stockholm))
})

test_that("lessdata: get city id",{
  expect_equal(getcityid_lessdata("linköping"), "0580")
})
test_that("case insensitive",{
  expect_equal(getcityid_lessdata("LINköping"), "0580")
})
test_that("city not found",{
  expect_equal(getcityid_lessdata("linkoping"), "Oops, notfound. Check is the input correct and a full name")
})
test_that("error input",{
  expect_error(getcityid_lessdata(132))
  expect_error(getcityid_lessdata(stockholm))
})