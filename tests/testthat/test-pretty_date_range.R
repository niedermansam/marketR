test_that("pretty_date_range works with defaults", {
  expect_equal(pretty_date_range("2019-09-01", "2020-09-01"), "September 1st, 2019 - September 1st, 2020")
  expect_equal(pretty_date_range("2019-09-01", "2019-10-01"), "September 1st - October 1st, 2019")
  expect_equal(pretty_date_range("2019-09-01", "2019-09-02"), "September 1st - 2nd, 2019")
  })

test_that("pretty_date_range shows day of week when show_wday=T", {
  expect_equal(pretty_date_range("2019-09-01", "2020-09-01", show_wday=T), "Sunday, September 1st, 2019 - Tuesday, September 1st, 2020")
})

test_that("pretty_date_range separator works", {
  expect_equal(pretty_date_range("2019-09-01", "2020-09-02", separator=" and "), "September 1st, 2019 and September 2nd, 2020")
  expect_equal(pretty_date_range("2019-09-01", "2020-09-01", separator=" to ", show_wday = T), "Sunday, September 1st, 2019 to Tuesday, September 1st, 2020")
})
