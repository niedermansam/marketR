test_that("pretty_date formats days correctly", {
  expect_equal(pretty_date("2019-09-01"), "September 1st, 2019")
  expect_equal(pretty_date("2019-09-02"), "September 2nd, 2019")
  expect_equal(pretty_date("2019-09-03"), "September 3rd, 2019")
  expect_equal(pretty_date("2019-09-04"), "September 4th, 2019")
})

test_that("prettyDate only includes weekday when show_wday=T", {
  expect_equal(pretty_date("2019-09-01", show_wday = F), "September 1st, 2019")
  expect_equal(pretty_date("2019-09-01", show_wday = T), "Sunday, September 1st, 2019")
})

test_that("prettyDate only includes month when show_month=T", {
  expect_equal(pretty_date("2019-09-01", show_month = F), "1st, 2019")
  expect_equal(pretty_date("2019-09-01", show_month = T), "September 1st, 2019")
})

test_that("prettyDate only includes year when show_year=T", {
  expect_equal(pretty_date("2019-09-01", show_year = F), "September 1st")
  expect_equal(pretty_date("2019-09-01", show_year = T), "September 1st, 2019")
})
