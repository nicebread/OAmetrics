test_that("Get a reference set for psychology", {
  expect_no_error(get_reference_set(years=2020:2021, n_per_year=20))
})
