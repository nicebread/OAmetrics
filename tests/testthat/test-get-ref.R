test_that("Get a reference set for psychology", {
  expect_no_error(get_reference_set(from_year = 2020, to_year = 2021, n_per_year=100))
})
