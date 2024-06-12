# Unit test to check that the function returns the correct number of citable items for a given ISSN and year

test_that("get_JIF returns the correct number of citable_items", {
  expect_equal(get_JIF(issn="0022-3514", year=2018)$citable_items, 204)
})
