# Unit test to check that the function returns the correct number of citable items for a given ISSN and year

test_that("get_JIF returns the correct number of citable_items", {
  # according to Clarivate, the JIF in 2019 was 6.3;
  # but OpenAlex counts more citing sources, so the IF is higher here (7.6).
  JIF <- get_JIF(issn="0022-3514", year=2019)
  expect_equal(JIF$citable_items, 236)
})
