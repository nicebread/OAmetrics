test_that("retrieve h-index", {
  expect_no_error(h_index("Julia Rohrer", academic_age_bonus = 1.5))
  expect_no_error(h_index("John Rauthmann", first_pub_year = 2009))
  expect_no_error(h_index(author.id = c("A5049380281", "A5022479713")))
})
