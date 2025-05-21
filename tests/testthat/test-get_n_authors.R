# Unit test to check that the function returns the correct number of authors for a sepcific work

test_that("get_n_authors returns the correct number of authors", {
  OA_object <- oa_fetch(doi=c("10.1098/rsos.201925", "10.1080/00223891.2020.1726936"))
  n_authors <- get_n_authors(OA_object)
  expect_equal(n_authors, c(6, 21))
})
