# Unit test to check that the function returns the correct number of citable items for a given ISSN and year

test_that("get_JIF returns the correct number of citable_items", {
  # not having a reference set makes these test impossible
  #expect_no_error(FNCS(doi="10.1037/met0000061", ref_set=ref_set, upper_trim = .01))
  #expect_no_error(FNCS(doi="10.1037/met0000061", ref_set=ref_set, upper_trim = .0))
})
