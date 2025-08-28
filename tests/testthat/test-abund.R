test_that("dtf(), dtt() and (dtbl() construct the correct object type", {
  expect_equal(abund(1:10)$data, "1:10")
  expect_error(abun("a"))
})
