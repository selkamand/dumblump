test_that("dumblump works", {
  unlumped <- c(1, 1001, 1, 3,4 , 5, 1, 1, 12, 12, 100, 112, 1000, 1008)
  expected_lump <- c(
    "Group 1",
    "Group 5",
    "Group 1",
    "Group 1",
    "Group 1",
    "Group 1",
    "Group 1",
    "Group 1",
    "Group 2",
    "Group 2",
    "Group 3",
    "Group 4",
    "Group 5",
    "Group 6"
  )

  expect_equal(dumblump(unlumped), expected_lump)
})
