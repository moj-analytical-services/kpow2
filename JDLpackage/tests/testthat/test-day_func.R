context('Test day_func')

test_that(
  "day_func returns correct values when given a sensible input.",
  {
    expect_equal(
      day_func(1),
      "day")
    
    expect_equal(
      day_func(4),
      "days"
    )
  }
)
