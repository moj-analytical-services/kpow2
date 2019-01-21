context('Test percentage')

test_that(
  "percentage returns correct values when given a sensible input.",
  {
    expect_equal(
      percentage(100,1),
      "1%"
    )
    
    expect_equal(
      percentage(50,25),
      "50%")
  }
)

test_that(
  "percentage returns an error when passed with negative numbers, the first number is smaller than the second or a string variable",
  {
    
    expect_error(
      percentage(1,10)
    )
    
    expect_error(
      percentage(10,-1)
    )
    
    expect_error(
      percentage('a',10)
    )
  }
)
