context('Test earlier_or_later')

test_that(
  "earlier_or_later returns correct values when given a sensible input.",
  {
    expect_equal(
      earlier_or_later(1234,2345),
      "earlier")
    
    expect_equal(
      earlier_or_later(2345,-1234),
      "later"
    )
    
  }
)

test_that(
  "earlier_or_later returns an error when passed with unrecognised gender and  type, and when numbers are wrong way round",
  {
    
    expect_error(
      earlier_or_later(-2345, 'a')
    )
    
    expect_error(
      earlier_or_later(-2345, NULL)
    )
    
    expect_error(
      earlier_or_later(2345, NA)
    )
    
    
  }
)
