context('Test next_pub_date')

test_that(
  "next_pub_date returns correct values when given a sensible input.",
  {
    expect_equal(
      next_pub_date(1,"July",2017),
      "12th October 2017")
    
    expect_equal(
      next_pub_date(1,"December",2017),
      "8th March 2018")
    
  }
)

test_that(
  "next_pub_date returns an error when passed with unrecognised when an invalid month is entered",
  {
    
    expect_error(
      ext_pub_date(1,"Month",2017)
    )
    
    expect_error(
      ext_pub_date(41,"July",2017)
    )
  }
)
