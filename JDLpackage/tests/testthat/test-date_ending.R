context('Test date_ending')

test_that(
  "date_ending returns correct values when given a sensible input.",
  {
    expect_equal(
      date_ending(1),
      "st")
    
    expect_equal(
      date_ending(12),
      "th"
    )
    
    expect_equal(
      date_ending(2),
      "nd"
    )
    
    expect_equal(
      date_ending(21),
      "st"
    )
  }
)
