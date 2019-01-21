context('Test higher_or_lower_CI')

test_that(
  "higher_or_lower_CI returns correct values when given a sensible input.",
  {
    expect_equal(
      higher_or_lower_CI(1234,2345,"days","men"),
      '<div class = "b_blue">higher by between 1,234 and 2,345 days.</div> <b>This is a statistically significant result.</b>')
    
    expect_equal(
      higher_or_lower_CI(-1234,2345,"offences","women"),
      '<div class = "b_blue">lower by as many as 1,234 offences, or higher by as many as 2,345 offences.</div> More women would need to be analysed in order to determine the direction of this difference.'
    )
    
    expect_equal(
      higher_or_lower_CI(-2345, -1234,"people","people"),
      '<div class = "b_blue">lower by between 1,234 and 2,345 people.</div> <b>This is a statistically significant result.</b>'
    )
  }
)

test_that(
  "higher_or_lower_CI returns an error when passed with unrecognised gender and  type, and when numbers are wrong way round",
  {
    
    expect_error(
      higher_or_lower_CI(-2345, -1234,"people","orange")
    )
    
    expect_error(
      higher_or_lower_CI(-2345, -1234,"orange","people")
    )
    
    expect_error(
      higher_or_lower_CI(2345, -1234,"people","people")
    )
    
    
  }
)
