context("count_sigfig")

# Functional Requirement 1 ------------------------------------------

test_that(
  "If `x` does not inherit class `measured`, cast an error.",
  {
    expect_error(
      count_sigfig(1:10)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Return the correct count of significant figures for the measurement.",
  {
    m <- as_measured(rep(123456.7890, 10), precision = 5:-4)
    expect_equal(
      count_sigfig(m),
      1:10
    )
  }
)