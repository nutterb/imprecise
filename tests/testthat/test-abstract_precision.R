context("abstract_precision")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly determines the smallest unit of precision in the value.",
  {
    cal <- as_calculated(rep(1234.567, 7), sigfig = 1:7)
    expect_equal(
      abstract_precision(cal),
      3:-3
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If `x` is not of class `calculated`, cast an error",
  {
    expect_error(abstract_precision(letters))
  }
)