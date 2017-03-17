context("set_precision")

# Functional Requirement 1 ------------------------------------------

test_that(
  "If `x` is not a `measured` object, cast a warning and return the vector unchanged",
  {
    expect_equal(
      suppressWarnings(set_precision(letters, precision = 2)),
      letters
    )
  }
)

test_that(
  "If `x` is not a `measured` object, cast a warning and return the vector unchanged",
  {
    expect_warning(
      set_precision(letters, precision = 2)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Correctly resets the precision attribute",
  {
    m <- as_measured(rep(123.45, 5), precision = 1)
    expect_equal(
      attr(set_precision(m, c(-2:2)), "precision"),
      -2:2
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "If length(x) is not a multiple of length(precision), cast a warning",
  {
    m <- as_measured(rep(123.45, 5), precision = 1)
    expect_warning(
      set_precision(m, 1:3)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If length(precision) is greater than length(x), cast an error",
  {
    m <- as_measured(rep(123.45, 5), precision = 1)
    expect_error(
      set_precision(m, 1:10)
    )
  }
)