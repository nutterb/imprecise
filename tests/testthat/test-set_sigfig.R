context("set_sigfig")

# Functional Requirement 1 ------------------------------------------

test_that(
  "If `x` is not a `calculated` object, cast a warning and return the vector unchanged",
  {
    expect_equal(
      suppressWarnings(set_sigfig(letters, sigfig = 2)),
      letters
    )
  }
)

test_that(
  "If `x` is not a `calculated` object, cast a warning and return the vector unchanged",
  {
    expect_warning(
      set_precision(letters, precision = 2)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Correctly resets the sigfig attribute",
  {
    m <- as_calculated(rep(123.45, 5), sigfig = 1)
    expect_equal(
      attr(set_sigfig(m, c(1:5)), "sigfig"),
      1:5
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "If length(x) is not a multiple of length(sigfig), cast a warning",
  {
    m <- as_calculated(rep(123.45, 5), sigfig = 1)
    expect_warning(
      set_sigfig(m, 1:3)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If length(sigfig) is greater than length(x), cast an error",
  {
    m <- as_calculated(rep(123.45, 5), sigfig = 1)
    expect_error(
      set_sigfig(m, 1:10)
    )
  }
)