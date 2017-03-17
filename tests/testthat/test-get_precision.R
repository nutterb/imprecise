context("get_precision")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Retrieves the correct precision values for measured object",
  {
    m <- as_measured(rep(123.45, 5), precision = 2:-2)
    expect_equal(
      get_precision(m),
      2:-2
    )
  }
)

test_that(
  "Retrieves the correct precision values for calculated object",
  {
    m <- as_calculated(rep(123.45, 5), sigfig = 1:5)
    expect_equal(
      get_precision(m),
      2:-2
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If x is not measured or calculated, return NULL and cast a warning",
  {
    expect_warning(
      get_precision(letters)
    )
  }
)

test_that(
  "If x is not measured or calculated, return NULL and cast a warning",
  {
    expect_null(
      suppressWarnings(get_precision(letters))
    )
  }
)