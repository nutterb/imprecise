context("get_sigfig")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Retrieves the correct precision values for calculated object",
  {
    m <- as_calculated(rep(123.45, 5), sigfig = 1:5)
    expect_equal(
      get_sigfig(m),
      1:5
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If x is not measured or calculated, return NULL and cast a warning",
  {
    expect_warning(
      get_sigfig(letters)
    )
  }
)

test_that(
  "If x is not measured or calculated, return NULL and cast a warning",
  {
    expect_null(
      suppressWarnings(get_sigfig(letters))
    )
  }
)