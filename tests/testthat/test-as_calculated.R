context("as_calculated")

# Functional Requirement 1 ------------------------------------------
test_that(
  "If x is numeric, return a vector with sigfig, precision, label, and units attributes",
  {
    m <- as_calculated(rnorm(10), sigfig = 2)
    expect_equal(
      names(attributes(m)),
      c("sigfig", "precision", "label", "units", "class")
    )
  }
)

test_that(
  "If x is numeric, return a vector of class `measured`",
  {
    expect_class(
      as_calculated(rnorm(10), sigfig = 3),
      classes = "calculated"
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If sigfig is not integerish, cast an error",
  {
    expect_error(
      as_calculated(rnorm(10), precision = 1:10)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "sigfig may have length no greater than x",
  {
    expect_error(
      as_calculated(rnorm(10), sigfig = 1:12)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If length(x) is not a multiple of length(sigfig) cast a warning",
  {
    expect_warning(
      as_calculated(rnorm(10), 1:3)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "If label is a character vector with length greater than 1, cast an error",
  {
    expect_error(
      as_calculated(rnorm(10), sigfig = 2, label = letters[1:2])
    )
  }
)

test_that(
  "If label is not a character vector, cast an error",
  {
    expect_error(
      as_calculated(rnorm(10)* 1000, precision = 2, label = 7)
    )
  }
)

test_that(
  "If label is a character vector of length 1, succeed",
  {
    expect_silent(
      as_calculated(rnorm(10) * 1000, sigfig = 2, label = "km")
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "If label is a character vector with length greater than 1, cast an error",
  {
    expect_error(
      as_calculated(rnorm(10), sigfig = 2, units = letters[1:2])
    )
  }
)

test_that(
  "If units is not a character vector, cast an error",
  {
    expect_error(
      as_calculated(rnorm(10)* 1000, sigfig = 2, units = 7)
    )
  }
)

test_that(
  "If units is a character vector of length 1, succeed",
  {
    expect_silent(
      as_calculated(rnorm(10) * 1000, sigfig = 2, units = "km")
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "If x is an integer, cast a warning",
  {
    expect_warning(
      as_calculated(1:10, 3,
                  "Integer values have no measurement imprecision")
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "If x is not a numeric, double, or integer vector, cast a warning",
  {
    expect_warning(
      as_calculated(letters, sigfig = 2),
      "Non-numeric values have no measurement imprecision"
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "If x is character, the return is x",
  {
    expect_equal(
      suppressWarnings(as_calculated(letters)),
      letters
    )
  }
)

test_that(
  "If x is factor, the return is x",
  expect_equal(
    suppressWarnings(as_calculated(factor(letters))),
    factor(letters)
  )
)

test_that(
  "If x is Date, the return is x",
  expect_equal(
    suppressWarnings(as_calculated(Sys.Date())),
    Sys.Date()
  )
)
