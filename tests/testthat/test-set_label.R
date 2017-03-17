context("set_label")

# Functional Requirement 1 ------------------------------------------

test_that(
  "If x is not an atomic vector, cast an error",
  {
    expect_error(set_label(mtcars))
  }
)

test_that(
  "If x is not an atomic vector, cast an error",
  {
    expect_error(get_label(mtcars))
  }
)

test_that(
  "If x is not an atomic vector, cast an error",
  {
    expect_error(set_units(mtcars))
  }
)

test_that(
  "If x is not an atomic vector, cast an error",
  {
    expect_error(get_units(mtcars))
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "For set_label, the label is set correctly",
  {
    expect_equal(
      attr(set_label(1:3, "this is a label"), "label"),
      "this is a label"
    )
  }
)

test_that(
  "For get_label, the label is set correctly",
  {
    x <- set_label(1:3, "this is a label")
    expect_equal(
      get_label(x),
      "this is a label"
    )
  }
)

test_that(
  "For set_units, the units attribute is set correctly",
  {
    expect_equal(
      attr(set_units(1:3, "mg/L"), "units"),
      "mg/L"
    )
  }
)

test_that(
  "For get_label, the label is set correctly",
  {
    x <- set_units(1:3, "mg/L")
    expect_equal(
      get_units(x),
      "mg/L"
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "For get_label, if the label attribute is NULL or doesn't exist, return the name of x",
  {
    some_variable_name <- 1:3
    expect_equal(
      get_label(some_variable_name),
      "some_variable_name"
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "For get_units, if the units attribute is NULL or doesn't exist, return an empty string",
  {
    some_variable_name <- 1:3
    expect_equal(
      get_units(some_variable_name),
      ""
    )
  }
)