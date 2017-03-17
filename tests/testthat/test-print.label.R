context("print.label")

vec <- 1:3
vec <- set_label(vec, "distance")
vec <- set_units(vec, "km")

# Functional Requirement 1 ------------------------------------------

test_that(
  "prints the vector using the method that would be used if the vector did 
   not carry the label subclass",
  {
    expect_output(
      print(vec),
      "[[]1[]] 1 2 3"
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When label = TRUE, the label attribute is printed above the output.",
  {
    expect_output(
      print(vec, label = TRUE),
      "distance \n[[]1[]] 1 2 3"
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "When units = TRUE, the units attribute is printed above the output.",
  {
    expect_output(
      print(vec, units = TRUE),
      "[(]km[)] \n[[]1[]] 1 2 3"
    )
  }
)

test_that(
  "When label = TRUE and units = TRUE, the label and units attributes is printed 
  above the output.",
  {
    expect_output(
      print(vec, label = TRUE, units = TRUE),
      "distance [(]km[)] \n[[]1[]] 1 2 3"
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "when units = TRUE and there are no non-white space characters in the 
   units attribute, quietly reset units to FALSE",
  {
    vec <- set_units(vec, "    ")
    expect_output(
      print(vec, units = TRUE),
      "[[]1[]] 1 2 3"
    )
  }
)

test_that(
  "when units = TRUE and there are no non-white space characters in the 
   units attribute, quietly reset units to FALSE",
  {
    vec <- set_units(vec, "    ")
    expect_output(
      print(vec, label = TRUE, units = TRUE),
      "distance \n[[]1[]] 1 2 3"
    )
  }
)

rm(list = "vec")
