context("print.measured")

# Functional Requirement 1 ------------------------------------------

test_that(
  "The value of x is rounded to the corresponding precision",
  {
    x <- c(23.123, 14.5134, 9.8394324)
    prec <- c(1, 0, -2)

    expect_output(
      print(as_measured(x, prec)),
      "20[.]00 15[.]00  9[.]84"
    )

  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When label = TRUE, the label attribute is printed above the output",
  {
    expect_output(
      print(as_measured(rnorm(10), precision = -3, label = "distance"),
            label = TRUE),
      "distance"
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "When units = TRUE, the label attribute is printed above the output",
  {
    expect_output(
      print(as_measured(rnorm(10), precision = -3, units = "miles"),
            units = TRUE),
      "miles"
    )
  }
)

test_that(
  "When label = TRUE and units = TRUE, the attributes are print above the output",
  {
    expect_output(
      print(as_measured(rnorm(10), 
                        precision = -3, 
                        label = "distance", 
                        units = "miles"),
            label = TRUE,
            units = TRUE),
      "distance [(]miles[)]"
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "When units = TRUE and there are no non-whitespace character, quitely rest units to FALSE",
  {
    expect_output(
      print(as_measured(rnorm(10),
                        precision = -3,
                        label = "distance",
                        units = "   "),
            units = TRUE,
            label = TRUE),
      "distance \n"
    )
  }
)