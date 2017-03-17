context("print.calculated")

# Functional Requirement 1 ------------------------------------------

test_that(
  "The value of x is printed with the significant figures in the sigfig attribute",
  {
    x <- c(23.123, 14.5134, 9.8394324)
    sigfig <- c(2:4)
    
    expect_output(
      print(as_calculated(x, sigfig)),
      "23[.]000 14[.]500  9[.]839"
    )
    
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When label = TRUE, the label attribute is printed above the output",
  {
    expect_output(
      print(as_calculated(rnorm(10), sigfig = 3, label = "distance"),
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
      print(as_calculated(rnorm(10), sigfig = 3, units = "miles"),
            units = TRUE),
      "miles"
    )
  }
)

test_that(
  "When label = TRUE and units = TRUE, the attributes are print above the output",
  {
    expect_output(
      print(as_calculated(rnorm(10), 
                          sigfig = 3, 
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
      print(as_calculated(rnorm(10),
                          sigfig = 3,
                          label = "distance",
                          units = "   "),
            units = TRUE,
            label = TRUE),
      "distance \n"
    )
  }
)