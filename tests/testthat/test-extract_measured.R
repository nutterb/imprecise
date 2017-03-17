context("extract_measured")

# Extraction Method -------------------------------------------------

# Functional Requirement 1 ------------------------------------------

test_that(
  "The values at position i are extracted for measured variables",
  {
    m <- as_measured(rep(123.45, 5),
                     precision = 2:-2)
    expect_equal(
      m[c(1, 4:5)],
      as_measured(rep(123.45, 3),
                  precision = c(2, -1:-2))
    )
  }
)

test_that(
  "The values at position i are extracted for calculated variables",
  {
    cal <- as_calculated(rep(123.45, 5),
                         sigfig = 5:1)
    expect_equal(
      cal[c(1, 4:5)],
      as_calculated(rep(123.45, 3),
                    sigfig = c(5, 2:1))
    )
  }
)

# Replacement Method ------------------------------------------------

# Functional Requirement 1 ------------------------------------------

test_that(
  "For measured, when value has a precision attribute, the precision attribute
   of x at positions i are replaced by the precision attribute of value",
  {
    m <- as_measured(rep(123.45, 5),
                     precision = 2:-2)
    m[c(1, 4:5)] <- set_precision(m[c(1, 4:5)], -2)
    expect_equal(
      m,
      as_measured(rep(123.45, 5),
                  precision = c(-2, 1, 0, -2, -2))
    )
  }
)

test_that(
  "For measured, when value has a precision attribute, the precision attribute
   of x at positions i are replaced by the precision attribute of value",
  {
    cal <- as_calculated(rep(123.45, 5),
                         sigfig = 5:1)
    cal[c(1, 4:5)] <- set_sigfig(cal[c(1, 4:5)], 5)
    expect_equal(
      cal,
      as_calculated(rep(123.45, 5),
                  sigfig = c(5, 4, 3, 5, 5))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "For measured values, when value has no precision attribute, the value of x 
   is replaced with no change to the precision of x",
  {
    m <- as_measured(rep(123.45, 5),
                     precision = 2:-2)
    m[c(1, 4:5)] <- 543.21
    expect_equal(
      m,
      as_measured(c(543.21, 123.45, 123.45, 543.21, 543.21),
                  precision = 2:-2)
    )
  }
)

test_that(
  "For calculated values, when value has no precision attribute, the value of x 
   is replaced with no change to the precision of x",
  {
    cal <- as_calculated(rep(123.45, 5),
                         sigfig = 5:1)
    cal[c(1, 4:5)] <- 543.21
    expect_equal(
      cal,
      as_calculated(c(543.21, 123.45, 123.45, 543.21, 543.21),
                    sigfig = 5:1)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "When value has a sigfig attribute, the sigfig attribute
   of x at positions i are replaced by the sigfig attribute of value",
  {
    cal <- as_calculated(rep(123.45, 5),
                         sigfig = 5:1)
    cal[c(1, 4:5)] <- set_sigfig(cal[c(1, 4:5)], 5)
    expect_equal(
      cal,
      as_calculated(rep(123.45, 5),
                    sigfig = c(5, 4, 3, 5, 5))
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "For calculated values, when value has no sigfig attribute, the value of x 
   is replaced with no change to the sigfig of x",
  {
    cal <- as_calculated(rep(123.45, 5),
                         sigfig = 5:1)
    cal[c(1, 4:5)] <- 543.21
    expect_equal(
      cal,
      as_calculated(c(543.21, 123.45, 123.45, 543.21, 543.21),
                    sigfig = 5:1)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Replacement works with named vectors: measured",
  {
    m <- as_measured(rep(123.45, 5),
                     precision = 2:-2)
    names(m) <- letters[1:5]
    m[c("a", "d", "e")] <- 543.21
    expect_equal(
      m,
      {
        m2 <- as_measured(c(543.21, 123.45, 123.45, 543.21, 543.21),
                          precision = 2:-2)
        names(m2) <- letters[1:5]
        m2
      }
    )
  }
)

test_that(
  "Replacement works with named vectors: calculated",
  {
    m <- as_calculated(rep(123.45, 5),
                       sigfig = 5:1)
    names(m) <- letters[1:5]
    m[c("a", "d", "e")] <- 543.21
    expect_equal(
      m,
      {
        m2 <- as_calculated(c(543.21, 123.45, 123.45, 543.21, 543.21),
                            sigfig = 5:1)
        names(m2) <- letters[1:5]
        m2
      }
    )
  }
)