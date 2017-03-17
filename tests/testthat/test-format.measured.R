context("format.measured")

test_that(
  "Format measured values",
  {
    m <- as_measured(rep(123.45, 5),
                     precision = 2:-2)
    expect_equal(
      format(m),
      c("100.00", "120.00", "123.00", "123.50", "123.45")
    )
  }
)

test_that(
  "Format calculated values",
  {
    cal <- as_calculated(rep(123.45, 5),
                         sigfig = 1:5)
    expect_equal(
      format(cal),
      c("100.00", "120.00", "123.00", "123.40", "123.45")
    )
  }
)