devtools::document(getwd())
devtools::test(getwd())
# devtools::check(getwd(), args = "--as-cran")
devtools::install_local(getwd())

library(imprecise)
m <- as_measured(rep(123.45, 5),
                 precision = 2:-2)

M <- data.frame(m = m)

M

x <- as_calculated(rep(123.45, 5),
                   sigfig = 5:1)

M$x <- x

M


x <- set_label(1:3, "Hello")
x <- set_units(x, "mL")
x
