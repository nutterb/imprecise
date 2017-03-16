devtools::document(getwd())
devtools::test(getwd())
devtools::check(getwd(), args = "--as-cran")
