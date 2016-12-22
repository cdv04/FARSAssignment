library(FARSAssignment)
context("functions")

test_that("fars_read returns a data frame", {
    filename <- system.file("extdata", "accident_2013.csv.bz2", package = "FARSAssignment")
    df <- fars_read(filename)
    expect_is(df, "data.frame")
})

test_that("fars_read generates an error if the file doesn't exist", {
    expect_error(fars_read("bogus"), "file 'bogus' does not exist")
})
