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

test_that("make_filename returns a proper filename", {
    expect_match(make_filename(2015), "accident_2015.csv.bz2")
})

test_that("fars_read_years returns a list of data frames", {
    result <- fars_read_years(2013:2014)
    expect_is(result, "list")
    expect_is(result[[1]], "data.frame")
})

test_that("fars_read_years generates a warning for invalid year", {
    expect_warning(fars_read_years(2000), "invalid year: 2000")
})

test_that("fars_summarize_years returns a data frame", {
    df <- fars_summarize_years(2013:2014)
    expect_is(df, "data.frame")
})

# TODO: figure out how to test fars_map_state
