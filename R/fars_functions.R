#' Reads data from a FARS data file
#'
#' Reads data from a FARS data file.
#'
#' Generates an error if the file does not exist.
#'
#' @param filename A character string giving the name of the file to read.
#'
#' @return Returns a data frame containing the FARS data.
#'
#' @export
#'
#' @examples
#' data <- fars_read(filename)
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Makes a FARS filename for a specified year
#'
#' @param year An integer specifying the year.
#'
#' @return A character string containing the filename.
#' @export
#'
#' @examples
#' filename <- make_filename(year)
#' filename <- make_filename(2015)
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads and extracts data for calculating crash counts
#'
#' Reads FARS data for the specified years and extracts a list containing a data frame
#' for each year. The list of data frames is in a format suitable for internal use by
#' the \link{fars_summarize_years} function.
#'
#' @param years Integer vector specifying year(s) to read.
#'
#' @return A list of data frames. Each data frame contains the variables \code{year} and
#' \code{MONTH} with a row for each fatal crash that occurred in that year and month.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' data_list <- fars_read_years(2013:2015)
#'
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize FARS crash data by year and month
#'
#' @param years Integer vector specifying year(s) to summarize.
#'
#' @return A data frame with counts of fatal crashes by year and month.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' summary <- fars_summarize_years(2013:2015)
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Maps FARS crash locations by state and year
#'
#' Plots a map showing FARS crash locations for a specified state and year.
#'
#' The \code{maps} package must be loaded so that the the \code{stateMapEnv} object is
#' available. (The \code{maps} package does not export \code{stateMapEnv}.) Do this by
#' running \code{library(maps)} .
#'
#' Crashes with \code{LOGITUD > 900} or \code{LATITUDE > 90} are excluded.
#'
#' Invalid \code{state} or \code{year} values will produce errors.
#'
#' @param state.num Integer state code. See \url{https://crashstats.nhtsa.dot.gov/Api/Public/Publication/812316} (PDF), p. 24 for a list.
#' @param year Integer year.
#'
#' @return NULL
#' @export
#'
#' @examples
#' fars_map_state(19, 2015) # 19 is Iowa
#'
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
