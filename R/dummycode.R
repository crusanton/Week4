#  Documenting Code Assignment
#  Rick Lunkenheimer

#' Read the file with the FARS data
#'
#' Reads in the file name of the FARS data and then stores it in a data frame.
#'
#' @param filename The name of the file to be read
#
#' @return A dataframe of with the data that was read in, or an error if it is an invalid file name.
#'
#' @importFrom dplyr tbl_df %>%
#' @importFrom readr read_csv
#'
#' @examples
#' fars_read(accident_2015.csv.bz2)
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Makes a file name for the years requested
#'
#' The main function to create the file name for the year.
#'
#' @param year The year of the data you want to analyze as an integer.
#'
#' @return A string of the correct file name for that year. No error will be returned as long as you send an integer, or string that can be converted to an integer as the parameter.
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' Read in the years to be collected and organized into month and year.
#'
#' Creates a data frame with month and year if the file name is correct.
#'
#' @param years The year or years you want to check data for.
#'
#' @return A dataframe of with the data for all the years selected, or an error if it is an invalid file name or invalid year.

#' @importFrom dplyr mutate select %>%
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_read_years(2015)
#' @export
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


#' Summarize the data.
#'
#' Creates a data frame with a summary of the years specified.
#'
#' @param years A list of the years you want to summarize.
#'
#' @return A data frame with the summary by year, then month. No error returned in this function, as validation is done elsewhere.

#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Display map of accidents by state and year
#'
#' Creates a state map with the accidents plotted with latitud and longitude.
#'
#' @param state.num  The integer for a state code
#' @param year The year of the data for that state
#'
#' @return No value returned, but creates a map with the accidents for that year. An error will be returned if you give an invalid state number, or if there are no accidents for the parameters given.
#'
#' @export
#' @importFrom dplyr filter %>%
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(17, 2015)
#'
#' @export
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


