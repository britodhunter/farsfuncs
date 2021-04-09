#' Read in File
#'
#' Simple function to read in a csv file by path and produce data table read-out in console
#'     with information on number of rows and column header names.
#'
#' @param filename A user specific file path of the .csv file to be imported into R environment
#'
#' @return This function creates a data frame of the subclass tibble from the .csv file path provided.
#'     A readout will be provided in the console with information on the rows and columns included.
#'
#' @note Error will appear if no file path name is provided or if file path does not exist on user's computer.
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

#' Create file Name based on year
#'
#' This function prints to the console the inputted year within a common file name path.
#'
#' @param year The year the user would like printed in the file name tag.
#'
#' @return This function will print the file name path with the year specified in the function.
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Create Month & Year Tibble
#'
#' This function creates a tibble with a 'MONTH' column and a 'year' column.
#'
#' @param years The year of the data the user would like be used in the function.
#'
#' @return A summary of the tibble will be printed to the console, with information on the two columns and number of rows.
#'
#' @note If a year is entered into the function that cannot be located on the user's computer,
#'          an error message will appear.
#'
#' @import dplyr
#'
#'
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

#' Create Number of Rows by Month in Specified Year
#'
#' This function creates a tibble with a 'MONTH' column and a 'year' column that returns the
#'   number of rows for each month (1-12) in the year inputted to the function.
#'
#' @param years The year of the data the user would like be used in the function.
#'
#' @return A summary of the tibble will be printed to the console, with one column indicating months 1 (January) -
#'     12 (December), and a second column, titled with 'years' parameter. The value in the second column is the number of rows
#'     found for that month in that year within the dataset.
#'
#' @note If a year is entered into the function that cannot be connected to a dataset located on the user's computer,
#'          an error message will appear.
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Create Map of Accidents in Each State
#'
#' This function creates a map graph of a given state and plots the location of accidents within that state for the specified year.
#'
#' @param state.num An integer value assigned to each state or territory within the United States (1-56).
#' @param year The year the user would like printed in the file name tag.
#'
#' @return A plot of the state map and marks for recorded accidents by geograhical location (longitude and latitude).
#'
#' @note If a state number is entered that does not have data or year is entered into the function that cannot be connected to a dataset located on the user's computer,
#'          an error message will appear. If there are no accidents within the state with the year specified, an error will appear.
#'
#' @import dplyr
#' @import maps
#' @import graphics
#'
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
