 #' Function for reading your csv files.
 #'
 #' This function reads your csv file (can be in bz2 zipped format also). If the filename you want to open does not exist,
 #' function returns error message and stops.
 #'
 #' @details filename you are giving to the function fars_read (or the path) must be
 #' correct and the file must exist. Otherwise the function returns error and stops.
 #'
 #' @param filename file to be read
 #'
 #' @return This function will return the loaded data in the form of a tibble.
 #'
 #' @importFrom dplyr tbl_df
 #'
 #' @importFrom readr read_csv
 #'
 #' @examples
 #'
 #' fars_read("/accident_2013")
 #' fars_read("somefilethatdoesnotexist.csv")
 #'
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

#' Create a name for your file
#'
#' This function gives a name to your compressed csv file. The form of the name is
#' accident_[wantedYear].csv.bz2.
#'
#' @details this function uses 'sprintf' function from base package.
#'
#' @param year - can be either in character or integer form, but it has to be a number. See examples below.
#'
#' @return this function will return a string of your new filename with specified year
#'
#' @examples
#'
#' make_filename(2015)
#' make_filename("2017")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Open the file with specifying the list of years
#'
#' This function allows you to open the accidents files for wanting year(s).
#'
#' @import dplyr
#'
#' @param years - variable containing only one year or many years (as list)
#'
#' @return tibble(s) showing month and year variables only.
#'
#' @details if there is no file which contains a year that you submitted, a warning will be shown and NULL will be returned
#' because the file with the given year does not exist
#'
#' @examples
#'
#' fars_read_years(list(2014,2015))
#' fars_read_years(list(2013))
#'
#' @export
fars_read_years <- function(years) {
  MONTH=NULL
  year=NULL
  lapply(years, function(year) {
    file <- make_filename(2014)
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

#' Summarizes your data per year and month
#'
#' This function opens your files regarding the specific year and also does summarization. It
#' will create one file using all files you have given the function  by simply binding the rows together.
#' Then it groups the data by year and month and counts observations per group (month of a year).
#' In the end it will give you the summarized data. Each column will represent the year you specified with the number of observations per
#' specific month.
#'
#' @import dplyr
#'
#' @importFrom tidyr spread
#'
#' @details It uses function 'spread' from tidyr package to describe the data per year.  If you submit a non-existing year, the same errors as for fars_read_years function
#' will appear.
#'
#' @param years numerical variable - can be a list
#'
#' @return a tibble where aach column represents the year you specified with the number of observations per
#' specific month.
#'
#' @examples
#'
#' fars_summarize_years(list(2013,2014,2015))
#' fars_summarize_years(list(2013))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots the map of the USA and the places with accidents according to the state and year you give
#'
#' This function will read the file according to the year you give and according to the state number you give.
#' It will plot accidents for the year and the state.
#'
#' @importFrom dplyr filter
#'
#' @importFrom maps map
#'
#' @importFrom graphics points
#'
#' @param state.num - the number of the state you want to inspect and plot. year - numeric. from which year you
#' want to plot your data?
#'
#' @param year -  numerical variable
#'
#' @details this function uses 'filter' function from dplyr, 'map' function from maps and 'points'
#' function from graphics package. All LATITUDEs > 90 and LONGITUDEs >900 will not be
#' plotted. If your there is no data for the given state in the given year, an error message will
#' be printed. If there is no state with given state.num, the function will stop with an error printed.
#'
#' @return the function will return a world map but with limited longitudes and latitutes shown
#' (only the ones including state regarding the state.num argument). Each dot on the map will represent
#' a accident which happend in the corresponding year and corresponding state in the USA.
#'
#' @examples
#'
#' fars_map_state(4, 2014)
#' fars_map_state(6, 2014)
#'
#' @export
fars_map_state <- function(state.num, year) {
  STATE=NULL
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
