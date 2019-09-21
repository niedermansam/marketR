#' Format a Date for Display.
#'
#' @param date The date to be formatted (either a date object
#'    or a string with the format "YYYY-MM-DD").
#'
#' @param show_year Whether or not to display year in output. Default: \code{TRUE}.
#'
#' @param show_month Whether or not to display month in output. Default: \code{TRUE}.
#'
#' @param show_wday Whether or not to display the day of the week
#'    in output. Default: \code{FALSE}.
#'
#' @return A string that represents the date supplied in \code{date}.
#' @examples
#' prettyDate("9-20-2019")
#' # September 20th, 2019
#'
#' prettyDate("9-20-2019", show_wday=T)
#' # Friday, September 20th, 2019
#'
#' @export pretty_date
pretty_date <- function(date,
                        show_year = TRUE,
                        show_month = TRUE,
                        show_wday = FALSE) {
  date <- lubridate::as_date(date)

  year <- date %>% lubridate::year()

  month <- date %>%
    lubridate::month(label = TRUE, abbr = FALSE) %>%
    toString()

  wday <- date %>%
    lubridate::wday(label = TRUE, abbr = FALSE) %>%
    toString()

  day <- date %>% lubridate::day()

  day %<>% stringr::str_replace("1$", "1st") %>%
    stringr::str_replace("2$", "2nd") %>%
    stringr::str_replace("3$", "3rd") %>%
    stringr::str_replace("(\\d)$", "\\1th")

  paste0(
    ifelse(show_wday, paste0(wday, ", "), ""),
    ifelse(show_month, paste0(month, " "), ""),
    day,
    ifelse(show_year, paste0(", ", year), "")
  )

}



#' Format the display of a date period.
#'
#' @param date1,date2 The dates to be formatted (either date objects
#'    or strings with the format "YYYY-MM-DD").
#' @param show_wday Whether or not to display the
#'     day of the week. Default: \code{FALSE}
#'     dates. DEFAULT: \code{" - "}
#'
#' @return A string with both dates provided formatted.
#'
#'
#' @examples
#'prettyDateRange("9-20-2019", "09-21-2019", show_wday=FALSE)
#'# September 20th - 21st, 2019
#'
#'prettyDateRange("9-20-2019", "10-20-2019", show_wday=FALSE)
#'# September 20th - October 20th, 2019
#'
#'prettyDateRange("9-20-2019", "10-20-2020", show_wday=FALSE)
#'# September 20th, 2019 - October 20th, 2020

#'
#' @export pretty_date_range
pretty_date_range <-
  function(date1,
           date2,
           separator = " - ",
           show_wday = FALSE) {
    if (lubridate::year(date1) == lubridate::year(date2)) {
      if (lubridate::month(date1) == lubridate::month(date2)) {
        formattedDate1 <-  date1 %>%
          marketR::pretty_date(show_year = FALSE,
                              show_wday = show_wday)

        formattedDate2 <- date2 %>%
          marketR::pretty_date(show_month = FALSE,
                              show_wday = show_wday)

      } else {
        formattedDate1 <-  date1 %>%
          marketR::pretty_date(show_year = FALSE,
                              show_wday = show_wday)

        formattedDate2 <- date2 %>%
          marketR::pretty_date(show_wday = show_wday)
      }

    } else {
      formattedDate1 <-  date1 %>%
        marketR::pretty_date(show_wday = show_wday)

      formattedDate2 <- date2 %>%
        marketR::pretty_date(show_wday = show_wday)

    }

    output <- paste(formattedDate1,
                    formattedDate2,
                    sep = separator) %>%
      stringr::str_replace_all(' 0', " ")

    output

  }
