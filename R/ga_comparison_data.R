#' Get Google Analytics Comparison Data
#'
#' A wrapper function around \code{googleAnalyticsR::google_analytics()} that returns
#' a list with data from three time periods: the time period requested by \code{date_range},
#' the time period 1 year prior to \code{date_range} and the time period immediately preceding
#'  \code{date_range}.
#'
#' @param viewId The ID of the Google Analytics View to retrieve data from.
#' @param date_range The date range to retrieve data from. Takes a vector of two dates
#'    with the format "YYYY-MM-DD"
#' @param metrics Metrics to retrieve from Google Analytics
#' @param dimensions Dimensions to retrieve from Google Analytics
#' @param anti_sample Set to \code{TRUE} to disallow sampling.
#'
#' @return A list object with data for the current date range,
#'    the same date range 1 year prior, and the previous period.
#'
#'
#'
#' @export
ga_comparison_data <- function(viewId,
                               date_range = c(Sys.Date(), Sys.Date() - 1),
                               metrics = c('users', 'newUsers'),
                               dimensions = c("date", "deviceCategory"),
                               anti_sample = TRUE) {
  reportDuration <-
    as.duration(interval(date_range[[1]], date_range[[2]])) / ddays(1)

  if(!is.vector(date_range)) stop("date_range should be a vector with a length of 2.")

  data <- list()

  # Get Current Data #####
  data$current <-
    google_analytics(
      viewId,
      date_range = dateRange,
      metrics = metrics,
      dimensions = dimensions,
      anti_sample = anti_sample
    )

  # Get Data from 1 Year Ago #########

  yearAgoDateRange <- date_range

  year(yearAgoDateRange[[1]]) <- year(yearAgoDateRange[[1]]) - 1
  year(yearAgoDateRange[[2]]) <- year(yearAgoDateRange[[2]]) - 1

  data$year_ago <- google_analytics(
    viewId,
    date_range = yearAgoDateRange,
    metrics = metrics,
    dimensions = dimensions,
    anti_sample = TRUE
  )

  # Get Data from the Previous Period #########


  prevPeriod <- date_range
  prevPeriod[[1]] <- prevPeriod[[1]] - reportDuration
  prevPeriod[[2]] <- prevPeriod[[2]] - reportDuration

  data$prev_period  <-
    google_analytics(
      viewId,
      date_range = prevPeriod,
      metrics = metrics,
      dimensions = dimensions,
      anti_sample = TRUE
    )

  data
}
