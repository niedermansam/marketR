#' Return a sentence describing the given data
#'
#' @param data A dataframe to summarize.
#' @param dimension The dimension (or grouping variable).
#' @param metric The data to summarize.
#' @param subject The subject of the sentence (e.g. users, pageviews, etc.)
#' @param action The action of the subject (e.g. made up, spent, etc.)
#' @param object Noun phrase to include in predicate (e.g. "of total traffic", "of all users", etc.)
#' @param label The label for the metric (e.g. \%, minutes, etc.)
#'
#' @return A sentence describing the data provided.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export describe_data
describe_data <- function(data,
                         dimension,
                         metric,
                         subject="users",
                         action="made up",
                         label="%",
                         object = "of total traffic"){
  output <- ""

  for(i in 1:nrow(data)){

    current_dimension <- data[[dimension]][i]
    current_value <- data[[metric]][i]

    if(i == 1){
      current_dimension %<>% stringr::str_to_sentence()
    }


    output <- paste0(output,
                     current_dimension, " ",
                     subject, " ",
                     action, " ",
                     current_value,
                     label, " ",
                     predicate,
                     ifelse(i == nrow(data), '.', ''),
                     ifelse(i == nrow(data) - 1, ", and ", ""),
                     ifelse(i < nrow(data) - 1, ", ", ""))
  }

  output
}
