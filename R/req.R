#' Require a data object with at least one row
#'
#' A convenience wrapper around [shiny::req()] that checks whether
#' the given object both exists and contains at least one row.
#' This is useful inside reactive expressions or render functions
#' to prevent downstream errors when subsetting empty or NULL data.
#'
#' @param x A data frame, tibble, data.table, or any object for which `nrow(x)` is defined.
#'
#' @return Invisibly returns `x` if the requirement is met; otherwise halts the reactive
#'   execution with a silent error (as [shiny::req()] does).
#'
#' @seealso [shiny::req()]
#'
#' @examples
#' \dontrun{
#' output$plot <- renderPlot({
#'   df <- get_data()
#'   req_has_rows(df)
#'   plot(df$x, df$y)
#' })
#' }
#'
#' @export
req_has_rows <- function(x) shiny::req(x, nrow(x) > 0)

#' Require an object with positive length
#'
#' A convenience wrapper around [shiny::req()] for validating that the
#' input object both exists and has a positive length. This is useful
#' when working with selections, vectors, or other inputs that may be
#' `NULL` or empty during the initial Shiny render cycle.
#'
#' @param x A vector-like object.
#'
#' @return Invisibly returns `x` if the requirement is satisfied; otherwise
#'   stops reactive execution with a silent exception.
#'
#' @seealso [shiny::req()]
#'
#' @examples
#' \dontrun{
#' observeEvent(input$choices, {
#'   req_has_length(input$choices)
#'   do_something(input$choices)
#' })
#' }
#'
#' @export
req_has_length <- function(x) shiny::req(x, length(x) > 0)

