#' @title Action Button UI
#'
#' @description Create an action button UI.
#'
#' @details
#' \preformatted{
#' ## Example
#' actionUI("action", label = "Run")
#' }
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#'
#' @return An action button UI
#'
#' @seealso [actionServer()]
#'
#' @export
actionUI <- function(id, label = "Run") {
  ns <- NS(id)
  tagList(
    actionButton(ns("action"), label = label)
  )
}

#' @title Action Button Server
#'
#' @description Create an action button server.
#'
#' @details
#' \preformatted{
#' ## Example
#' actionServer("action")
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [actionUI()]
#'
#' @export
actionServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      action <- eventReactive(input$action, {
        input$action
      })
      return(action)
    }
  )
}
