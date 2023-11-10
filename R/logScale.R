#' @title Log scale Control UI
#'
#' @description Create a log scale control UI.
#'
#' \preformatted{
#' ## Example
#' logScaleUI("logscale")
#' }
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices Default NULL, list of values to select from.
#' @param selected Default NULL, the initially selected value.
#'
#' @return A log scale control UI
#'
#' @seealso [logScaleServer()]
#'
#' @export
logScaleUI <- function(id, label = "Log scale", choices = NULL,
                       selected = NULL) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("logScale"),
      label = label,
      choices = choices,
      selected = selected
    )
  )
}

#' @title Log Scale Control Server
#'
#' @description Create a log scale control server.
#'
#' \preformatted{
#' ## Example
#' logScaleServer("logscale")
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices c("force", "log"), a string vector specifying choices
#' @param selected NULL, a string vector specifying selected choices
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [logScaleUI()]
#'
#' @export
logScaleServer <- function(id, choices = c("force", "log"), selected = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        updateSelectInput(
          session,
          inputId = "logScale",
          choices = choices,
          selected = selected
        )
      })
      logScale <- reactive({
        validate(need(input$logScale, message = FALSE))
        input$logScale
      })
      return(logScale)
    }
  )
}
