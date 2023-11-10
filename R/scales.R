#' @title Scales Control UI
#'
#' @description Create a scales control UI.
#'
#' \preformatted{
#' ## Example
#' scalesUI("scales")
#' }
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices Default NULL, list of values to select from.
#' @param selected Default NULL, the initially selected value.
#'
#' @return A scale control UI
#'
#' @seealso [scalesServer()]
#'
#' @export
scalesUI <- function(id, label = "Scales", choices = NULL, selected = NULL) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("scales"), label = label, choices = choices, selected = selected
    )
  )
}

#' @title Scales Control Server
#'
#' @description Create a scales control server.
#'
#' \preformatted{
#' ## Example
#' scalesServer("scales")
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices c("fixed", "free_x", "free_y", "free"), a string vector specifying choices
#' @param selected NULL, a string vector specifying selected choices
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [scalesUI()]
#'
#' @export
scalesServer <- function(id, choices = c("fixed", "free_x", "free_y", "free"),
                         selected = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        updateSelectInput(
          session,
          inputId = "scales",
          choices = choices,
          selected = selected
        )
      })
      scales <- reactive({
        validate(need(input$scales, message = FALSE))
        input$scales
      })
      return(scales)
    }
  )
}
