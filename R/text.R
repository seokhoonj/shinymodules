#' @title Create a text input control UI
#'
#' @description
#' Create an input control ui for entry of unstructured text values
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#'
#' @return A text input UI
#'
#' @seealso [textServer()]
#'
#' @export
textUI <- function(id, label = "", value = "") {
  ns <- NS(id)
  tagList(
    textInput(ns("text"), label = label, value = value)
  )
}

#' @title Create a text input control Server
#'
#' @description
#' Create an input control server for entry of unstructured text values
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [textUI()]
#'
#' @export
textServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      text <- reactive({
        validate(need(input$text, message = FALSE))
        input$text
      })
      return(text)
    }
  )
}
