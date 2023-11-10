#' @title Date UI
#'
#' @description Create a date UI.
#'
#' \preformatted{
#' ## Example
#' dateUI("date", label = "Date", value = Sys.Date(), format = "yyyy-mm-dd")
#' }
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value A date value.
#' @param format A date format value.
#'
#' @return A date UI
#'
#' @export
dateUI <- function(id, label = "Date", value = Sys.Date(),
                   format = "yyyy-mm-dd") {
  ns <- NS(id)
  tagList(
    dateInput(ns("date"), label = label, value = value, format = format)
  )
}

#' @title Date Server
#'
#' @description Create a date server.
#'
#' \preformatted{
#' ## Example
#'   dateServer("date")
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @return The return value, if any, from executing the module server function.
#'
#' @seealso [dateUI()]
#'
#' @export
dateServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      date <- reactive({
        validate(need(input$date, message = FALSE))
        as.Date(input$date)
      })
      return(date)
    }
  )
}
