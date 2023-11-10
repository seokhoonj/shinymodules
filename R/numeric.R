#' @title Numeric Input Control UI
#'
#' @description Create a numeric input control UI.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#'
#' @return A numeric input control UI
#'
#' @seealso [numericServer()], [dynNumericServer()]
#'
#' @export
numericUI <- function(id, label = "", value = 0, min = 0, max = 1, step = 1) {
  ns <- NS(id)
  numericInput(
    ns("numeric"), label = label, value = value, min = min, max = max, step = 1
  )
}

#' @title Numeric Input Control Server
#'
#' @description Create a numeric input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param value Initial value.
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [numericUI()]
#'
#' @export
numericServer <- function(id, value = 0, min = 0, max = 1, step = 1) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        updateNumericInput(
          session = session,
          inputId = "numeric",
          value = value,
          min = min,
          max = max,
          step = step
        )
      })
      numeric <- reactive({
        validate(need(input$numeric, message = FALSE))
        input$numeric
      })
      return(numeric)
    }
  )
}

#' @title Dynamic Numeric Input Control Server
#'
#' @description Create a dynamic numeric input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data.
#' @param column A data column for calculating range of values.
#' @param value Initial value.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [numericUI()]
#'
#' @export
dynNumericServer <- function(id, data, column, value = 0) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(data(), {
        column_range <- unique(data()[[column]])
        col_min <- min(column_range, na.rm = TRUE)
        col_max <- max(column_range, na.rm = TRUE)
        updateNumericInput(
          session,
          inputId = "numeric",
          value = value,
          min = col_min,
          max = col_max
        )
      })
      numeric <- reactive({
        validate(need(input$numeric, message = FALSE))
        input$numeric
      })
      return(numeric)
    }
  )
}
