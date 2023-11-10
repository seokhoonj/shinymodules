#' @title Multiselect Control UI
#'
#' @description Create a multiselect control UI.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#' @param options List of options passed to multi (enable_search = FALSE for disabling the search bar for example).
#' @param width width The width of the input, e.g. `400px`, or `100%`.
#'
#' @return A multiselect control UI.
#'
#' @seealso [multiServer()], [dynMultiServer()]
#'
#' @export
multiUI <- function(id, label = "", choices = character(0),
                    selected = character(0), options = NULL, width = NULL) {
  ns <- NS(id)
  tagList(
    shinyWidgets::multiInput(
      inputId = ns("multi"),
      label = label,
      choices = choices,
      selected = selected,
      options = options,
      width = width
    )
  )
}

#' @title Multiselect Control Server
#'
#' @description Create a multiselect control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [multiUI()]
#'
#' @export
multiServer <- function(id, choices = character(0), selected = character(0)) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        shinyWidgets::updateMultiInput(
          session,
          inputId = "multi",
          choices = choices,
          selected = selected
        )
      })
      multi <- reactive({
        validate(need(input$multi, message = FALSE))
        input$multi
      })
      return(multi)
    }
  )
}

#' @title Dynamic Multiselect Control Server
#'
#' @description Create a dynamic multiselect control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data.
#' @param column A data column for unique list of values.
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [multiUI()]
#'
#' @export
dynMultiServer <- function(id, data, column, selected = character(0),
                           reverse = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(data(), {
        choices <- sort(unique(data()[[column]]))
        if (reverse)
          choices <- rev(choices)
        shinyWidgets::updateMultiInput(
          session,
          inputId = "multi",
          choices = choices,
          selected = selected
        )
      })
      multi <- reactive({
        validate(need(input$multi, message = FALSE))
        input$multi
      })
      return(multi)
    }
  )
}
