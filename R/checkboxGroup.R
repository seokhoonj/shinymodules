#' @title Checkbox Group UI
#'
#' @description Create a checkbox group UI.
#'
#' @details
#' \preformatted{
#' ## Example
#' checkboxGroupUI("checkboxgroup")
#' }
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#' @param value A boolean value for All/None value (All: TRUE, None: FALSE)
#' @param inline If TRUE, render the choices inline (i.e. horizontally)
#'
#' @return A checkbox group UI
#'
#' @seealso [checkboxGroupServer()]
#'
#' @export
checkboxGroupUI <- function(id, label = NULL, choices = character(0),
                            selected = character(0), value = FALSE,
                            inline = FALSE) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(
      ns("checkboxGroup"), label, choices = choices, selected = selected,
      inline = inline
    ),
    div(
      checkboxInput(ns("checkbox"), label = "All/None", value = value),
      style = "color: #FB8072"
    )
  )
}

#' @title Checkbox group Server
#'
#' @description Create a checkbox group server.
#'
#' @details
#' \preformatted{
#' ## Example
#' checkboxGroupServer("checkboxgroup", choices, selected, reverse = FALSE)
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [checkboxGroupUI()]
#'
#' @export
checkboxGroupServer <- function(id, choices, selected = NULL, reverse = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        if (reverse)
          choices <- rev(choices)
        updateCheckboxGroupInput(
          session,
          inputId = "checkboxGroup",
          choices = choices,
          selected = if (input$checkbox) choices else selected
        )
      })
      checkboxGroup <- reactive({
        validate(need(input$checkboxGroup, message = FALSE))
        input$checkboxGroup
      })
      return(checkboxGroup)
    }
  )
}

#' @title Dynamic Checkbox Group Server
#'
#' @description Create a dynamic checkbox group server.
#'
#' @details
#' \preformatted{
#' ## Example
#' dynCheckboxGroupServer("checkboxgroup", data, column, selected, reverse = FALSE)
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data
#' @param column A data column for unique list of values
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [checkboxGroupUI()]
#'
#' @export
dynCheckboxGroupServer <- function(id, data, column, selected = NULL,
                                   reverse = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent({data(); input$checkbox}, {
        choices <- sort(unique(data()[[column]]))
        if (reverse)
          choices <- rev(choices)
        updateCheckboxGroupInput(
          session,
          inputId = "checkboxGroup",
          choices = choices,
          selected = if (input$checkbox) choices else selected
        )
      })
      checkboxGroup <- reactive({
        # validate(need(data(), message = FALSE)) # it causes a plot refresh
        validate(need(input$checkboxGroup, message = FALSE))
        input$checkboxGroup
      })
      return(checkboxGroup)
    }
  )
}
