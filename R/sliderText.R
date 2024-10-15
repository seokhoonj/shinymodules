#' @title Slider Text Input Control UI
#'
#' @description Create a slider text input ui.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices Default c(0, 1), list of values to select from.
#' @param selected Default C(0, 1), the initially selected value.
#' @param hide_min_max Hides min and max labels.
#'
#' @return A slider text input UI
#'
#' @seealso [sliderTextServer()], [dynSliderTextServer()]
#'
#' @export
sliderTextUI <- function(id, label = "Period", choices = c(0, 1),
                         selected = c(0, 1), hide_min_max = FALSE) {
  ns <- NS(id)
  tagList(
    shinyWidgets::sliderTextInput(
      ns("sliderText"), label = label, choices = choices, selected = selected,
      hide_min_max = hide_min_max
    )
  )
}

#' @title Slider Text Input Control Server
#'
#' @description Create a slider text input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [sliderTextUI()]
#'
#' @export
sliderTextServer <- function(id, choices, selected = NULL, reverse = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        if (reverse)
          choices <- rev(choices)
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "sliderText",
          choices = choices,
          selected = if (is.null(selected)) {
            c(choices[1L], choices[length(choices)])
          } else {
            selected
          }
        )
      })
      sliderText <- reactive({
        validate(need(input$sliderText, message = FALSE))
        input$sliderText
      })
      return(sliderText)
    }
  )
}

#' @title Dynamic Slider Text Input Control Server
#'
#' @description Create a dynamic slider text input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data
#' @param column A data column for unique list of values
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [sliderTextUI()]
#'
#' @export
dynSliderTextServer <- function(id, data, column, selected = NULL,
                                reverse = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(data(), {
        choices <- sort(unique(data()[[column]]))
        if (reverse)
          choices <- rev(choices)
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "sliderText",
          choices = choices,
          selected = if (is.null(selected)) {
            c(choices[1L], choices[length(choices)])
          } else {
            selected
          }
        )
      })
      sliderText <- reactive({
        validate(need(input$sliderText, message = FALSE))
        input$sliderText
      })
      return(sliderText)
    }
  )
}
