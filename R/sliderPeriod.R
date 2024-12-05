#' @title Slider Period Input Control UI
#'
#' @description Create a slider period input ui.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices Default c(0, 1), list of values to select from.
#' @param selected Default C(0, 1), the initially selected value.
#' @param hide_min_max Hides min and max labels.
#'
#' @return A slider period input UI
#'
#' @seealso [sliderPeriodServer()], [dynSliderPeriodServer()]
#'
#' @export
sliderPeriodUI <- function(id, label = "Period", choices = c(0, 1),
                           selected = c(0, 1), hide_min_max = FALSE) {
  ns <- NS(id)
  tagList(
    shinyWidgets::sliderTextInput(
      ns("sliderPeriod"), label = label, choices = choices, selected = selected,
      hide_min_max = hide_min_max
    )
  )
}

#' @title Slider Period Input Control Server
#'
#' @description Create a slider period input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices List of values to select from.
#' @param fun An expression function for `choices`
#' @param invfun An inverse function for `fun`
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [sliderPeriodUI()]
#'
#' @export
sliderPeriodServer <- function(
    id, choices,
    fun = function(x) format(x, "%y-%m"),
    invfun = function(x) as.Date(paste0(x, "-01"), "%y-%m-%d"),
    selected = NULL, reverse = FALSE
  ) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        if (reverse)
          choices <- rev(choices)
        if (!is.null(fun))
          choices <- fun(choices)
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "sliderPeriod",
          choices = choices,
          # selected = selected
          selected = if (is.null(selected)) c(min(choices), max(choices)) else selected
        )
      })
      sliderPeriod <- reactive({
        validate(need(input$sliderPeriod, message = FALSE))
        if (!is.null(invfun)) {
          invfun(input$sliderPeriod)
        } else {
          input$sliderPeriod
        }
      })
      return(sliderPeriod)
    }
  )
}

#' @title Dynamic Slider Period Input Control Server
#'
#' @description Create a dynamic slider period input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data
#' @param column A data column for unique list of values
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#' @param fun An expression function for period.
#' @param invfun An inverse function for original value of period.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [sliderPeriodUI()]
#'
#' @export
dynSliderPeriodServer <- function(
    id, data, column, selected = NULL, reverse = FALSE,
    fun = function(x) format(x, "%y-%m"),
    invfun = function(x) as.Date(paste0(x, "-01"), "%y-%m-%d")
  ) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(data(), {
        choices <- sort(unique(data()[[column]]))
        if (reverse)
          choices <- rev(choices)
        if (!is.null(fun))
          choices <- fun(choices)
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "sliderPeriod",
          choices = choices,
          # selected = selected
          selected = if (is.null(selected)) c(min(choices), max(choices)) else selected
        )
      })
      sliderPeriod <- reactive({
        validate(need(input$sliderPeriod, message = FALSE))
        if (!is.null(invfun)) {
          invfun(input$sliderPeriod)
        } else {
          input$sliderPeriod
        }
      })
      return(sliderPeriod)
    }
  )
}
