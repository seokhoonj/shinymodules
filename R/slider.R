#' @title Slider Input Control UI
#'
#' @description Create slider input control UI.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param ticks `FALSE` to hide tick marks, `TRUE` to show them according to some simple heuristics.
#' @param min,max The minimum and maximum values (inclusive) that can be
#'   selected.
#' @param value The initial value of the slider, either a number, a date
#'   (class Date), or a date-time (class POSIXt). A length one vector will
#'   create a regular slider; a length two vector will create a double-ended
#'   range slider. Must lie between `min` and `max`.
#' @param step Specifies the interval between each selectable value on the
#'   slider. Either `NULL`, the default, which uses a heuristic to determine the
#'   step size or a single number. If the values are dates, `step` is in days;
#'   if the values are date-times, `step` is in seconds.
#' @param round `TRUE` to round all values to the nearest integer;
#'   `FALSE` if no rounding is desired; or an integer to round to that
#'   number of digits (for example, 1 will round to the nearest 10, and -2 will
#'   round to the nearest .01). Any rounding will be applied after snapping to
#'   the nearest step.
#' @param ticks `FALSE` to hide tick marks, `TRUE` to show them
#'   according to some simple heuristics.
#' @param animate `TRUE` to show simple animation controls with default
#'   settings; `FALSE` not to; or a custom settings list, such as those
#'   created using [animationOptions()].
#' @param sep Separator between thousands places in numbers.
#' @param pre A prefix string to put in front of the value.
#' @param post A suffix string to put after the value.
#' @param dragRange This option is used only if it is a range slider (with two
#'   values). If `TRUE` (the default), the range can be dragged. In other
#'   words, the min and max can be dragged together. If `FALSE`, the range
#'   cannot be dragged.
#' @param timeFormat Only used if the values are Date or POSIXt objects. A time
#'   format string, to be passed to the Javascript strftime library. See
#'   <https://github.com/samsonjs/strftime> for more details. The allowed
#'   format specifications are very similar, but not identical, to those for R's
#'   [base::strftime()] function. For Dates, the default is `"%F"`
#'   (like `"2015-07-01"`), and for POSIXt, the default is `"%F %T"`
#'   (like `"2015-07-01 15:32:10"`).
#' @param timezone Only used if the values are POSIXt objects. A string
#'   specifying the time zone offset for the displayed times, in the format
#'   `"+HHMM"` or `"-HHMM"`. If `NULL` (the default), times will
#'   be displayed in the browser's time zone. The value `"+0000"` will
#'   result in UTC time.
#'
#' @return A numeric input control UI
#'
#' @seealso [sliderServer()], [dynSliderServer()]
#'
#' @export
sliderUI <- function(id, label = "", min = 0, max = 1, value = c(0, 1),
                     step = NULL, round = FALSE, ticks = FALSE, animate = FALSE,
                     sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                     timezone = NULL, dragRange = TRUE) {
  ns <- NS(id)
  tagList(
    sliderInput(
      ns("slider"),
      label = label,
      min = min,
      max = max,
      value = value,
      step = step,
      round = round,
      ticks = ticks,
      animate = animate,
      sep = sep,
      pre = pre,
      post = post,
      timeFormat = timeFormat,
      timezone = timezone,
      dragRange = dragRange
    )
  )
}

#' @title Slider Input Control Server
#'
#' @description Create a slider input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param min,max The minimum and maximum values (inclusive) that can be
#'   selected.
#' @param value The initial value of the slider, either a number, a date
#'   (class Date), or a date-time (class POSIXt). A length one vector will
#'   create a regular slider; a length two vector will create a double-ended
#'   range slider. Must lie between `min` and `max`.
#' @param step Specifies the interval between each selectable value on the
#'   slider. Either `NULL`, the default, which uses a heuristic to determine the
#'   step size or a single number. If the values are dates, `step` is in days;
#'   if the values are date-times, `step` is in seconds.
#'
#' @return A select input control server
#'
#' @seealso [selectUI()]
#'
#' @export
sliderServer <- function(id, min = NULL, max = NULL, value = NULL,
                         step = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        updateSliderInput(
          session,
          inputId = "slider",
          min = min,
          max = max,
          value = value,
          step = step
        )
      })
      slider <- reactive({
        validate(need(input$slider, message = FALSE))
        input$slider
      })
      return(slider)
    }
  )
}

#' @title Dynamic Slider Input Control Server
#'
#' @description Create a dynamic slider input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data.
#' @param column A data column for unique list of values.
#' @param value The initial value of the slider, either a number, a date
#'   (class Date), or a date-time (class POSIXt). A length one vector will
#'   create a regular slider; a length two vector will create a double-ended
#'   range slider. Must lie between min and max of the `column`.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [sliderUI()]
#'
#' @export
dynSliderServer <- function(id, data, column, value = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        validate(need(data(), message = FALSE))
        column_range <- range(data()[[column]], na.rm = TRUE)
        updateSliderInput(
          session,
          inputId = "slider",
          min = min(column_range),
          max = max(column_range),
          value = if (is.null(value)) column_range else value
        )
      })
      slider <- reactive({
        validate(need(input$slider, message = FALSE))
        input$slider
      })
      return(slider)
    }
  )
}
