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
  ns <- shiny::NS(id)
  shiny::tagList(
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
    fun      = function(x) format(x, "%y-%m"),
    invfun   = function(x) as.Date(paste0(x, "-01"), "%y-%m-%d"),
    selected = NULL,
    reverse  = FALSE
  ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shiny::observe({

        # make a local copy to avoid mutating original values
        vals <- choices

        # reverse the choices if needed
        if (reverse)
          vals <- rev(vals)

        # apply formatting function (e.g. "%y-%m")
        if (!is.null(fun))
          vals <- fun(vals)

        # if "selected" was not provided, default to full range
        target <- if (is.null(selected)) {
          c(min(vals), max(vals))
        } else {
          selected
        }

        # isolate() prevent triggering reactivity during comparison
        current <- shiny::isolate(input$sliderPeriod)

        # if the current slider value is already equal to what we
        # intend to set, skip updateSliderTextInput() completely.
        # This prevents UI "blinking" or re-drawing on initial load.
        if (!is.null(current) && length(current) == length(target) &&
            all(current == target)) {
          return(NULL)
        }

        # perform update only when necessary.
        shinyWidgets::updateSliderTextInput(
          session,
          inputId  = "sliderPeriod",
          choices  = vals,
          selected = target
        )
      })

      # reactive conversion (ui -> date objects)
      sliderPeriod <- shiny::reactive({
        shiny::req(input$sliderPeriod)
        if (!is.null(invfun)) {
          invfun(input$sliderPeriod)
        } else {
          input$sliderPeriod
        }
      })

      sliderPeriod
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
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # ------------------------------------------------------------------------------
      # Update slider choices and selected range whenever 'data()' changes
      # ------------------------------------------------------------------------------
      shiny::observeEvent(data(), {
        dt <- data()

        # Ensure valid data
        if (is.null(dt) || !nrow(dt))
          return(NULL)

        # Extract unique values from the specified column
        vals <- sort(unique(dt[[column]]))
        if (!length(vals))
          return(NULL)

        # Reverse order if requested
        if (reverse)
          vals <- rev(vals)

        # Apply user-provided formatting function
        choices <- if (!is.null(fun)) fun(vals) else vals

        # Current selected values already shown in the slider
        current <- shiny::isolate(input$sliderPeriod)

        # Determine what selection should be applied
        selected_choices <- NULL

        if (
          !is.null(current) &&
          length(current) == 2L &&
          all(current %in% choices)
        ) {
          # 1) Keep user's current selection if still valid
          selected_choices <- current

        } else if (!is.null(selected)) {
          # 2) Use user-specified 'selected' argument (formatted if needed)
          selected_choices <- if (!is.null(fun)) fun(selected) else selected

        } else {
          # 3) Default to full range of choices
          selected_choices <- c(choices[1L], choices[length(choices)])
        }

        # Update slider on the UI
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "sliderPeriod",
          choices = choices,
          selected = selected_choices
        )
      }, ignoreInit = FALSE)

      # ------------------------------------------------------------------------------
      # Reactive value for returning processed slider output
      # ------------------------------------------------------------------------------

      sliderPeriod <- shiny::reactive({
        shiny::req(input$sliderPeriod)

        # Apply inverse formatting (string → Date)
        if (!is.null(invfun)) {
          invfun(input$sliderPeriod)
        } else {
          input$sliderPeriod
        }
      })

      # Return reactive
      sliderPeriod
    }
  )
}
