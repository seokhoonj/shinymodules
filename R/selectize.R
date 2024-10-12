#' @title Selectize Input Control UI
#'
#' @description Create selectize input control UI.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices Default NULL, list of values to select from.
#' @param selected Default NULL, the initially selected value.
#' @param multiple Is selection of multiple items allowed?
#' @param options A list of options. See the documentation of
#' **selectize.js**([https://selectize.dev/docs/usage]) for possible options
#'
#' @return A selectize input control UI
#'
#' @seealso [selectizeServer()], [dynSelectizeServer()]
#'
#' @export
selectizeUI <- function(id, label = "Selectize", choices = NULL, selected = NULL,
                        multiple = FALSE, options = NULL) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      ns("selectize"), label = label, choices = choices, selected = selected,
      multiple = multiple, options = options
    )
  )
}

#' @title Selectize Input Control Server
#'
#' @description Create a numeric input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices Default NULL, list of values to select from.
#' @param selected Default NULL, the initially selected value.
#' @param options A list of options. See the documentation of
#' **selectize.js**([https://selectize.dev/docs/usage]) for possible options
#'
#' @return A selectize input control server
#'
#' @seealso [selectizeUI()]
#'
#' @export
selectizeServer <- function(id, choices, selected = NULL, options = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      updateSelectizeInput(
        session,
        inputId = "selectize",
        choices = choices,
        selected = selected,
        options = options
      )
      selectize <- reactive({
        validate(need(input$selectize, message = FALSE))
        input$selectize
      })
      return(selectize)
    }
  )
}

#' @title Dynamic Selectize Input Control Server
#'
#' @description Create a dynamic selectize input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data.
#' @param column A data column for unique list of values.
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#' @param options A list of options. See the documentation of
#' **selectize.js**([https://selectize.dev/docs/usage]) for possible options
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [selectizeUI()]
#'
#' @export
dynSelectizeServer <- function(id, data, column, selected = NULL, reverse = FALSE,
                               options = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(data(), {
        dt <- data()
        choices <- sort(unique(dt[[column]]))
        if (reverse)
          choices <- rev(choices)
        updateSelectizeInput(
          session,
          inputId = "selectize",
          choices = choices,
          selected = selected,
          options = options
        )
      })
      selectize <- reactive({
        # validate(need(data(), message = FALSE)) # it causes a plot refresh
        validate(need(input$selectize, message = FALSE))
        input$selectize
      })
      return(selectize)
    }
  )
}
