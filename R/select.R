#' @title Select Input Control UI
#'
#' @description Create select input control UI.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices Default NULL, list of values to select from.
#' @param selected Default NULL, the initially selected value.
#' @param multiple Is selection of multiple items allowed?
#' @param selectize Whether to use \pkg{selectize.js} or not.
#'
#' @return A select input control UI
#'
#' @seealso [selectServer()], [dynSelectServer()]
#'
#' @export
selectUI <- function(id, label = "Select", choices = NULL, selected = NULL,
                     multiple = FALSE, selectize = TRUE) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("select"), label = label, choices = choices, selected = selected,
      multiple = multiple, selectize = selectize
    )
  )
}

#' @title Select Input Control Server
#'
#' @description Create a numeric input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices Default NULL, list of values to select from.
#' @param selected Default NULL, the initially selected value.
#' @param selectize Whether to use \pkg{selectize.js} or not.
#'
#' @return A select input control server
#'
#' @seealso [selectUI()]
#'
#' @export
selectServer <- function(id, choices, selected = NULL, selectize = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      updateSelectInput(
        session,
        inputId = "select",
        choices = choices,
        selected = selected
      )
      select <- reactive({
        validate(need(input$select, message = FALSE))
        input$select
      })
      return(select)
    }
  )
}

#' @title Dynamic Select Input Control Server
#'
#' @description Create a dynamic select input control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data.
#' @param column A data column for unique list of values.
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#' @param selectize Whether to use \pkg{selectize.js} or not.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [selectUI()]
#'
#' @export
dynSelectServer <- function(id, data, column, selected = NULL, reverse = FALSE,
                            selectize = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(data(), {
        dt <- data()
        choices <- sort(unique(dt[[column]]))
        if (reverse)
          choices <- rev(choices)
        updateSelectInput(
          session,
          inputId = "select",
          choices = choices,
          selected = selected
        )
      })
      select <- reactive({
        # validate(need(data(), message = FALSE)) # it causes a plot refresh
        validate(need(input$select, message = FALSE))
        input$select
      })
      return(select)
    }
  )
}
