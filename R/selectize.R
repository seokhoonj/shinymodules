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
#' \pkg{selectize.js}(<https://selectize.dev/docs/usage>) for possible options
#'
#' @return A selectize input control UI
#'
#' @seealso [selectizeServer()], [dynSelectizeServer()]
#'
#' @export
selectizeUI <- function(id, label = "Select", choices = NULL, selected = NULL,
                        multiple = FALSE, options = NULL) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      ns("select"), label = label, choices = choices, selected = selected,
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
#' \pkg{selectize.js}(<https://selectize.dev/docs/usage>) for possible options
#' @param server whether to store choices on the server side, and load the select options dynamically on searching,
#' instead of writing all choices into the page at once (i.e., only use the client-side version of \pkg{selectize.js})
#'
#' @return A selectize input control server
#'
#' @seealso [selectizeUI()]
#'
#' @export
selectizeServer <- function(id, choices, selected = NULL, options = NULL,
                            server = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      updateSelectizeInput(
        session,
        inputId = "select",
        choices = choices,
        selected = selected,
        options = options,
        server = server
      )
      selectize <- reactive({
        validate(need(input$select, message = FALSE))
        input$select
      })
      return(select)
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
#' \pkg{selectize.js}(<https://selectize.dev/docs/usage>) for possible options
#' @param server whether to store choices on the server side, and load the select options dynamically on searching,
#' instead of writing all choices into the page at once (i.e., only use the client-side version of \pkg{selectize.js})
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [selectizeUI()]
#'
#' @export
dynSelectizeServer <- function(id, data, column, selected = NULL, reverse = FALSE,
                               options = NULL, server = FALSE) {
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
          inputId = "select",
          choices = choices,
          selected = selected,
          options = options,
          server = server
        )
      })
      selectize <- reactive({
        # validate(need(data(), message = FALSE)) # it causes a plot refresh
        validate(need(input$select, message = FALSE))
        input$select
      })
      return(select)
    }
  )
}
