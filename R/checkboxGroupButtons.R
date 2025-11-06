#' @title Checkbox Group buttons UI
#'
#' @description Create checkbox group buttons UI.
#'
#' @details
#' \preformatted{
#' ## Example
#' checkboxGroupButtonsUI(
#'   id = "checkboxgroupbuttons", label = "",
#'   status = "primary")
#' }
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param status Add a class to the buttons, you can use Bootstrap status like
#' 'info', 'primary', 'danger', 'warning' or 'success'. Or use an arbitrary
#' strings to add a custom class, e.g. : with status = "custom-class",
#' buttons will have class btn-custom-class.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [checkboxGroupButtonsServer()]
#'
#' @export
checkboxGroupButtonsUI <- function(id, label = "", status = "primary",
                                   choices = as.character(0),
                                   selected = as.character(0)) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::checkboxGroupButtons(
      inputId = ns("checkboxGroupButtons"),
      label = label,
      choices = choices,
      selected = selected,
      status = status,
      checkIcon = list(
        yes = icon("ok"   , lib = "glyphicon"),
        no  = icon("xmark", lib = "glyphicon")
      )
    )
  )
}

#' @title Checkbox group buttons Server
#'
#' @description Create a checkbox group buttons server.
#'
#' @details
#' \preformatted{
#' ## Example
#' checkboxGroupButtonsServer(
#'   id = "checkboxgroupbuttons", label = "",
#'   status = "primary", choices = c("A", "B", "C"), selected = "A")
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param status Add a class to the buttons, you can use Bootstrap status like
#' 'info', 'primary', 'danger', 'warning' or 'success'. Or use an arbitrary
#' strings to add a custom class, e.g. : with status = "custom-class",
#' buttons will have class btn-custom-class.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [checkboxGroupButtonsUI()]
#'
#' @export
checkboxGroupButtonsServer <- function(id, status = "primary", choices,
                                       selected = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      shiny::observe({
        shinyWidgets::updateCheckboxGroupButtons(
          session,
          inputId = "checkboxGroupButtons",
          choices = choices,
          selected = selected,
          status = status,
          checkIcon = list(
            yes = icon("ok"   , lib = "glyphicon"),
            no  = icon("xmark", lib = "glyphicon")
          )
        )
      })
      checkboxGroupButtons <- shiny::reactive({
        shiny::validate(shiny::need(input$checkboxGroupButtons, message = FALSE))
        input$checkboxGroupButtons
      })
      return(checkboxGroupButtons)
    }
  )
}
