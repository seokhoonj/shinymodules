#' @title Switch Input Control UI
#'
#' @description Create a toggle switch control ui.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display a text in the center of the switch.
#' @param onLabel Text on the left side of the switch (TRUE).
#' @param offLabel Text on the right side of the switch (FALSE).
#'
#' @return A switch Input UI
#'
#' @seealso [switchServer()]
#'
#' @export
switchUI <- function(id, label = "Gender", onLabel = "Split",
                     offLabel = "Merge") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::switchInput(
      ns("switch"), label = label, onLabel = onLabel, offLabel = offLabel
    )
  )
}

#' @title Slider Input Control Server
#'
#' @description Create a toggle switch control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [switchUI()]
#'
#' @export
switchServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      switch <- shiny::eventReactive(input$switch, {
        # shiny::validate(shiny::need(input$switch, message = FALSE)) is not working for switchInput
        input$switch
      })
      return(switch)
    }
  )
}
