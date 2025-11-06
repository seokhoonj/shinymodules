#' @title Show Modal Action Button UI
#'
#' @description Create a show modal action button UI.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param icon An optional [icon()] to appear on the button.
#' @param width The width of the input, e.g. `400px`, or `100%`; see [shiny::validateCssUnit()].
#' @param ... Named attributes to be applied to the button or link.
#'
#' @return A show modal action button UI
#'
#' @seealso [showModalServer()]
#'
#' @export
showModalUI <- function(id, label = "", icon = NULL, width = NULL, ...) {
  ns <- shiny::NS(id)
  if (missing(icon)) icon <- shiny::icon("search", class = "opt")
  shiny::tagList(
    shiny::actionButton(
      ns("showModalAction"),
      label = label, icon = icon, width = width, ...
    )
  )
}

#' @title Show Modal Server
#'
#' @description Create a show modal server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param ... Named attributes to be applied to the button or link.
#' @param title An optional title for the dialog.
#' @param footer UI for footer. Use NULL for no footer.
#' @param size One of `"s"` for small, `"m"` (the default) for medium, or `"l"` for large.
#' @param easyClose If `TRUE`, the modal dialog can be dismissed by clicking outside
#' the dialog box, or be pressing the Escape key. If `FALSE` (the default),
#' the modal dialog can't be dismissed in those ways; instead it must be
#' dismissed by clicking on a `shiny::modalButton()`, or from a call to [removeModal()]
#' on the server.
#' @param fade If `FALSE`, the modal dialog will have no fade-in animation
#' (it will simply appear rather than fade in to view).
#'
#' @return A show modal server
#'
#' @seealso [showModalUI()]
#'
#' @export
showModalServer <- function(id, ...,
                            title = NULL,
                            footer = shiny::modalButton("Dismiss"),
                            size = c("m", "s", "l", "xl"),
                            easyClose = FALSE,
                            fade = TRUE) {
  size <- match.arg(size)
  shiny::moduleServer(
    id,
    function(input, output, session) {
      showModalAction <- shiny::reactive({
        shiny::validate(shiny::need(input$showModalAction, message = FALSE))
        input$showModalAction
      })
      shiny::observeEvent(showModalAction(), {
        shiny::showModal(shiny::modalDialog(
          ...,
          title = title,
          footer = footer,
          size = size,
          easyClose = easyClose,
          fade = fade
        ))
      })
    }
  )
}
