#' @title Load Button UI
#'
#' @description Create a load button UI.
#'
#' \preformatted{
#'   loadUI("load", label = "Load")
#' }
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#'
#' @return A load button UI
#'
#' @seealso [loadServer()]
#'
#' @export
loadUI <- function(id, label = "Load") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("load"), label = label),
  )
}

#' @title Load Server
#'
#' @description Create a load server.
#'
#' \preformatted{
#'   loadServer("load_id", filename = "tmp.rds", loadfun = "readRDS")
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param filename A string specifying the file name.
#' @param loadfun A string specifying the function name to use when loading data.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [loadUI()]
#'
#' @export
loadServer <- function(id, filename, loadfun = "readRDS") {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      data <- shiny::eventReactive(input$load, {
        showId <- shiny::showNotification("Reading data...", duration = NULL,
                                   closeButton = FALSE)
        on.exit(shiny::removeNotification(showId), add = TRUE)
        stime <- as.numeric(Sys.time())
        f <- match.fun(loadfun)
        data <- f(sprintf("%s", filename))
        etime <- as.numeric(Sys.time())
        lubridate::seconds_to_period(etime - stime)
        data
      })
      return(data)
    }
  )
}
