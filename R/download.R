#' @title Data Download Button UI
#'
#' @description Create a data download button UI.
#'
#' \preformatted{
#'   downloadDataUI("download_data", label = "download")
#' }
#'
#' @param id The name of the output slot that the shiny::downloadHandler is assigned to.
#' @param label The label that should appear on the button.
#' @param right A boolean specifying right or not.
#' @param up A boolean specifying up or not.
#' @param animate A boolean whether to use animation or not.
#'
#' @return A data download button UI
#'
#' @seealso [downloadDataServer()]
#'
#' @export
downloadDataUI <- function(id, label, right = FALSE, up = TRUE,
                           animate = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::dropdown(
      shiny::downloadButton(outputId = ns("downloadRds") , label = "rds" ),
      shiny::downloadButton(outputId = ns("downloadCsv") , label = "csv" ),
      shiny::downloadButton(outputId = ns("downloadXlsx"), label = "xlsx"),
      size = "xs",
      icon = icon("gear", class = "opt"),
      right = right,
      up = up,
      animate = animate
    )
  )
}

#' @title Data Download Server
#'
#' @description Create data download server
#'
#' \preformatted{
#'   downloadDataServer("download_data", filename = "tmp", data = data)
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param filename A string specifying a file name
#' @param data A data object
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [downloadDataUI()]
#'
#' @export
downloadDataServer <- function(id, filename, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$downloadRds <- shiny::downloadHandler(
        filename = function() {
          sprintf("%s_%s.rds", filename, format(Sys.Date(), "%Y%m%d"))
        },
        content = function(file) {
          saveRDS(data, file = file)
        }
      )
      output$downloadCsv <- shiny::downloadHandler(
        filename = function() {
          sprintf("%s_%s.csv", filename, format(Sys.Date(), "%Y%m%d"))
        },
        content = function(file) {
          data.table::fwrite(data, file = file)
        }
      )
      output$downloadXlsx <- shiny::downloadHandler(
        filename = function() {
          sprintf("%s_%s.xlsx", filename, format(Sys.Date(), "%Y%m%d"))
        },
        content = function(file) {
          writexl::write_xlsx(data, path = file)
        }
      )
    }
  )
}

#' @title Plot Download Button UI
#'
#' @description Create a plot download button UI.
#'
#' \preformatted{
#'   downloadPlotUI("download_plot", label = "download")
#' }
#'
#' @param id The name of the output slot that the shiny::downloadHandler is assigned to.
#' @param label The label that should appear on the button.
#' @param right A boolean specifying right or not.
#' @param up A boolean specifying up or not.
#' @param animate A boolean whether to use animation or not.
#'
#' @return A plot download button UI
#'
#' @seealso [downloadPlotServer()]
#'
#' @export
downloadPlotUI <- function(id, label, right = FALSE, up = TRUE,
                           animate = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::dropdown(
      shiny::downloadButton(outputId = ns("downloadPlot")),
      size = "xs",
      icon = icon("gear", class = "opt"),
      right = right,
      up = up,
      animate = animate
    )
  )
}

#' @title Plot Download Server
#'
#' @description Create a plot download server.
#'
#' \preformatted{
#'   downloadPlotServer("download_plot", filename = "tmp", data = data)
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param filename A string specifying a file name.
#' @param plot A plot object.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [downloadPlotUI()]
#'
#' @export
downloadPlotServer <- function(id, filename, plot) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$downloadPlot <- shiny::downloadHandler(
        filename = function() {
          sprintf("%s_%s.png", filename, format(Sys.Date(), "%Y%m%d"))
        },
        content = function(file) {
          ggplot2::ggsave(filename = file, plot = plot, device = "png",
                          width = 10, height = 5, dpi = 600)
        }
      )
    }
  )
}
