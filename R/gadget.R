test_shiny <- function(output_id = "", expr, title = NULL, dialog_name,
                       width = "100%", height = "500px",
                       viewer_width = 1200, viewer_height = 500) {
  if (missing(dialog_name))
    dialog_name <- output_id
  shiny::runGadget(
    shiny::fluidPage(
      shiny::plotOutput(output_id, width = width, height = height)
    ),
    function(input, output, session) {
      output[[output_id]] <- shiny::renderPlot({
        eval(expr)
      })
    }, viewer = shiny::dialogViewer(
      dialogName = dialog_name, width = viewer_width, height = viewer_height
    )
  )
}
