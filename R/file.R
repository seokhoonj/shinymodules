#' @title File Control UI
#'
#' @description Create a file control UI.
#'
#' \preformatted{
#' ## Example
#' fileUI("file_load", label = "Files")
#' }
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#'
#' @return A file control UI
#'
#' @seealso [fileServer()]
#'
#' @export
fileUI <- function(id, label = "Files") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(4,
      shiny::fileInput(
        ns("file"),
        label = label,
        accept = c(".rds", ".csv", ".xlsx", ".xls")
      )
    ),
    shiny::column(4,
      shiny::selectInput(
        ns("sheet"),
        label = "Sheet",
        choices = NULL,
        selected = NULL
      )
    ),
    shiny::div(
      shiny::column(2,
        shiny::actionButton(ns("save"), "Save"),
        align = "left",
        style = "padding-top: 25px;"
      )
    )
  )
}

#' @title File Control Server
#'
#' @description Create a file control server.
#'
#' @details
#' \preformatted{
#' ## Example
#' fileServer("file_load")
#' }
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [fileUI()]
#'
#' @export
fileServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      userFile <- shiny::eventReactive(input$file, {
        input$file
      })

      ext <- shiny::eventReactive(userFile(), {
        tools::file_ext(userFile()$datapath)
      })

      shiny::observeEvent(ext(), {
        choices <- if (ext() %in% c("xlsx", "xls"))
          readxl::excel_sheets(userFile()$datapath) else character(0)
        shiny::updateSelectInput(session = session, inputId = "sheet",
                          choices = choices)
      })

      sheet <- shiny::eventReactive(input$sheet, {
        input$sheet
      })

      data <- shiny::eventReactive(ext(), {
        if (ext() == "rds") {
          as.data.table(readRDS(userFile()$datapath))
        } else if (ext() == "csv") {
          tryCatch(
            expr = as.data.table(read.csv(
              userFile()$datapath,
              header = TRUE,
              stringsAsFactors = FALSE
            )),
            error = function(e) as.data.table(read.csv(
              userFile()$datapath,
              header = TRUE,
              stringsAsFactors = FALSE,
              fileEncoding = "euc-kr"
            )))
        } else if (ext() %in% c("xlsx", "xls")) {
          shiny::validate(shiny::need(sheet(), message = FALSE))
          tryCatch(
            expr = as.data.table(readxl::read_excel(
              userFile()$datapath,
              sheet = sheet(),
              guess_max = 21474836
            )),
            error = function(e) data.table::data.table()
          )
        }
      })

      # Save
      userSave <- shiny::eventReactive(input$save, {
        input$save
      })

      shiny::observeEvent(userSave(), {
        if (is.null(input$file)) {
          shiny::showModal(shiny::modalDialog(
            title = shiny::h4("Unsaved", align = "center"),
            shiny::h5("You have not uploaded a file", align = "center"),
            easyClose = TRUE,
            footer = NULL,
            size = "s"
          ))
        } else {
          if (ext() == "rds") {
            saveRDS(data(), file = sprintf("save/%s", userFile()$name))
          } else if (ext() == "csv") {
            data.table::fwrite(
              data(), file = sprintf("save/%s", userFile()$name),
              row.names = FALSE
            )
          } else if (ext() %in% c("xlsx", "xls")) {
            writexl::write_xlsx(
              data(), path = sprintf("save/%s", userFile()$name)
            )
          }
          shiny::showModal(shiny::modalDialog(
            title = shiny::h4("Saved", align = "center"),
            shiny::h5(sprintf("%s file is saved", userFile()$name), align = "center"),
            easyClose = TRUE,
            footer = NULL,
            size = "s"
          ))
        }
      })
      return(data)
    }
  )
}
