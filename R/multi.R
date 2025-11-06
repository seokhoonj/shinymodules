#' @title Multiselect Control UI
#'
#' @description Create a multiselect control UI.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#' @param options List of options passed to multi (enable_search = FALSE for disabling the search bar for example).
#' @param width width The width of the input, e.g. `400px`, or `100%`.
#' @param autocomplete Sets the initial state of the autocomplete property.
#'
#' @return A multiselect control UI.
#'
#' @seealso [multiServer()], [dynMultiServer()]
#'
#' @export
multiUI <- function(id, label = "", choices = character(0),
                    selected = character(0), options = NULL, width = NULL,
                    autocomplete = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::multiInput(
      inputId = ns("multi"),
      label = label,
      choices = choices,
      selected = selected,
      options = options,
      width = width,
      autocomplete = autocomplete
    )
  )
}

#' @title Multiselect Control Server
#'
#' @description Create a multiselect control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param choices List of values to select from.
#' @param selected The initially selected value.
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [multiUI()]
#'
#' @export
multiServer <- function(id, choices = character(0), selected = character(0)) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      shiny::observe({
        shinyWidgets::updateMultiInput(
          session,
          inputId = "multi",
          choices = choices,
          selected = selected
        )
      })
      multi <- shiny::reactive({
        shiny::validate(shiny::need(input$multi, message = FALSE))
        input$multi
      })
      return(multi)
    }
  )
}

#' @title Dynamic Multiselect Control Server
#'
#' @description Create a dynamic multiselect control server.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data A reactive data.
#' @param column A data column for unique list of values.
#' @param selected The initially selected value.
#' @param reverse A boolean value whether to reverse the choices or not
#'
#' @return The return value, if any, from executing the module server function
#'
#' @seealso [multiUI()]
#'
#' @export
dynMultiServer <- function(id, data, column, selected = character(0),
                           reverse = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      shiny::observeEvent(data(), {
        choices <- sort(unique(data()[[column]]))
        if (reverse)
          choices <- rev(choices)
        shinyWidgets::updateMultiInput(
          session,
          inputId = "multi",
          choices = choices,
          selected = selected
        )
      })
      multi <- shiny::reactive({
        shiny::validate(shiny::need(input$multi, message = FALSE))
        input$multi
      })
      return(multi)
    }
  )
}

#' Dynamic Single-Selection Server Module for MultiInput
#'
#' @description
#' Enhances a `shinyWidgets::multiInput` widget to behave as a
#' *single-selection* input. Whenever a new item is selected, all
#' previous selections are cleared, and only the most recently
#' selected item remains.
#'
#' The module prevents intermediate and empty states from propagating
#' during internal updates by freezing reactive values and using an
#' internal update lock flag.
#'
#' @param id Shiny module ID.
#' @param choices Character vector of available choices.
#' @param selected Character vector of initially selected values
#'   (if more than one, only the first is used in single mode).
#' @param single Logical; if `TRUE` (default), enforce single selection
#'   with most-recent value kept. If `FALSE`, behave like normal multi-select.
#'
#' @return A `reactive()` expression returning the selected value(s).
#'   In single mode, this returns a vector of length ≤ 1.
#'
#' @details
#' - Tracks previous selection using `reactiveVal()`
#' - Detects newly added values using set difference (`setdiff`)
#' - Uses `freezeReactiveValue()` and an `internal` flag to avoid
#'   update loops and transient invalid states
#' - Downstream usage should apply `req()` for safe access
#'
#' @examples
#' \dontrun{
#' # UI
#' multiUI("sector", label = "Select Sector", choices = c("Tech", "Bio", "Finance"))
#'
#' # Server
#' sector_sel <- multiSingleServer("sector", choices = c("Tech", "Bio", "Finance"))
#'
#' observe({
#'   req(sector_sel())
#'   print(sector_sel())
#' })
#' }
#'
#' @seealso [multiUI()]
#'
#' @export
multiSingleServer <- function(id,
                              choices  = character(0),
                              selected = character(0),
                              single   = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {

    # Stores previous selected values (for tracking added items)
    prev <- shiny::reactiveVal(character(0))

    # Flag to avoid reacting to internal updates
    internal <- shiny::reactiveVal(FALSE)

    # One-time initialization
    shiny::observeEvent(TRUE, {
      sel0 <- if (single && length(selected) > 1L) selected[1L] else selected

      # Internal update block begins
      internal(TRUE)
      shiny::freezeReactiveValue(input, "multi")
      shinyWidgets::updateMultiInput(session, "multi",
                                     choices  = choices,
                                     selected = sel0)
      internal(FALSE)
      # End internal update block

      prev(sel0)
    }, once = TRUE)

    # Observe every selection change
    shiny::observeEvent(input$multi, {
      # Ignore events triggered by internal updates
      if (isTRUE(internal())) return(invisible(NULL))

      cur <- input$multi %||% character(0)
      old <- prev()

      # Multi-select mode: no restriction
      if (!single) {
        prev(cur)
        return(invisible(NULL))
      }

      # If nothing selected → store empty state
      if (length(cur) == 0L) {
        prev(character(0))
        return(invisible(NULL))
      }

      # Detect newly added values (set difference)
      added <- setdiff(cur, old)

      # Keep only the most recently added item
      keep <- if (length(added)) added[[length(added)]] else cur[[1L]]

      # If UI's state doesn't match target state → update UI
      if (!identical(sort(cur), sort(keep))) {
        internal(TRUE)
        shiny::freezeReactiveValue(input, "multi")
        shinyWidgets::updateMultiInput(session, "multi", selected = keep)
        internal(FALSE)
      }

      prev(keep)
    }, ignoreInit = TRUE, priority = 100)

    # Return a reactive expression; downstream code should use req()
    reactive({
      shiny::req(!is.null(input$multi), length(input$multi) > 0)
      input$multi
    })
  })
}
