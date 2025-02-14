
#' @title Import data from Googlesheet
#'
#' @description Let user paste link to a Google sheet then import the data.
#'
#' @param id Module's ID
#' @param title Module's title, if \code{TRUE} use the default title,
#'  use \code{NULL} for no title or a \code{shiny.tag} for a custom one.
#'
#' @eval doc_return_import()
#'
#' @export
#' @name import-googlesheets
#'
#' @importFrom shiny NS actionLink
#' @importFrom shinyWidgets textInputIcon
#' @importFrom htmltools tags tagList
#'
#' @example examples/googlesheets.R
import_googlesheets_ui <- function(id, title = TRUE) {

  ns <- NS(id)

  if (isTRUE(title)) {
    title <- tags$h4(
      i18n("Importar hoja de cálculo de Google"),
      class = "datamods-title"
    )
  }

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    title,
    tags$div(
      class = "pull-right",
      help_popup(tagList(
        i18n("Puede utilizar:"),
        tags$ul(
          tags$li(
            i18n("Un enlace para compartir, en ese caso se leerá la primera hoja")
          ),
          tags$li(
            i18n("La URL que aparece en su navegador, en ese caso se leerá la hoja actual")
          )
        )
      ))
    ),
    textInputIcon(
      inputId = ns("link"),
      label = i18n("Introduce un enlace compartible a una hoja de Google:"),
      icon = icon("link"),
      width = "100%"
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b(i18n("Todavía no hay datos pegados.")),
        i18n("Pega un enlace válido de GoogleSheet en el cuadro de diálogo anterior."),
        dismissible = TRUE
      )
    ),
    uiOutput(
      outputId = ns("container_confirm_btn"),
      style = "margin-top: 20px;"
    )
  )
}


#' @param btn_show_data Display or not a button to display data in a modal window if import is successful.
#' @param trigger_return When to update selected data:
#'  \code{"button"} (when user click on button) or
#'  \code{"change"} (each time user select a dataset in the list).
#' @param return_class Class of returned data: \code{data.frame}, \code{data.table} or \code{tbl_df} (tibble).
#' @param reset A `reactive` function that when triggered resets the data.
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom shiny reactiveValues observeEvent removeUI reactive req
#' @importFrom htmltools tags tagList
#'
#' @rdname import-googlesheets

import_googlesheets_server <- function(id,
                                       btn_show_data = TRUE,
                                       trigger_return = c("button", "change"),
                                       return_class = c("data.frame", "data.table", "tbl_df"),
                                       reset = reactive(NULL)) {

  trigger_return <- match.arg(trigger_return)

  module <- function(input, output, session) {

    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL, name = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL, status = NULL)

    observeEvent(reset(), {
      temporary_rv$data <- NULL
      temporary_rv$name <- NULL
      temporary_rv$status <- NULL
    })

    output$container_confirm_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        button_import()
      }
    })

    observeEvent(input$trigger, {
      if (identical(trigger_return, "change")) {
        hideUI(selector = paste0("#", ns("confirm-button")))
      }
    })

    observeEvent(input$link, {
      req(input$link)
      imported <- try(read_gsheet(input$link), silent = TRUE)
      if (inherits(imported, "try-error") || NROW(imported) < 1) {
        toggle_widget(inputId = "confirm", enable = FALSE)
        insert_error()
        temporary_rv$status <- "error"
        temporary_rv$data <- NULL
      } else {
        toggle_widget(inputId = "confirm", enable = TRUE)
        insert_alert(
          selector = ns("import"),
          status = "success",
          make_success_alert(
            imported,
            trigger_return = trigger_return,
            btn_show_data = btn_show_data
          )
        )
        temporary_rv$status <- "success"
        temporary_rv$data <- imported
      }
    }, ignoreInit = TRUE)

    observeEvent(input$see_data, {
      show_data(temporary_rv$data, title = i18n("Datos importados"))
    })

    observeEvent(input$confirm, {
      imported_rv$data <- temporary_rv$data
    })

    if (identical(trigger_return, "button")) {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(imported_rv$name),
        data = reactive(as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(temporary_rv$name),
        data = reactive(as_out(temporary_rv$data, return_class))
      ))
    }
  }

  moduleServer(
    id = id,
    module = module
  )
}



# Utils -------------------------------------------------------------------

get_id <- function(x) {
  if (grepl("/d/", x)) {
    x <- strsplit(x = x, split = "/")
    x <- unlist(x)
    x[which(x == "d") + 1]
  } else if (grepl("id=", x)) {
    x <- regmatches(x, gregexpr("id=[[:alnum:]_-]+", x))
    gsub("^id=", "", x[[1]])
  } else {
    stop("No se ha podido recuperar el ID de la hoja de cálculo")
  }
}

#' @importFrom data.table fread .SD
#' @importFrom utils type.convert
read_gsheet <- function(url, dec = NULL) {
  url_ <- sprintf(
    "https://docs.google.com/spreadsheets/export?id=%s&format=csv",
    get_id(url)
  )
  if (grepl("gid=", url)) {
    gid <- regmatches(url, gregexpr("gid=[0-9]+", url))
    url_ <- paste0(url_, "&", gid[[1]])
  }
  dt <- fread(input = url_)
  if (!is.null(dec)) {
    dt <- dt[, lapply(.SD, type.convert, dec = dec)]
  }
  return(dt)
}

