
# IApp ---------------------------------------------------------------

#' @title
#' Class that functions as an interface for app controllers
#'
#' @description
#' Defines the interface for app controllers.
#'
#' @details
#' The terms \emph{interace} is used in a looser context than in more
#' rigid OOP contexts such as \emph{C#} or the like
#'
#' @section Getters/setters:
#'
#' \itemize{
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{importData} {
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-IApp.R
#'
#' @import R6
#' @export
IApp <- R6Class(
  classname = "IApp",
  portable = TRUE,
  public = list(
    importData = function() {},
    prepare = function() {}
  )
)

# App ---------------------------------------------------------------

#' @title
#' Generic class for inheritance for app controllers
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#'
#' @field ns \code{\link{ANY}}
#'  App object
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[eventr]{IApp}}
#'  }
#'  \item{initialize} {
#'
#'    \itemize{
#'      \item{ns }{\code{\link{ANY}}}
#'    }
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-App.R
#'
#' @import R6
#' @export
App <- R6Class(
  classname = "App",
  inherit = IApp,
  portable = TRUE,
  public = list(
    ## Meta field //
    injected = list(),
    meta = list(),
    reactives = list(),
    config_files = list(),
    config = list(),

    ## Fields //
    ns = character(),

    ## Methods //
    initialize = function(
      ...,
      injected = list(),
      meta = list(),
      reactives = list(),
      config_files = list(),
      config = list(),
      ns = character()
    ) {
      self$ns <- ns
      self$injected <- injected
      self$meta <- meta
      self$reactives <- reactives
      self$config_files <- config_files
      self$config <- config
    },
    importData = function() {
      dataconr::methodNotImplemented(self)
    },
    prepare = function(path, ...) {
      ## DB //
      prepareDatabase(path, ...)
    },
    loadConfigs = function() {
      dataconr::methodNotImplemented(self)
    }
  )
)

# App.Timetracking ---------------------------------------------------------

#' @title
#' Connector for Timetracking
#'
#' @description
#' Methods for controlling the app \emph{Timetracking}.
#'
#' @details
#' TODO
#'
#' @field ns \code{\link{character}}
#'  App name/class
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[eventr]{IApp}}
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-App.Timetracking.R
#'
#' @import R6
#' @export
App.Timetracking <- R6Class(
  classname = "App.Timetracking",
  inherit = App,
  portable = TRUE,
  public = list(
    ## Fields //
    ns = "Timetracking",

    ## Methods //
    initialize = function(
      ...,
      ns = "Timetracking"
    ) {
      super$initialize(...)
      self$ns <- ns
    },
    createFilePath = function(
      input,
      session,
      parent = "data"
    ) {
      value <- eventReactive(input$import, {
        file.path(parent, input$import_file)
      })
      self$reactives$import_file <- value
      value
    },
    loadConfigs = function(
      path = self$config_files,
      ...
    ) {
      if (length(path) == 1 && file.exists(path) && file.info(path)$isdir) {
        path <- list.files(path, full.names = TRUE)
      }
      config <- lapply(path, function(ii) {
        if (file.exists(ii)) {
          txt <- readLines(ii)
          jsonlite::fromJSON(txt)
        }
      })
      names(config) <- basename(path)

      config <- mapConfigs(configs = config, ...)

      self$config <- config
    },
    importData = function(
      input,
      session,
      path = self$reactives$import_file
    ) {
      out <- reactive({
        withProgress(
          message = 'Importing data',
          detail = "This may take a while...",
          {

            ## Read from file system //
            fs_con <- self$injected$fs_con
            fs_con$setConnection(path())
            fs_con$pull()
            incProgress(0.5)

            ## Pass to database controller //
            db_con <- self$injected$db_con
            db_con$setCached(fs_con$getCached())
            data <- db_con$applyExternalFormat()
            incProgress(0.5)

            ## Update configs //

            json <- jsonlite::toJSON(
              fs_con$getCached()$getRFormat()$getStructure()
            )
            write(json, file = "data/config/R$format$structure")
            self$loadConfigs("data/config")

            data
          }
        )
        data
      })
      ## Cache in app instance //
      self$reactives$imported_data <- out

      out
    },
    prepare = function(path = sqlite_path, ...) {
      ## DB //
      prepareDatabase(path, ...)
    }
  )
)
