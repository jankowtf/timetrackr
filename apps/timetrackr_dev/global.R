
# Packages ----------------------------------------------------------------
# install.packages("ddR")
# install.packages("RSQLite")
# install.packages("shiny")
# install.packages("shinydashboard")

library(shiny)
library(shinydashboard)

# library(ddR)
library(DT)
# library(readxl)
# library(RNeo4j)
library(RSQLite)
library(digest)

# library(crudr)
library(timetrackr)
# install.packages("rsconnect")
# library(rsconnect)

# if (!require("devtools"))
#   install.packages("devtools")
# devtools::install_github("rstudio/rsconnect")


# Variables ---------------------------------------------------------------

GLOBALS <- list()
GLOBALS$debug$enabled <- TRUE

# Buffer ------------------------------------------------------------------

GLOBALS$buffer <- new.env()

## Initialize counter //
GLOBALS$buffer$action_task_create <- 0
GLOBALS$buffer$action_task_create_2 <- 0
GLOBALS$buffer$action_task_create_cancel <- 0
GLOBALS$buffer$action_task_update_cancel <- 0
GLOBALS$buffer$action_time_update_cancel <- 0

# Database ----------------------------------------------------------------

sqlite_path <- "data/db/timetracking-db"

GLOBALS$db$tables$issues$tablename <- "issues"
GLOBALS$db$tables$times$tablename <- "times"

GLOBALS$db$tables$issues$public_fields_compact <- list(
  list(name = "issue_summary", nicename = "Summary", datatype = "TEXT"),
  list(name = "issue_project", nicename = "Project", datatype = "TEXT"),
  list(name = "issue_week", nicename = "Week", datatype = "INTEGER"),
  list(name = "issue_status", nicename = "Status", datatype = "TEXT"),
  list(name = "issue_time_estimated", nicename = "Time estimated", datatype = "REAL"),
  list(name = "issue_time_spent", nicename = "Time spent", datatype = "REAL"),
  list(name = "issue_time_until_done", nicename = "Time until done", datatype = "REAL")
)
GLOBALS$db$tables$issues$public_fields_details <- list(
  list(name = "issue_description", nicename = "Description", datatype = "TEXT"),
  list(name = "issue_adhoc", nicename = "Ad hoc", datatype = "TEXT"),
  list(name = "issue_interruption", nicename = "Interruption", datatype = "TEXT"),
  list(name = "issue_meeting", nicename = "Meeting", datatype = "TEXT")
)
GLOBALS$db$tables$issues$private_fields <- list(
  list(name = "_uid", datatype = "TEXT"),
  list(name = "_time_created", datatype = "TEXT"),
  list(name = "_time_modified", datatype = "TEXT")
)

## Times //
GLOBALS$db$tables$times$public_fields_compact <- list(
  list(name = "issue_time_logged_date", nicename = "Date", datatype = "TEXT"),
  list(name = "issue_time_logged", nicename = "Time", datatype = "REAL"),
  list(name = "issue_time_logged_week", nicename = "Week", datatype = "TEXT")
)
GLOBALS$db$tables$times$public_fields_details <- list(
  list(name = "issue_time_logged_description",
    nicename = "Description", datatype = "TEXT")
)

GLOBALS$db$tables$times$private_fields <- list(
  list(name = "_uid", datatype = "TEXT"),
  list(name = "_refuid", datatype = "TEXT"),
  list(name = "_time_created", datatype = "TEXT"),
  list(name = "_time_modified", datatype = "TEXT")
)

# Instantiate -------------------------------------------------------------

# App.Timetracking$debug("importData")
# App.Timetracking$debug("getSelectedTimeSeries")

app <- App.Timetracking$new(
  injected = list(
    fs_con = NULL,
    db_con = NULL
  )
)

# Configs -----------------------------------------------------------------

# app$loadConfigs("data/config")

# Dynamic UI: issue details ----------------------------------------------

createDynamicUi_issueDetails <- function(
  input,
  output,
  debug = GLOBALS$debug$enabled
) {
  if (debug) {
    message("Dynamic UI: issues ----------")
    print(Sys.time())
  }

  ## Dependencies //
  action_selected_row <- input$dt_issues_rows_selected
  if (debug) {
    message("action_selected_row:")
    print(action_selected_row)
  }

  action_task_create <- input$action_task_create
  if (debug) {
    message("action_task_create:")
    message(paste0("* Current: ", action_task_create))
    message(paste0("* Buffered: ", GLOBALS$buffer$action_task_create))
  }

  action_task_create_2 <- input$action_task_create_2
  # action_task_create_cancel <- isolate(input$action_task_create_cancel)
  if (is.null(action_task_create_2)) {
    action_task_create_2 <- 0
  }
  if (debug) {
    message("action_task_create_2:")
    message(paste0("* Current: ", action_task_create_2))
    message(paste0("* Buffered: ", GLOBALS$buffer$action_task_create_2))
  }

  action_task_create_cancel <- input$action_task_create_cancel
  # action_task_create_cancel <- isolate(input$action_task_create_cancel)
  if (is.null(action_task_create_cancel)) {
    action_task_create_cancel <- 0
  }
  if (debug) {
    message("action_task_create_cancel:")
    message(paste0("* Current: ", action_task_create_cancel))
    message(paste0("* Buffered: ", GLOBALS$buffer$action_task_create_cancel))
  }

  action_task_update_cancel <- input$action_task_update_cancel
  # action_task_update_cancel <- isolate(input$action_task_update_cancel)
  if (is.null(action_task_update_cancel)) {
    action_task_update_cancel <- 0
  }
  if (debug) {
    message("action_task_update_cancel:")
    message(paste0("* Current: ", action_task_update_cancel))
    message(paste0("* Buffered: ", GLOBALS$buffer$action_task_update_cancel))
  }

  decideOnUi <- function(
    action_selected_row,
    action_task_create,
    action_task_create_cancel,
    buffer,
    debug
  ) {
    ## Initial state //
    value <- if (
      !length(action_selected_row) &&
        c(
          action_task_create <= buffer$action_task_create &&
            action_task_create_cancel <= buffer$action_task_create_cancel
        ) ||
        action_task_create_2 > buffer$action_task_create_2
    ) {
      "hide"
    } else if (
      # c(!length(action_selected_row) &&
      action_task_create > buffer$action_task_create
    ) {
      "create"
    } else if (
      # !length(action_selected_row) &&
      # action_task_create == buffer$action_task_create &&
      c(
        action_task_create_cancel > buffer$action_task_create_cancel ||
          action_task_update_cancel > buffer$action_task_update_cancel
      )
    ) {
      "cancel"
    } else if (
      length(action_selected_row) &&
        action_task_create == buffer$action_task_create &&
        action_task_create_cancel <= buffer$action_task_create_cancel
    ) {
      "update"
    } else {
      "NIY"
    }
    if (debug) {
      message("UI decision:")
      print(value)
    }
    value
  }
  ui_decision <- decideOnUi(
    action_selected_row = action_selected_row,
    action_task_create = action_task_create,
    action_task_create_cancel = action_task_create_cancel,
    buffer = GLOBALS$buffer,
    debug = debug
  )

  ## Aux function //
  getFormValue <- function(field, idx, default = "") {
    if (!is.null(idx)) {
      dat <- loadData(table = "issues")
      dat[idx, field]
    } else {
      value <- isolate(input[[field]])
      if (is.null(value)) {
        default
      } else {
        value
      }
    }
  }

  ## Form components //
  container <- list()

  field <- "issue_summary"
  name <- "Summary"
  value <- getFormValue(field = field, idx = action_selected_row)
  container[[field]] <- textInput(field, name, value)

  field <- "issue_description"
  name <- "Description"
  value <- getFormValue(field = field, idx = action_selected_row)
  container[[field]] <- textInput(field, name, value)

  field <- "issue_project"
  name <- "Project"
  value <- getFormValue(field = field, idx = action_selected_row)
  container[[field]] <- textInput(field, name, value)

  field <- "issue_week"
  name <- "Week"
  value <- getFormValue(field = field, idx = action_selected_row)
  container[[field]] <- numericInput(field, name, value,
    min = 1, max = 53, step = 1)

  field <- "issue_status"
  name <- "Status"
  value <- getFormValue(field = field, idx = action_selected_row,
    default = validStatuses()["todo"])
  container[[field]] <- selectInput(field, name, unname(validStatuses()),
    selected = value)

  field <- "issue_time_estimated"
  name <- "Time estimated"
  value <- getFormValue(field = field, idx = action_selected_row)
  container[[field]] <- textInput(field, name, value)

  field <- "issue_adhoc"
  name <- "Ad hoc"
  value <- as.logical(getFormValue(field = field, idx = action_selected_row,
    default = FALSE))
  container[[field]] <- checkboxInput(field, name, value = value)

  field <- "issue_interruption"
  name <- "Interruption"
  value <- as.logical(getFormValue(field = field, idx = action_selected_row,
    default = FALSE))
  container[[field]] <- checkboxInput(field, name, value = value)

  field <- "issue_meeting"
  name <- "Meeting"
  value <- as.logical(getFormValue(field = field, idx = action_selected_row,
    default = FALSE))
  container[[field]] <- checkboxInput(field, name, value = value)

  ## Bundle in box //
  value <- if (ui_decision == "hide") {
    div()
  } else if (ui_decision == "create") {
    container$buttons <- div(style="display:inline-block",
      actionButton("action_task_create_2", "Create"),
      actionButton("action_task_create_cancel", "Cancel")
    )
    do.call(box, args = list(container, title = "Create task",
      status = "primary", width = NULL))
  } else if (ui_decision == "cancel") {
    div()
  } else if (ui_decision == "update") {
    container$buttons <- div(style="display:inline-block",
      actionButton("action_task_update", "Update"),
      actionButton("action_task_update_cancel", "Cancel"),
      p(),
      actionButton("action_task_delete", "Delete",
        icon = icon("exclamation-triangle"))
    )
    do.call(box, args = list(container, title = "Update task",
      status = "danger", width = NULL))
  }

  #     ## Resets //
  #     attr(input, "readonly") <- FALSE
  #     if (action_task_create_cancel >= 1) {
  #       input$action_task_create_cancel <- structure(
  #         0, class = c("integer", "shinyActionButtonValue"))
  #     }
  # #     if (action_task_create > 0) {
  # #       input$action_task_create <- structure(
  # #         0, class = c("integer", "shinyActionButtonValue"))
  # #     }
  #     attr(input, "readonly") <- TRUE

  ## Remember counter //
  GLOBALS$buffer$action_task_create <- action_task_create
  GLOBALS$buffer$action_task_create_2 <- action_task_create_2
  GLOBALS$buffer$action_task_create_cancel <- action_task_create_cancel
  GLOBALS$buffer$action_task_update_cancel <- action_task_update_cancel

  value
}

# Dynamic UI: log time 2 ----------------------------------------------------

createDynamicUi_logTime2 <- function(
  input,
  output,
  debug = GLOBALS$debug$enabled
) {
  if (debug) {
    message("Dynamic UI: log time ----------\n")
    print(Sys.time())
  }

  ## Dependencies //
  action_dt_issues_selected_row <- input$dt_issues_rows_selected
  if (debug) {
    message("action_dt_issues_selected_row:")
    print(action_dt_issues_selected_row)
  }

  action_dt_times_selected_row <- input$dt_times_rows_selected
  if (debug) {
    message("action_dt_times_selected_row:")
    print(action_dt_times_selected_row)
  }

  action_time_update_cancel <- input$action_time_update_cancel
  # action_time_update_cancel <- isolate(input$action_time_update_cancel)
  if (is.null(action_time_update_cancel)) {
    action_time_update_cancel <- 0
  }
  if (debug) {
    message("action_time_update_cancel:")
    message(paste0("* Current: ", action_time_update_cancel))
    message(paste0("* Buffered: ", GLOBALS$buffer$action_time_update_cancel))
  }

  decideOnUi <- function(
    action_dt_issues_selected_row,
    action_dt_times_selected_row,
    action_time_update_cancel,
    buffer,
    debug
  ) {
    ## Initial state //
    value <- if (
      length(action_dt_issues_selected_row) &&
        !length(action_dt_times_selected_row)
    ) {
      "create"
    } else if (
      # length(action_dt_times_selected_row) &&
      action_time_update_cancel > buffer$action_time_update_cancel
    ) {
      "cancel update"
    } else if (
      length(action_dt_issues_selected_row) &&
        length(action_dt_times_selected_row)
      # action_time_update_cancel <= buffer$action_time_update_cancel
    ) {
      "update"
    } else {
      "NIY"
    }
    if (debug) {
      message("Log time UI decision:")
      print(value)
    }
    value
  }
  ui_decision <- decideOnUi(
    action_dt_issues_selected_row = action_dt_issues_selected_row,
    action_dt_times_selected_row = action_dt_times_selected_row,
    action_time_update_cancel = action_time_update_cancel,
    buffer = GLOBALS$buffer,
    debug = debug
  )

  ## Aux function //
  getFormValue <- function(field, idx, default = "") {
    if (!is.null(idx)) {
      dat <- loadData(table = GLOBALS$db$tables$times$tablename)
      dat[idx, field]
    } else {
      value <- isolate(input[[field]])
      if (is.null(value)) {
        default
      } else {
        value
      }
    }
  }

  ## Form components //
  container <- list()

  field <- "issue_time_logged"
  name <- "Time spent"
  value <- getFormValue(field = field, idx = action_dt_times_selected_row)
  container[[field]] <- textInput(field, name, value)

  field <- "issue_time_logged_date"
  name <- "Date"
  value <- getFormValue(field = field, idx = action_dt_times_selected_row,
    default <- as.character(Sys.Date()))
  container[[field]] <- dateInput(field, name, value = value)

  field <- "issue_time_logged_description"
  name <- "Work description"
  value <- getFormValue(field = field, idx = action_dt_times_selected_row)
  container[[field]] <- textInput(field, name, value)

  value <- if (ui_decision == "create") {
    container$buttons <- div(style="display:inline-block",
      actionButton("action_time_log", "Log")
    )
    do.call(box, args = list(container, title = "Log time",
      status = "primary", width = NULL))
  } else if (ui_decision == "update") {
    container$buttons <- div(style="display:inline-block",
      actionButton("action_time_update", "Update"),
      actionButton("action_time_update_cancel", "Cancel"),
      p(),
      actionButton("action_time_delete", "Delete",
        icon = icon("exclamation-triangle"), color = "red")
    )
    do.call(box, args = list(container, title = "Update time",
      status = "danger", width = NULL))
  } else if (ui_decision == "cancel update") {
    ## Same as `ui_decision == "cancel update"` //
    container$buttons <- div(style="display:inline-block",
      actionButton("action_time_log", "Log")
    )
    do.call(box, args = list(container, title = "Log time",
      status = "primary", width = NULL))
  }

  ## Remember counter //
  GLOBALS$buffer$action_time_update_cancel <- action_time_update_cancel

  value
}

# Dynamic UI: display logged times ------------------------------------------------

createDynamicUi_displayLoggedTimes <- function(input, output) {
  ## Dependencies //
  action_selected_row <- input$dt_issues_rows_selected

  container <- list()
  container$dt_times <- DT::dataTableOutput("dt_times")

  ## Bundle in box //
  value <- if (length(action_selected_row)) {
    do.call(box, args = list(container, title = "Logged times",
      status = "primary", width = NULL))
  } else {
    div()
  }
  value
}

# Dynamic UI: experimental ----------------------------------------------

createDynamicUi_experimental <- function(
  input,
  output,
  ui_decision,
  debug = GLOBALS$debug$enabled
) {
  if (debug) {
    message("Dynamic UI: experimental ----------")
    print(Sys.time())
  }

  ## Form components //
  container <- list()

  field <- "exp_field"
  name <- "Something"
  value <- ""
  container[[field]] <- textInput(field, name, value)

  ## Bundle in box //
  value <- if (ui_decision == "hide") {
    div()
  } else if (ui_decision == "show") {
    container$buttons <- div(style="display:inline-block",
      actionButton("action_exp_create", "Create"),
      actionButton("action_exp_cancel", "Cancel")
    )
    do.call(box, args = list(container, title = "Experimental dynamic UI",
      status = "primary", width = NULL))
  } else {
    "Not implemented yet"
  }
  # print(value)
  value
}

# Bundle input data -------------------------------------------------------

bundleInputData_dbTableIssues <- function(input) {
  fields <- c(
    GLOBALS$db$tables$issues$public_fields_compact,
    GLOBALS$db$tables$issues$public_fields_details
  )
  data <- sapply(sapply(fields, "[[", "name"), function(x) input[[x]])
  data
}

bundleInputData_dbTableTimes <- function(input) {
  fields <- c(
    GLOBALS$db$tables$times$public_fields_compact,
    GLOBALS$db$tables$times$public_fields_details
  )
  # fields <- paste0("issue_", sapply(fields, "[[", "name"))
  fields <- sapply(fields, "[[", "name")
  data <- sapply(fields, function(x) input[[x]])

  data$issue_time_logged <- handleTime(data$issue_time_logged)
  data$issue_time_logged_date <- as.character(data$issue_time_logged_date)
  ## TODO: make transformations more transparent and more robust
  ## against name changes

  data
}

# UIDs --------------------------------------------------------------------

getUids_dbTableIssues <- function(input) {
  idx <- input$dt_issues_rows_selected
  dat <- loadData(table = GLOBALS$db$tables$issues$tablename)
  dat[idx, "_uid"]
}

getUids_dbTableTimes <- function(input) {
  idx <- input$dt_times_rows_selected
  dat <- loadData(table = GLOBALS$db$tables$times$tablename)
  dat[idx, "_uid"]
}

# Perform actions ---------------------------------------------------------

performAction_createIssue <- function(input_bundles = list(), uids = list()) {
  saveData(
    data = input_bundles$inputbundle_db_table_issues(),
    table = GLOBALS$db$tables$issues$tablename,
    uid = uids$uid_issues()
  )
}

performAction_updateIssue <- function(input_bundles = list(), uids = list()) {
  saveData(
    data = input_bundles$inputbundle_db_table_issues(),
    table = GLOBALS$db$tables$issues$tablename,
    uid = uids$uid_issues()
  )
}

performAction_deleteIssue <- function(input_bundles = list(), uids = list()) {
  deleteData(
    table = GLOBALS$db$tables$issues$tablename,
    uid = uids$uid_issues(),
    dependent = c(
      GLOBALS$db$tables$times$tablename
    )
  )
}

performAction_logTime <- function(input_bundles = list(), uids = list()) {
  logTime(
    table = GLOBALS$db$tables$issues$tablename,
    uid = uids$uid_issues(),
    values = input_bundles$inputbundle_db_table_times())
}

performAction_updateTime <- function(input_bundles = list(), uids = list()) {
  ## Update table: times //
  updateSpecific(
    table = GLOBALS$db$tables$times$tablename,
    uid = uids$uid_times(),
    values = as.list(input_bundles$inputbundle_db_table_times()),
    refuid = uids$uid_issues()
  )

  ## Update dependent table: issues //
  ## Load relevant logged times in order to sum it up
  dat <- loadData(
    table = GLOBALS$db$tables$times$tablename,
    refuid = uids$uid_issues()
  )
  updateSpecific(
    table = GLOBALS$db$tables$issues$tablename,
    uid = uids$uid_issues(),
    values = list(issue_time_spent = sum(dat$issue_time_logged))
  )
}

performAction_deleteTime <- function(input_bundles = list(), uids = list()) {
  ## Update table: times //
  deleteData(
    table = GLOBALS$db$tables$times$tablename,
    uid = uids$uid_times()
  )

  ## Update dependent table: issues //
  ## Load relevant logged times in order to sum it up
  dat <- loadData(
    table = GLOBALS$db$tables$times$tablename,
    refuid = uids$uid_issues()
  )
  updateSpecific(
    table = GLOBALS$db$tables$issues$tablename,
    uid = uids$uid_issues(),
    values = list(issue_time_spent = sum(dat$issue_time_logged))
  )
}

# Render results ----------------------------------------------------------

renderResults_dbTableIssues <- function(input, uids = list()) {
  ## Dependencies: issues //
  input$action_task_create_2
  input$action_task_update
  input$action_task_delete

  ## Dependencies: times //
  input$action_task_update
  input$action_time_log
  input$action_time_update
  input$action_time_delete

  data <- loadData(table = GLOBALS$db$tables$issues$tablename)
  prepareForDisplay(data, GLOBALS$db$tables$issues$public_fields_compact)
}

renderResults_dbTableTimes <- function(input, uids = list()) {
  ## Dependencies: issues //
  action_dt_issues_rows_selected <- input$dt_issues_rows_selected
  input$action_task_delete

  ## Dependencies: times //
  input$action_time_log
  input$action_time_update
  input$action_time_delete

  value <- if (length(action_dt_issues_rows_selected)) {
    dat <- loadData(
      table = GLOBALS$db$tables$times$tablename,
      refuid = uids$uid_issues()
    )
    prepareForDisplay(dat, GLOBALS$db$tables$times$public_fields_compact)
  } else {
    data.frame()
  }
}

# Debug infos -------------------------------------------------------------

handleDebugInfo <- function(input, output) {
  output$debug_selection = renderPrint({
    ## Dependencies //
    action_dt_issues_rows_selected = input$dt_issues_rows_selected
    action_dt_times_rows_selected = input$dt_times_rows_selected

    if (length(action_dt_issues_rows_selected)) {
      cat('Table issues: selected rows:\n')
      cat(action_dt_issues_rows_selected, sep = ', ')
      cat("\n\n")
      cat('UIDs:\n')
      tmp <- loadData("issues")[action_dt_issues_rows_selected, "_uid"]
      cat(tmp, sep = ', ')
      cat("\n\n")
    }

    if (length(action_dt_times_rows_selected)) {
      cat('Table times: selected rows:\n')
      cat(action_dt_times_rows_selected, sep = ', ')
      cat("\n\n")
      cat('UIDs:\n')
      tmp <- loadData("times")[action_dt_times_rows_selected, "_uid"]
      cat(tmp, sep = ', ')
    }
  })

  #     output$dt_issues_cells = renderPrint({
  #       s = input$dt_issues_cells_selected
  #       if (length(s)) {
  #         cat('These cells were selected:\n\n')
  #         print(s)
  #       }
  #     })
  #     output$selection = DT::renderDataTable({
  #       dat <- loadData(table = table_2)
  #       dat[input$dt_issues_rows_selected, ]
  #     })
}

# DEPRECATED  -------------------------------------------------------------

if (FALSE) {
  dynamicUi_logTime <- function(input, output) {
    output$ui_form_logtime <- renderUI({
      ## Dependencies //
      action_selected_row <- input$dt_issues_rows_selected
      action_time_log <- input$action_time_log

      ## Aux function //
      getFormValue <- function(field, idx, default = "") {
        if (!is.null(idx)) {
          dat <- loadData(table = "issues")
          dat[idx, field]
        } else {
          value <- isolate(input[[field]])
          if (is.null(value)) {
            default
          } else {
            value
          }
        }
      }

      ## Form components //
      container <- list()

      field <- "issue_time_logged"
      name <- "Time spent"
      value <- getFormValue(field = field, idx = action_selected_row)
      container[[field]] <- textInput(field, name, value)

      field <- "issue_time_logged_date"
      name <- "Date"
      value <- getFormValue(field = field, idx = action_selected_row,
        default <- as.character(Sys.Date()))
      container[[field]] <- dateInput(field, name, value = value)

      field <- "issue_time_logged_description"
      name <- "Work description"
      value <- getFormValue(field = field, idx = action_selected_row)
      container[[field]] <- textInput(field, name, value)

      field <- "action_time_log"
      name <- "Log"
      # value <- getFormValue(field = field, idx = action_selected_row)
      container[[field]] <- actionButton(field, name)

      ## Bundle in box //
      value <- if (length(action_selected_row)) {
        do.call(box, args = list(container, title = "Log time",
          status = "primary", width = NULL))
      } else {
        div()
      }
      value
    })
  }
}
