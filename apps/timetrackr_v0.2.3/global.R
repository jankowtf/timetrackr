



# Variables ---------------------------------------------------------------

GLOBAL <- list()
GLOBAL$debug$enabled <- TRUE

GLOBAL$valids <- list(
  issueStatus = VALIDS_issueStatus
)

# Buffer ------------------------------------------------------------------

GLOBAL$buffer <- new.env()

## Initialize counter //
GLOBAL$buffer$action_task_create <- 0
GLOBAL$buffer$action_task_create_2 <- 0
GLOBAL$buffer$action_task_create_cancel <- 0
GLOBAL$buffer$action_task_update_cancel <- 0
GLOBAL$buffer$action_time_update_cancel <- 0

# Database ----------------------------------------------------------------

sqlite_path <- "data/db/timetracking-db"

cloneNames <- function(input,
  subsets = character(),
  field = "name") {
  if (!length(subsets)) {

  } else {
    for (subset in subsets) {
      nms <- sapply(input[[subset]], "[[", field)
      if (is.list(nms)) {
        stop(sprintf(
          "Base field for cloning not consistently available: %s",
          field
        ))
      }
      names(input[[subset]]) <- nms
    }
  }
  input
}

GLOBAL$db$tables$issues$tablename <- "issues"
GLOBAL$db$tables$times$tablename <- "times"

GLOBAL$db$tables$issues$public_fields_compact <- list(
  list(
    name = "issue_summary",
    nicename = "Summary",
    datatype = "TEXT",
    # datatype_r = "character",
    default = ""
  ),
  list(
    name = "issue_project",
    nicename = "Project",
    datatype = "TEXT",
    # datatype_r = "character",
    default = ""
  ),
  list(
    name = "issue_date",
    nicename = "Date",
    datatype = "TEXT",
    # datatype_r = "Date",
    default = Sys.Date()
  ),
  list(
    name = "issue_week",
    nicename = "Week",
    datatype = "INTEGER",
    default = as.integer(format(Sys.Date(), "%V"))
  ),
  list(
    name = "issue_status",
    nicename = "Status",
    datatype = "TEXT",
    default = GLOBAL$valids$issueStatus()["todo"]
  ),
  list(
    name = "issue_time_estimated",
    nicename = "Time estimated",
    datatype = "REAL",
    default = as.numeric(1)
  ),
  list(
    name = "issue_time_spent",
    nicename = "Time spent",
    datatype = "REAL",
    default = as.numeric(0)
  ),
  list(
    name = "issue_time_until_done",
    nicename = "Time until done",
    datatype = "REAL",
    default = as.numeric(1)
  )
)

GLOBAL$db$tables$issues$public_fields_details <- list(
  list(
    name = "issue_description",
    nicename = "Description",
    datatype = "TEXT",
    default = ""
  ),
  list(
    name = "issue_unplanned",
    nicename = "Unplanned",
    datatype = "TEXT",
    default = FALSE
  ),
  list(
    name = "issue_interruption",
    nicename = "Interruption",
    datatype = "TEXT",
    default = FALSE
  ),
  list(
    name = "issue_meeting",
    nicename = "Meeting",
    datatype = "TEXT",
    default = FALSE
  )
)
GLOBAL$db$tables$issues$private_fields <- list(
  list(name = "_uid", datatype = "TEXT"),
  list(name = "_time_created", datatype = "TEXT"),
  list(name = "_time_modified", datatype = "TEXT")
)

GLOBAL$db$tables$issues <- cloneNames(
  input = GLOBAL$db$tables$issues,
  subset = c(
    "public_fields_compact",
    "public_fields_details",
    "private_fields"
  )
)

## Times //
GLOBAL$db$tables$times$public_fields_compact <- list(
  list(
    name = "issue_time_logged_date",
    nicename = "Date",
    datatype = "TEXT",
    default = Sys.Date()
  ),
  list(
    name = "issue_time_logged",
    nicename = "Time",
    datatype = "REAL",
    default = as.numeric(1)
  ),
  list(
    name = "issue_time_logged_week",
    nicename = "Week",
    datatype = "INTEGER",
    default = as.integer(format(Sys.Date(), "%V"))
  )
)
GLOBAL$db$tables$times$public_fields_details <-
  list(
    list(
      name = "issue_time_logged_description",
      nicename = "Description",
      datatype = "TEXT",
      default = ""
    )
  )
GLOBAL$db$tables$times$private_fields <- list(
  list(name = "_uid", datatype = "TEXT"),
  list(name = "_refuid", datatype = "TEXT"),
  list(name = "_time_created", datatype = "TEXT"),
  list(name = "_time_modified", datatype = "TEXT")
)
GLOBAL$db$tables$times <- cloneNames(
  input = GLOBAL$db$tables$times,
  subset = c(
    "public_fields_compact",
    "public_fields_details",
    "private_fields"
  )
)

# Instantiate -------------------------------------------------------------

# App.Timetracking$debug("importData")
# App.Timetracking$debug("getSelectedTimeSeries")

app <- App.Timetracking$new(injected = list(fs_con = NULL,
  db_con = NULL))

# Configs -----------------------------------------------------------------

# app$loadConfigs("data/config")

# Dynamic UI: issue details ----------------------------------------------

composeUi_issueDetails <- function(input,
  output,
  ui_control,
  debug = GLOBAL$debug$enabled) {
  ## Dependencies //
  action_selected_row <- ui_control$selected

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
  # container[[field]] <- textInput(field, name, value)
  # container$title <- tags$strong("Description")
  # container$space <- p()
  container[[field]] <- div(
    class = "form-group shiny-input-container",
    tags$label("for" = name, name),
    tags$textarea(id = field, rows = 3, cols = 30, value, class = "form-control")
  )

  field <- "issue_project"
  name <- "Project"
  value <- getFormValue(field = field, idx = action_selected_row)
  container[[field]] <- textInput(field, name, value)

  # field <- "issue_week"
  # name <- "Week"
  # value <- getFormValue(field = field, idx = action_selected_row)
  # if (is.na(value)) {
  #   value <- as.numeric(format(Sys.Date(), "%V"))
  # }
  # container[[field]] <- numericInput(field, name, value,
  #   min = 1, max = 53, step = 1)
  ## TODO 2015-12-29:
  ## App is actually designed to just plan tasks for weeks and not specific
  ## days. Implement a switch that lets you do that (i.e. "disabling" the
  ## date input and just used weeks)

  field <- "issue_date"
  name <- "Date"
  value <- getFormValue(field = field,
    idx = action_selected_row,
    # default = as.character(Sys.Date())
    default = GLOBAL$db$tables$issues$public_fields_compact[[field]]$default
  )
  container[[field]] <- dateInput(field, name, value = value)

  field <- "issue_status"
  name <- "Status"
  value <- getFormValue(field = field,
    idx = action_selected_row,
    # default = GLOBAL$valids$issueStatus()["todo"]
    default = GLOBAL$db$tables$issues$public_fields_compact[[field]]$default
  )
  # print(value)
  container[[field]] <- selectInput(field, name,
    unname(GLOBAL$valids$issueStatus()), selected = unname(value))
  # print(container[[field]])
  ## TODO 2015-12-29: check why pre-selection does not work

  field <- "issue_time_estimated"
  name <- "Time estimated"
  value <- getFormValue(field = field, idx = action_selected_row)
  container[[field]] <- textInput(field, name, value)

  container$goto_info <- actionLink("goto_info", "Valid time formats")

  field <- "issue_unplanned"
  name <- "Unplanned"
  value <- as.logical(getFormValue(
    field = field,
    idx = action_selected_row,
    default = FALSE
  ))
  container[[field]] <- checkboxInput(field, name, value = value)

  field <- "issue_interruption"
  name <- "Interruption"
  value <- as.logical(getFormValue(
    field = field,
    idx = action_selected_row,
    default = FALSE
  ))
  container[[field]] <- checkboxInput(field, name, value = value)

  field <- "issue_meeting"
  name <- "Meeting"
  value <-
    as.logical(getFormValue(
      field = field,
      idx = action_selected_row,
      default = FALSE
    ))
  container[[field]] <- checkboxInput(field, name, value = value)

  ## Bundle in box //
  value <- if (ui_control$case == "create") {
    container$buttons <- div(
      style = "display:inline-block",
      actionButton("action_task_create_2", "Create"),
      actionButton("action_task_create_cancel", "Cancel")
    )
    do.call(box,
      args = list(
        container,
        title = "Create task",
        status = "primary",
        width = NULL
      ))
  } else if (ui_control$case == "update") {
    container$buttons <- div(
      style = "display:inline-block",
      actionButton("action_task_update", "Update"),
      # actionButton("action_task_update_cancel", "Cancel"),
      p(),
      actionButton(
        "action_task_delete",
        "Delete",
        icon = icon("exclamation-triangle")
      )
    )
    do.call(box,
      args = list(
        container,
        title = "Update task",
        status = "danger",
        width = NULL
      ))
  } else {
    stop("Not implemented")
  }
  value
}

# Dynamic UI: create time ----------------------------------------------------

composeUi_timeDetails <- function(input,
  output,
  ui_control,
  debug = GLOBAL$debug$enabled) {
  ## Dependencies //
  action_dt_issues_selected_row <- ui_control$selected_issue
  action_dt_times_selected_row <- ui_control$selected_times

  ## Aux function //
  getFormValue <- function(field, idx, default = "") {
    if (!is.null(idx)) {
      dat <- loadData(table = GLOBAL$db$tables$times$tablename)
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
  value <-
    getFormValue(field = field, idx = action_dt_times_selected_row)
  container[[field]] <- textInput(field, name, value)

  container$goto_info <-
    actionLink("goto_info", "Valid time formats")
  # container$p <- p(HTML("<a href='#timeformats'>Valid time formats</a> (link broken, see info tab)"))
  #   container$timeformats <- div(id="linkToTimeFormats",
  #     tags$a("Valid time formats (see info tab)"))

  field <- "issue_time_logged_date"
  name <- "Date"
  value <-
    getFormValue(field = field,
      idx = action_dt_times_selected_row,
      default <- as.character(Sys.Date()))
  container[[field]] <- dateInput(field, name, value = value)

  field <- "issue_time_logged_description"
  name <- "Work description"
  value <- getFormValue(field = field, idx = action_dt_times_selected_row)
  # container[[field]] <- textInput(field, name, value)
  container[[field]] <- div(
    class = "form-group shiny-input-container",
    tags$label("for" = name, name),
    tags$textarea(id = field, rows = 3, cols = 30, value, class = "form-control")
  )

  value <- if (ui_control$case == "create") {
    container$buttons <- div(style = "display:inline-block",
      actionButton("action_time_create", "Log"))
    do.call(box,
      args = list(
        container,
        title = "Log time",
        status = "primary",
        width = NULL
      ))
  } else if (ui_control$case == "update") {
    container$buttons <- div(
      style = "display:inline-block",
      actionButton("action_time_update", "Update"),
      actionButton("action_time_update_cancel", "Cancel"),
      p(),
      actionButton(
        "action_time_delete",
        "Delete",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    )
    do.call(box,
      args = list(
        container,
        title = "Update time",
        status = "danger",
        width = NULL
      ))
  } else {
    stop("Not implemented")
  }

  value
}

# Dynamic UI: display times ------------------------------------------------

composeUi_displayTimes <- function(input,
  output,
  ui_control_ref) {
  ## Dependencies //
  selected_ref <- ui_control_ref$selected

  container <- list()
  container$dt_times <- DT::dataTableOutput("dt_times")

  ## Bundle in box //
  do.call(box,
    args = list(
      container,
      title = "Logged times",
      status = "primary",
      width = NULL
    ))
}

# Bundle input data -------------------------------------------------------

bundleInputData_dbTableIssues <- function(input) {
  fields <- c(
    GLOBAL$db$tables$issues$public_fields_compact,
    GLOBAL$db$tables$issues$public_fields_details
  )
  data <-
    sapply(sapply(fields, "[[", "name"), function(x)
      input[[x]])
  data$issue_date <- as.character(data$issue_date)
  data
}

bundleInputData_dbTableTimes <- function(input) {
  fields <- c(
    GLOBAL$db$tables$times$public_fields_compact,
    GLOBAL$db$tables$times$public_fields_details
  )
  # fields <- paste0("issue_", sapply(fields, "[[", "name"))
  fields <- sapply(fields, "[[", "name")
  data <- sapply(fields, function(x)
    input[[x]])

  data$issue_time_logged <- handleTime(data$issue_time_logged)
  data$issue_time_logged_date <-
    as.character(data$issue_time_logged_date)
  ## TODO: make transformations more transparent and more robust
  ## against name changes

  data
}

# UIDs --------------------------------------------------------------------

getUids_dbTableIssues <- function(input) {
  idx <- input$dt_issues_rows_selected
  dat <- loadData(table = GLOBAL$db$tables$issues$tablename)
  dat[idx, "_uid"]
}

getUids_dbTableTimes <- function(input) {
  idx <- input$dt_times_rows_selected
  dat <- loadData(table = GLOBAL$db$tables$times$tablename)
  dat[idx, "_uid"]
}

# Perform actions ---------------------------------------------------------

act_createIssue <- function(input_bundles = list(),
  uids = list()) {
  saveData(
    data = input_bundles$bundle_db_table_issues(),
    table = GLOBAL$db$tables$issues$tablename,
    uid = uids$uid_issues()
  )
}

act_updateIssue <- function(input_bundles = list(),
  uids = list()) {
  saveData(
    data = input_bundles$bundle_db_table_issues(),
    table = GLOBAL$db$tables$issues$tablename,
    uid = uids$uid_issues()
  )
}

act_deleteIssue <- function(input_bundles = list(),
  uids = list()) {
  deleteData(
    table = GLOBAL$db$tables$issues$tablename,
    uid = uids$uid_issues(),
    dependent = c(GLOBAL$db$tables$times$tablename)
  )
}

act_createTime <- function(input_bundles = list(),
  uids = list()) {
  logTime(
    table = GLOBAL$db$tables$issues$tablename,
    uid = uids$uid_issues(),
    values = input_bundles$bundle_db_table_times()
  )
}

act_updateTime <- function(input_bundles = list(),
  uids = list()) {
  ## Update table: times //
  updateSpecific(
    table = GLOBAL$db$tables$times$tablename,
    uid = uids$uid_times(),
    values = as.list(input_bundles$bundle_db_table_times()),
    refuid = uids$uid_issues()
  )

  ## Update dependent table: issues //
  ## Load relevant logged times in order to sum it up
  dat <- loadData(table = GLOBAL$db$tables$times$tablename,
    refuid = uids$uid_issues())
  updateSpecific(
    table = GLOBAL$db$tables$issues$tablename,
    uid = uids$uid_issues(),
    values = list(issue_time_spent = sum(dat$issue_time_logged))
  )
}

act_deleteTime <- function(input_bundles = list(),
  uids = list()) {
  ## Update table: times //
  deleteData(table = GLOBAL$db$tables$times$tablename,
    uid = uids$uid_times())

  ## Update dependent table: issues //
  ## Load relevant logged times in order to sum it up
  dat <- loadData(table = GLOBAL$db$tables$times$tablename,
    refuid = uids$uid_issues())
  updateSpecific(
    table = GLOBAL$db$tables$issues$tablename,
    uid = uids$uid_issues(),
    values = list(issue_time_spent = sum(dat$issue_time_logged))
  )
}

act_resetIssueInput <- function(session) {
  handleInputUpdate <- function(field) {
    name <- field$name
    default <- field$default
    switch(
      class(default),
      "character" = updateTextInput(session, name, value = default),
      "integer" = updateNumericInput(session, name, value = default),
      "numeric" = updateNumericInput(session, name, value = default),
      "Date" = updateDateInput(session, name, value = default),
      "logical" = updateCheckboxInput(session, name, value = default)
    )
  }

  sapply(GLOBAL$db$tables$issues$public_fields_compact, function(field) {
    handleInputUpdate(field)
  })

  sapply(GLOBAL$db$tables$issues$public_fields_details, function(field) {
    handleInputUpdate(field)
  })
}

# Render results ----------------------------------------------------------

renderResults_dbTableIssues <- function() {
  data <- loadData(table = GLOBAL$db$tables$issues$tablename)
  prepareForDisplay(data, GLOBAL$db$tables$issues$public_fields_compact)
}

renderResults_dbTableTimes <- function(uids = list(),
  ui_control_ref = list()) {
  ## Dependencies: issues //
  selected_ref <- ui_control_ref$selected

  value <- if (length(selected_ref)) {
    dat <- loadData(table = GLOBAL$db$tables$times$tablename,
      refuid = uids$uid_issues())
    prepareForDisplay(dat, GLOBAL$db$tables$times$public_fields_compact)
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
      tmp <-
        loadData("issues")[action_dt_issues_rows_selected, "_uid"]
      cat(tmp, sep = ', ')
      cat("\n\n")
    }

    if (length(action_dt_times_rows_selected)) {
      cat('Table times: selected rows:\n')
      cat(action_dt_times_rows_selected, sep = ', ')
      cat("\n\n")
      cat('UIDs:\n')
      tmp <-
        loadData("times")[action_dt_times_rows_selected, "_uid"]
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
