##------------------------------------------------------------------------------
## Meta //
##------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)

debug_mode <- FALSE

## App name //
app_name <- "Time tracking"

## App stage //
app_stage <- "v1.1"

app_stages <- c(
  "v1.0",
  "v1.1"
)

## App stages -----

validateAppStage <- function(
  stage,
  .stages
) {
  match.arg(stage, .stages)
}
app_stage <- validateAppStage(app_stage, .stages = app_stages)


# Version 1.0 -------------------------------------------------------------

if (app_stage == "v1.0") {
  source("global.R")

  # Define the fields we want to save from the form
  fields <- c("name", "used_shiny", "r_num_years")

  # Shiny app with 3 fields that the user can submit data for
  ui <- fluidPage(
    DT::dataTableOutput("responses", width = 300), tags$hr(),
    textInput("name", "Name", ""),
    checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
    sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
    actionButton("submit", "Submit")
  )
  server <- function(input, output, session) {
    app$prepare()

    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })

    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })

    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })
  }
}

# Version 1.1 -------------------------------------------------------------

if (app_stage == "v1.1") {
  source("global.R")
  source("dependencies.R")

  # Shiny app with 3 fields that the user can submit data for
  ui <- dashboardPage(
    ## Header //
    dashboardHeader(title = app_name),

    ## Sidebar content //
    dashboardSidebar(
      sidebarMenu(
        menuItem("Tasks", tabName = "tasks",
          icon = icon("database")),
        menuItem("Projects", tabName = "projects",
          icon = icon("list")),
        menuItem("Admin access", tabName = "admin_access",
          icon = icon("lock")),
        tags$hr(),
        menuItem("Info", tabName = "info", icon = icon("info")),
        menuItem("Experimental", tabName = "experimental", icon = icon("flask"))
      )
    ),
    ## Body content
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "tasks",
          fluidRow(
            column(width = 7,
              div()
            ),
            column(width = 3,
              actionButton("action_task_create", "Create task")
            )
          ),
          p(),
          fluidRow(
            column(width = 7,
              box(
                title = "Task list",
                DT::dataTableOutput("dt_issues"),
                width = NULL
              ),
              uiOutput("ui_form_loggedtimes")
            ),
            column(width = 3,
              uiOutput('ui_form_logtime'),
              uiOutput("ui_form_taskdetails")
            )
          ),
          fluidRow(
            column(width = 6,
              box(
                title = "Selection info",
                verbatimTextOutput('debug_selection'),
                width = NULL
              )
            )
          )
        ),
        tabItem(
          tabName = "projects",
          fluidRow(
            column(width = 4,
              box(
                title = "Projects",
                selectInput("projects_list", "Projects list", letters, letters[1]),
                status = "primary",
                width = NULL
              )
            )
          )
        ),
        tabItem(
          tabName = "admin_access",
          fluidRow(

          )
        ),
        tabItem(
          tabName = "info",
          fluidRow(
            box(
              title = "Time format",
              p("Possible formats for time inputs (examples):"),
              div("* 1.5: 1.5 hours"),
              div("* 1.5d: 12 hours (a day has 8 hours)"),
              div("* 1.5h: 1.5 hours"),
              div("* 45m: 0.75 hours"),
              div("* 1d 2h 30m: 10.5 hours"),
              p(),
              strong("NOTE:"),
              p(),
              div(strong("* Everything is transformed to hours before DB commit")),
              width = 12
            )
          )
        ),
        tabItem("experimental",
          fluidRow(
            box(
              selectInput(
                "plotType", "Plot Type",
                c(Scatter = "scatter",
                  Histogram = "hist")),

              # Only show this panel if the plot type is a histogram
              conditionalPanel(
                condition = "input.plotType == 'hist'",
                selectInput(
                  "breaks", "Breaks",
                  c("Sturges",
                    "Scott",
                    "Freedman-Diaconis",
                    "[Custom]" = "custom")),

                # Only show this panel if Custom is selected
                conditionalPanel(
                  condition = "input.breaks == 'custom'",
                  sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
                )
              )
            ),
            conditionalPanel(
              condition = "input.dt_issues_rows_selected > 0",
              box(
                sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
              )
            )
          ),
          fluidRow(
            box(actionButton("action_exp_1", "Trigger 1"), width = 3),
            uiOutput("ui_experimental"),
            box(textOutput("exp"))
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    app$prepare(
      public_fields_compact = GLOBALS$db$tables$issues$public_fields_compact,
      public_fields_details = GLOBALS$db$tables$issues$public_fields_details,
      private_fields = GLOBALS$db$tables$issues$private_fields,
      times_public_fields_compact = GLOBALS$db$tables$times$public_fields_compact,
      times_public_fields_details = GLOBALS$db$tables$times$public_fields_details,
      times_private_fields = GLOBALS$db$tables$times$private_fields
    )

    ##################
    ## Bundle input ##
    ##################

    inputbundle_db_table_issues <- reactive({
      bundleInputData_dbTableIssues(input)
    })
    inputbundle_db_table_times <- reactive({
      bundleInputData_dbTableTimes(input)
    })

    ################
    ## Dynamic UI ##
    ################

    output$ui_form_taskdetails <- renderUI({
      createDynamicUi_issueDetails(input, output)
    })
    output$ui_form_logtime <- renderUI({
      createDynamicUi_logtime(input, output)
    })
    output$ui_form_loggedtimes <- renderUI({
      createDynamicUi_displayLoggedTimes(input, output)
    })

    ## UIDs //
    uid_issues <- reactive({
      getUids_dbTableIssues(input = input)
    })
    uid_times <- reactive({
      getUids_dbTableTimes(input = input)
    })

    #############
    ## Actions ##
    #############

    ## Create issue //
    observeEvent(input$action_task_create_2, {
      performAction_createIssue(input_bundles = list(
        inputbundle_db_table_issues = inputbundle_db_table_issues
      ), uids = list(uid_issues = uid_issues))
    })
    ## Update issue //
    observeEvent(input$action_task_update, {
      performAction_updateIssue(input_bundles = list(
        inputbundle_db_table_issues = inputbundle_db_table_issues
      ), uids = list(uid_issues = uid_issues))
    })
    ## Delete issue //
    observeEvent(input$action_task_delete, {
      performAction_deleteIssue(input_bundles = list(
        inputbundle_db_table_issues = inputbundle_db_table_issues
      ), uids = list(uid_issues = uid_issues))
    })

    ## Log time //
    observeEvent(input$action_time_log, {
      performAction_logTime(input_bundles = list(
        inputbundle_db_table_times = inputbundle_db_table_times
      ), uids = list(uid_issues = uid_issues))
    })
    ## Update time //
    observeEvent(input$action_time_update, {
      performAction_updateTime(input_bundles = list(
        inputbundle_db_table_times = inputbundle_db_table_times
      ), uids = list(uid_issues = uid_issues, uid_times = uid_times))
    })
    ## Delete time //
    observeEvent(input$action_time_delete, {
      performAction_deleteTime(input_bundles = list(
        inputbundle_db_table_times = inputbundle_db_table_times
      ), uids = list(uid_issues = uid_issues, uid_times = uid_times))
    })

    ####################
    ## Render results ##
    ####################

    ## Issues //
    output$dt_issues <- DT::renderDataTable({
      renderResults_dbTableIssues(input)
    }, filter = "top",
      width = "100%", class = "cell-border stripe",
      selection = "single",
      options = list(
        dom = "ltipr",
        autoWidth = TRUE,
        columnDefs = list(list(width = '300px', targets = "_all"))

      )
    )
    ## Times //
    output$dt_times <- DT::renderDataTable({
      renderResults_dbTableTimes(input,
        uids = list(uid_issues = uid_issues))
    }, selection = "single", options = list(dom = "ltipr"))

    ###########
    ## DEBUG ##
    ###########

    handleDebugInfo(input = input, output = output)

    ##################
    ## EXPERIMENTAL ##
    ##################

    reactives <- reactiveValues(
      action_exp_1 = 0,
      action_exp_1__last = 0,
      action_exp_cancel = 0,
      action_exp_cancel__last = 0,
      ui_decision = "hide"
    )
    ui_decision <- reactive({

      ## Dependencies //
      ## Trigger button:
      value <- input$action_exp_1
      if (reactives$action_exp_1 != value) reactives$action_exp_1 <- value

      ## Cancel button that is dynamically created within `createDynamicUi_experimental`
      value <- input$action_exp_cancel
      if (is.null(value)) {
        value <- 0
      }
      if (reactives$action_exp_cancel != value) reactives$action_exp_cancel <- value

      if (GLOBALS$debug$enabled) {
        message("Dependency clearance -----")
        message("action_exp_1:")
        print(reactives$action_exp_1)
        print(reactives$action_exp_1__last)
        message("action_exp_cancel:")
        print(reactives$action_exp_cancel)
        print(reactives$action_exp_cancel__last)
      }
      ui_decision <- if (
        c (reactives$action_exp_1 == 0 && reactives$action_exp_1 == 0) ||
          c(
            reactives$action_exp_1 > 0 &&
              reactives$action_exp_1 <= reactives$action_exp_1__last &&

              reactives$action_exp_cancel > 0 &&
              reactives$action_exp_cancel > reactives$action_exp_cancel__last
          )
      ) {
        "hide"
      } else if (
        reactives$action_exp_1 >= reactives$action_exp_1__last
      ) {
        reactives$action_exp_cancel__last <- reactives$action_exp_cancel
        "show"
      } else {
        "Not implemented yet"
      }
      if (GLOBALS$debug$enabled) {
        print(ui_decision)
      }
      ## Synchronize //
      reactives$action_exp_1__last <- reactives$action_exp_1

      reactives$ui_decision <- ui_decision

      # Sys.sleep(1)
      ## --> just to be able to escape infinite recursions
    })

    output$ui_experimental <- renderUI({
      ui_decision()
      createDynamicUi_experimental(input, output,
        ui_decision = reactives$ui_decision)
    })

    # output$exp <- renderPrint({dep_clearance()})
    output$exp <- renderPrint({reactiveValuesToList(reactives)})
  }
}

# Launch ------------------------------------------------------------------

shinyApp(ui, server)
