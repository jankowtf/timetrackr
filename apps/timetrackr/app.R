##------------------------------------------------------------------------------
## Meta //
##------------------------------------------------------------------------------
# library(shiny)
# library(DT)
# install.packages("DT")
# devtools::install_github('rstudio/DT')

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
        menuItem("Info", tabName = "info",
          icon = icon("info"))
      )
    ),
    ## Body content
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "tasks",
          fluidRow(
            column(width = 7, div()),
            column(width = 3,
              actionButton("action_task_create", "Create task", type = "toggle")
            ),
            tags$hr()
          ),
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
              uiOutput('ui_form_logtime2'),
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
            column(width = 6,
              box(
                DT::dataTableOutput("timetracking2", width = 300),
                width = NULL
              )
            ),
            column(width = 4,
              uiOutput('admin_ui'),
              uiOutput('delete_ui')
            )
          ),
          fluidRow(
            box(
              title = "Selection info",
              verbatimTextOutput('timetracking2_rows')
            )
          )
        ),
        tabItem(
          tabName = "info",
          fluidRow(
            box(
              title = "Time format",
              p("Possible formats (examples):"),
              div("* '1.5' (treated as hours)"),
              div("* '1.5d' (days, a day has 8 hours)"),
              div("* '1.5h' (hours)"),
              div("* '1.5m' (minutes)"),
              div("* '1d 2h 30m'"),
              p(),
              strong("NOTE:"),
              strong("* No quotes necessary for input"),
              strong("* Everything is transformed to hours before DB write")
            )
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
    output$ui_form_logtime2 <- renderUI({
      createDynamicUi_logTime2(input, output)
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
    }, # filter = "top",
      width = "100%", class = "cell-border stripe",
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '300px', targets = "_all"))
      )
    )
    ## Times //
    output$dt_times <- DT::renderDataTable({
      renderResults_dbTableTimes(input,
        uids = list(uid_issues = uid_issues))
    })

    handleDebugInfo(input = input, output = output)
  }
}

# Start app ---------------------------------------------------------------


shinyApp(ui, server)
