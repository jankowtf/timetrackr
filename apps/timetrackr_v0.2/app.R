
# Meta --------------------------------------------------------------------

library(shiny)
library(shinydashboard)

## App name //
app_name <- "Time tracking"
enable_times <- FALSE

source("dependencies.R")
source("global.R")

# UI ----------------------------------------------------------------------

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
      menuItem("Info", tabName = "info", icon = icon("info"))
      # menuItem("Experimental", tabName = "experimental", icon = icon("flask"))
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
            uiOutput("ui_times_table"),
            box(
              title = "Task list",
              DT::dataTableOutput("dt_issues"),
              width = NULL
            )
          ),
          column(width = 3,
            uiOutput('ui_times'),
            uiOutput("ui_issues")
          )
        )
      ),
      tabItem(
        tabName = "projects",
        fluidRow(
          column(width = 4,
            box(
              title = "Projects (work in progress)",
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
          box(
            title = "Admin access (work in progress)"
          )
        )
      ),
      tabItem(
        tabName = "info",
        fluidRow(
          box(
            title = "Time format",
            div(id = "time_formats",
              HTML("<script>$('#linkToTimeFormats').click(function() {
						 tabs = $('.tabbable .nav.nav-tabs li')
					 	 tabs.each(function() {
							$(this).removeClass('active')
					 	 })
						 $(tabs[1]).addClass('active')

						 tabsContents = $('.tabbable .tab-content .tab-pane')
					 	 tabsContents.each(function() {
							$(this).removeClass('active')
					 	 })
						 $(tabsContents[1]).addClass('active')

						$('#summary').trigger('change').trigger('shown');

					 })</script>
			"),
              p("Possible formats for time inputs (examples):"),
              tags$li("1.5: 1.5 hours"),
              tags$li("1.5d: 12 hours (a day has 8 hours)"),
              tags$li("1.5h: 1.5 hours"),
              tags$li("45m: 0.75 hours"),
              tags$li("1d 2h 30m: 10.5 hours"),
              p(),
              strong("NOTE //"),
              p(),
              tags$li(strong("Everything is transformed to hours before DB commit"))
            ),
            width = 12
          )
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

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

  bundle_db_table_issues <- reactive({
    bundleInputData_dbTableIssues(input)
  })
  bundle_db_table_times <- reactive({
    bundleInputData_dbTableTimes(input)
  })

  ## UIDs //
  uid_issues <- reactive({
    getUids_dbTableIssues(input = input)
  })
  uid_times <- reactive({
    getUids_dbTableTimes(input = input)
  })

  ########################
  ## Dynamic UI: issues ##
  ########################

  ui_control_issues <- reactiveValues(
    case = c("hide", "create", "update")[1],
    selected = NULL,
    refresh = 0,
    initial = TRUE
  )

  observe(
    if (ui_control_issues$initial) {
      ui_control_issues$refresh <- 1
      ui_control_issues$initial <- FALSE
    }
  )
  observe(
    if (!is.null(input$dt_issues_rows_selected)) {
      ui_control_issues$case <- "update"
    }
  )
  observeEvent(input$action_task_create, {
    ui_control_issues$selection <- NULL
    ui_control_issues$case <- "create"

    ## Dependents //
    ui_control_times$case <- "hide"
    ui_control_times$show_table <- FALSE
  })
  observe({
    idx <- input$dt_issues_rows_selected
    if (!is.null(idx)) {
      ui_control_issues$case <- "update"
    } else {
      ui_control_issues$case <- "hide"
    }
    ui_control_issues$selected <- idx
  })

  ## Create issue //
  observeEvent(input$action_task_create_2, {
    performAction_createIssue(input_bundles = list(
      bundle_db_table_issues = bundle_db_table_issues
    ), uids = list(uid_issues = uid_issues))
    ui_control_issues$refresh <- ui_control_issues$refresh + 1
    ui_control_issues$case <- "hide"
  })

  # Cancel create issue //
  observeEvent(input$action_task_create_cancel, {
    ui_control_issues$case <- "hide"
  })

  # Update issue //
  observeEvent(input$action_task_update, {
    performAction_updateIssue(input_bundles = list(
      bundle_db_table_issues = bundle_db_table_issues
    ), uids = list(uid_issues = uid_issues))
    ui_control_issues$refresh <- ui_control_issues$refresh + 1
    ui_control_issues$case <- "hide"
  })

  #   # Cancel update issue //
  #   observeEvent(input$action_task_update_cancel, {
  #     ui_control_issues$case <- "hide"
  #   })

  # Delete issue //
  observeEvent(input$action_task_delete, {
    performAction_deleteIssue(input_bundles = list(
      bundle_db_table_issues = bundle_db_table_issues
    ), uids = list(uid_issues = uid_issues))
    ui_control_issues$refresh <- ui_control_issues$refresh + 1
    ui_control_issues$case <- "hide"
  })

  ## Assemble dynamic UI: issue details //
  output$ui_issues <- renderUI({
    if (ui_control_issues$case == "hide") return()

    createDynamicUi_issueDetails(input, output,
      ui_control = ui_control_issues)
  })

  ## Render //
  output$dt_issues <- DT::renderDataTable({
    ## Dependency trigger //
    ui_control_issues$refresh

    renderResults_dbTableIssues()
  }, filter = "top",
    width = "50%", class = "cell-border stripe",
    selection = "single",
    options = list(
      dom = "ltipr",
      autoWidth = TRUE,
      scrollX = TRUE,
      columnDefs = list(list(width = '8%', targets = "_all"))
    )
  )

  #######################
  ## Dynamic UI: times ##
  #######################

  if (TRUE) {
    ui_control_times <- reactiveValues(
      case = c("hide", "create", "update")[1],
      show_table = FALSE,
      selected = NULL,
      refresh = 0
    )

    ## Selected rows issues table //
    observe({
      idx <- input$dt_issues_rows_selected
      if (!is.null(idx)) {
        # ui_control_times$refresh <- ui_control_times$refresh + 1
        ui_control_times$refresh <- NULL
        ui_control_times$refresh <- TRUE
        ## TODO 2015-12-16: find best-practice for refresh controls
        ## Simply ensuring that `ui_control_times$refresh` is set to TRUE/FALSE
        ## doesn't do trick as only CHANGES in values will trigger re-evaluations
        ## of dependent objects (i.e. `output$ui_times_table` and
        ## `output$dt_times`).
        ## That's why I thought a simple "counting up" would suffice, but then
        ## I run into infinite recursions somehow:
        ##     ui_control_times$refresh <- ui_control_times$refresh + 1
        #              print(ui_control_times$refresh)
        #              Sys.sleep(3) # to have a chance to escape infinite recursion
        ## Current solution makes sure `ui_control_times$refresh` is set to
        ## `NULL` before assigning any other value.
        ## That does the trick, but seems unnecessary

        ui_control_times$case <- "create"
        ui_control_times$show_table <- TRUE
      } else {
        ui_control_times$refresh <- NULL
        ui_control_times$refresh <- FALSE
        ui_control_times$case <- "hide"
        ui_control_times$show_table <- FALSE
      }
      ui_control_times$selected <- idx
    })

    ## Selected rows times table //
    observe({
      idx <- input$dt_times_rows_selected
      if (!is.null(idx)) {
        ui_control_times$case <- "update"
      } else {
        ui_control_times$case <- "create"
      }
      ui_control_times$selected <- idx
    })

    ## Create //
    observeEvent(input$action_time_create, {
      performAction_createTime(input_bundles = list(
        bundle_db_table_times = bundle_db_table_times
      ), uids = list(uid_issues = uid_issues))
      # ui_control_times$refresh <- ui_control_times$refresh + 1
      ui_control_times$refresh <- NULL
      ui_control_times$refresh <- TRUE
    })

    ## Update //
    observeEvent(input$action_time_update, {
      performAction_updateTime(input_bundles = list(
        bundle_db_table_times = bundle_db_table_times
      ), uids = list(uid_issues = uid_issues, uid_times = uid_times))
      # ui_control_times$refresh <- ui_control_times$refresh + 1
      ui_control_times$refresh <- NULL
      ui_control_times$refresh <- TRUE
      ui_control_times$case <- "create"
    })

    ## Cancel update //
    observeEvent(input$action_time_update_cancel, {
      ui_control_times$case <- "create"
    })

    ## Delete //
    observeEvent(input$action_time_delete, {
      performAction_deleteTime(input_bundles = list(
        bundle_db_table_times = bundle_db_table_times
      ), uids = list(uid_issues = uid_issues, uid_times = uid_times))
      # ui_control_times$refresh <- ui_control_times$refresh + 1
      ui_control_times$refresh <- NULL
      ui_control_times$refresh <- TRUE
      ui_control_times$case <- "create"
    })

    output$ui_times <- renderUI({
      if (ui_control_times$case == "hide") return()

      createDynamicUi_timeDetails(input, output, ui_control = ui_control_times)
    })

    output$ui_times_table <- renderUI({
      if (!ui_control_times$show_table) return()

      createDynamicUi_displayTimes(input, output,
        ui_control_ref = ui_control_issues)
    })

    ## Render //
    output$dt_times <- DT::renderDataTable({
      ## Refresh trigger //
      if(!ui_control_times$refresh) return()

      renderResults_dbTableTimes(
        uids = list(uid_issues = uid_issues),
        ui_control_ref = ui_control_issues
      )
    }, selection = "single", options = list(dom = "ltipr"))
  }
}

# Launch  ---------------------------------------------------------------

shinyApp(ui, server)
