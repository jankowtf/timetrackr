# options(shiny.error = browser)

# Meta --------------------------------------------------------------------

app_name <- "Time tracking"

source("dependencies.R")
source("global.R")

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  ## Header //
  dashboardHeader(title = app_name),

  ## Sidebar content //
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
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
  dashboardBody(tabItems(
    tabItem(
      tabName = "tasks",
      fluidRow(
        # column(width = 7, div()),
        column(
          width = GLOBAL$ui$main$left$width,
          # verbatimTextOutput("selection_info")
          radioButtons("choice_issues_selectiontype", "Selection type (not working yet)",
            c("single", "multiple"), "single", inline = TRUE)
        ),
        column(
          width = GLOBAL$ui$main$right$width,
          actionButton("action_issue_create", "New task")
          # shinyBS::bsButton("action_issue_create", "New task", type = "toggle")
        )),
      p(),
      fluidRow(
        column(
          width = GLOBAL$ui$main$left$width,
          uiOutput("ui_times_table"),
          box(
            title = "Tasks",
            DT::dataTableOutput("dt_issues"),
            width = NULL,
            if (adjust_box_height) {
              height = GLOBAL$ui$main$issues$left$height
            }
          )
        ),
        column(
          width = GLOBAL$ui$main$right$width,
          uiOutput('ui_times'),
          uiOutput("ui_issues"))
      )
    ),
    tabItem(
      tabName = "projects",
      fluidRow(
        # column(width = 7, div()),
        column(
          width = GLOBAL$ui$main$left$width,
          radioButtons("choice_projects_selectiontype", "Selection type (not working yet)",
            c("single", "multiple"), "single", inline = TRUE)
        ),
        column(
          width = GLOBAL$ui$main$right$width,
          actionButton("action_project_create", "New project"),
          # shinyBS::bsButton("action_project_create", "New project", type = "toggle"),
          p(),
          actionLink("goto_issues_from_projects", "Go back to Tasks")
        )),
      p(),
      fluidRow(
        column(
          width = GLOBAL$ui$main$left$width,
          box(
            title = "Projects",
            DT::dataTableOutput("dt_projects"),
            width = NULL
            # height = GLOBAL$ui$main$projects$left$height
          )
        ),
        column(
          width = GLOBAL$ui$main$right$width,
          uiOutput("ui_projects"))
      )
    ),
    tabItem(tabName = "admin_access",
      fluidRow(
        box(title = "Admin access (work in progress)",
          p(),
          strong("This will be password protected or something like that"),
          hr(),
          actionButton("action_admin_reset_db", "Reset database"))
      )),
    tabItem(tabName = "info",
      fluidRow(box(
        title = "Time format",
        div(
          p("Possible formats for time inputs (examples):"),
          tags$li("1.5: 1.5 hours"),
          tags$li("1.5d: 12 hours (a day has 8 hours)"),
          tags$li("1.5h: 1.5 hours"),
          tags$li("45m: 0.75 hours"),
          tags$li("1d 2h 30m: 10.5 hours"),
          p(),
          strong("NOTE:"),
          p(),
          tags$li(strong(
            "Everything is transformed to hours before DB commit"
          )),
          p(),
          actionLink("goto_issues", "Go back to Tasks")
        ),
        width = 12
      )))
  ))
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  app$prepare(
    public_fields_compact = GLOBAL$db$tables$issues$public_fields_compact,
    public_fields_details = GLOBAL$db$tables$issues$public_fields_details,
    private_fields = GLOBAL$db$tables$issues$private_fields,
    times_public_fields_compact = GLOBAL$db$tables$times$public_fields_compact,
    times_public_fields_details = GLOBAL$db$tables$times$public_fields_details,
    times_private_fields = GLOBAL$db$tables$times$private_fields,
    projects_public_fields_compact = GLOBAL$db$tables$projects$public_fields_compact,
    projects_public_fields_details = GLOBAL$db$tables$projects$public_fields_details,
    projects_private_fields = GLOBAL$db$tables$projects$private_fields
  )

  observeEvent(input$action_admin_reset_db, {
    unlink("data/db", recursive = TRUE, force = TRUE)
    app$prepare(
      public_fields_compact = GLOBAL$db$tables$issues$public_fields_compact,
      public_fields_details = GLOBAL$db$tables$issues$public_fields_details,
      private_fields = GLOBAL$db$tables$issues$private_fields,
      times_public_fields_compact = GLOBAL$db$tables$times$public_fields_compact,
      times_public_fields_details = GLOBAL$db$tables$times$public_fields_details,
      times_private_fields = GLOBAL$db$tables$times$private_fields,
      projects_public_fields_compact = GLOBAL$db$tables$projects$public_fields_compact,
      projects_public_fields_details = GLOBAL$db$tables$projects$public_fields_details,
      projects_private_fields = GLOBAL$db$tables$projects$private_fields
    )
    uicontrol_issues$refresh <- 0
  })

  ##################
  ## Bundle input ##
  ##################

  bundle_db_table_issues <- reactive({
    bundleInputData_dbTableIssues(input)
  })
  bundle_db_table_times <- reactive({
    bundleInputData_dbTableTimes(input)
  })
  bundle_db_table_projects <- reactive({
    bundleInputData_dbTableProjects(input)
  })

  ## UIDs //
  uid_issues <- reactive({
    getUids_dbTableIssues(input = input)
  })
  uid_times <- reactive({
    getUids_dbTableTimes(input = input)
  })
  uid_projects <- reactive({
    getUids_dbTableProjects(input = input)
  })

  ########################
  ## Dynamic UI: issues ##
  ########################

  uicontrol_issues <- reactiveValues(
    case = c("hide", "create", "update")[1],
    selected = NULL,
    refresh = 0,
    initial = TRUE,
    clear_selected_on_create = FALSE
  )
  observe(if (uicontrol_issues$initial) {
    uicontrol_issues$refresh <- 1
    uicontrol_issues$initial <- FALSE
  })
  observe(if (!is.null(input$dt_issues_rows_selected)) {
    uicontrol_issues$case <- "update"
  })
  observeEvent(input$action_issue_create, {
    use_toggle_button <- FALSE
    if (!use_toggle_button) {
      ## Cases with action button //
      ## Own:
      uicontrol_issues$case <- "create"

      ## Dependents:
      uicontrol_times$case <- "hide"
      uicontrol_times$show_table <- FALSE
    } else {
      ## Cases with toggle button //
      if (input$action_issue_create) {
        uicontrol_issues$case <- "create"
      } else {
        uicontrol_issues$case <- "hide"
      }
      ## Dependents:
      uicontrol_times$case <- "hide"
      uicontrol_times$show_table <- FALSE
    }

    ## Reset selection //
    uicontrol_issues$clear_selected_on_create <- NULL
    uicontrol_issues$clear_selected_on_create <- TRUE
    ## --> necessary in order to re-render proxy stuff
    ## Otherwise, a supposed "create" will actually result in an "update"
    ## which is not desired.
    ## --> GH issue #3
  })

  observe({
    idx <- input$dt_issues_rows_selected
    # if (length(idx) && length(uicontrol_issues$selected)) {
    # if (length(idx)) {
      uicontrol_issues$selected <- idx
    # }
  })
  observe({
    idx <- uicontrol_issues$selected
    if (!is.null(idx)) {
      uicontrol_issues$case <- "update"
    } else {
      if (!uicontrol_issues$clear_selected_on_create) {
        uicontrol_issues$case <- "hide"
      } else {
        uicontrol_issues$case <- "create"
      }
    }
  })
  observeEvent(input$choice_issues_selectiontype, {
    uicontrol_issues$refresh <- NULL
    uicontrol_issues$refresh <- 1
  })

  ## Reset issue-related inputs //
  observe(if (uicontrol_issues$case == "create") {
    act_resetIssueInput(session)
  })
  observeEvent(input$action_issue_create_2, {
    # uicontrol_issues$clear_selected_on_create <- NULL
    uicontrol_issues$clear_selected_on_create <- FALSE
  })

  ## Create issue //
  observeEvent(input$action_issue_create_2, {
    act_createIssue(
      input_bundles = list(bundle_db_table_issues = bundle_db_table_issues),
      uids = list(uid_issues = uid_issues)
    )
    uicontrol_issues$refresh <- uicontrol_issues$refresh + 1

    uicontrol_issues$case <- "hide"
    uicontrol_issues$selected <- NULL
  })

  # Cancel create issue //
  observeEvent(input$action_issue_create_cancel, {
    uicontrol_issues$case <- "hide"
  })

  # Update issue //
  observeEvent(input$action_issue_update, {
    act_updateIssue(
      input_bundles = list(bundle_db_table_issues = bundle_db_table_issues),
      uids = list(uid_issues = uid_issues)
    )
    uicontrol_issues$refresh <- uicontrol_issues$refresh + 1
    uicontrol_issues$case <- "hide"
  })

  #   # Cancel update issue //
  #   observeEvent(input$action_issue_update_cancel, {
  #     uicontrol_issues$case <- "hide"
  #   })

  # Delete issue //
  observeEvent(input$action_issue_delete, {
    act_deleteIssue(
      input_bundles = list(bundle_db_table_issues = bundle_db_table_issues),
      uids = list(uid_issues = uid_issues)
    )
    uicontrol_issues$refresh <- uicontrol_issues$refresh + 1
    uicontrol_times$refresh <- NULL
    uicontrol_times$refresh <- TRUE
    uicontrol_issues$case <- "hide"
  })

  ## Conditional UI: issue details //
  output$ui_issues <- renderUI({
    if (uicontrol_issues$case == "hide")
      return()

    generateUi_issueDetails(input, output,
      uicontrol = uicontrol_issues)
  })

  ## Render //
  output$dt_issues <- DT::renderDataTable({
    ## Refresh trigger //
    uicontrol_issues$refresh

    renderResults_dbTableIssues()
  }, filter = "top",
    width = "50%", class = "cell-border stripe",
    # selection = "single",
    selection = input$choice_issues_selectiontype,
    options = list(
      dom = "ltipr",
      autoWidth = TRUE,
      scrollX = TRUE,
      # scrollY = "500px",
      scrollY = sprintf("%spx", as.numeric(GLOBAL$ui$main$issues$left$height) * 0.67),
      scrollCollapse = TRUE,
      columnDefs = list(list(width = '7.5%', targets = "_all")),
      lengthMenu = list(
        c(5, 10, 15, 20, -1),
        c(5, 10, 15, 20, "All")
      ),
      iDisplayLength = 10
    ))

  ## Proxy //
  proxy_dt_issues <- DT::dataTableProxy("dt_issues")
  observe({
    uicontrol_issues$refresh
    DT::selectRows(proxy_dt_issues, as.numeric(uicontrol_issues$selected))
  })
  observe({
    uicontrol_issues$clear_selected_on_create
    DT::selectRows(proxy_dt_issues, NULL)
  })

  #######################
  ## Dynamic UI: times ##
  #######################

  uicontrol_times <- reactiveValues(
    case = c("hide", "create", "update")[1],
    show_table = FALSE,
    selected = NULL,
    refresh = FALSE
  )

  ## Selected rows issues table //
  observe({
    # idx <- input$dt_issues_rows_selected
    idx <- uicontrol_issues$selected
    if (!is.null(idx)) {
      # uicontrol_times$refresh <- uicontrol_times$refresh + 1
      uicontrol_times$refresh <- NULL
      uicontrol_times$refresh <- TRUE
      ## TODO 2015-12-16: find best-practice for refresh controls
      ## Simply ensuring that `uicontrol_times$refresh` is set to TRUE/FALSE
      ## doesn't do trick as only CHANGES in values will trigger re-evaluations
      ## of dependent objects (i.e. `output$ui_times_table` and
      ## `output$dt_times`).
      ## That's why I thought a simple "counting up" would suffice, but then
      ## I run into infinite recursions somehow:
      ##     uicontrol_times$refresh <- uicontrol_times$refresh + 1
      #              print(uicontrol_times$refresh)
      #              Sys.sleep(3) # to have a chance to escape infinite recursion
      ## Current solution makes sure `uicontrol_times$refresh` is set to
      ## `NULL` before assigning any other value.
      ## That does the trick, but seems unnecessary

      uicontrol_times$case <- "create"
      uicontrol_times$show_table <- TRUE
    } else {
      uicontrol_times$refresh <- NULL
      uicontrol_times$refresh <- FALSE

      uicontrol_times$case <- "hide"
      uicontrol_times$show_table <- FALSE
    }
  })

  ## Selected rows times table //
  observe({
    idx <- input$dt_times_rows_selected
    if (!is.null(idx)) {
      uicontrol_times$case <- "update"
    } else {
      if (!is.null(uicontrol_issues$selected)) {
        uicontrol_times$case <- "create"
      }
    }
    uicontrol_times$selected <- idx
  })
  observeEvent(input$choice_issues_selectiontype, {
    uicontrol_times$refresh <- NULL
    uicontrol_times$refresh <- TRUE
  })

  ## Reset times-related inputs //
  observe({
    uicontrol_issues$selected
    # if (uicontrol_times$case == "create") {
    act_resetTimeInput(session)
    # }
  })

  ## Create //
  observeEvent(input$action_time_create, {
    act_createTime(
      input_bundles = list(bundle_db_table_times = bundle_db_table_times),
      uids = list(uid_issues = uid_issues)
    )
    uicontrol_times$refresh <- NULL
    uicontrol_times$refresh <- TRUE

    ## Refresh parent //
    uicontrol_issues$refresh <- NULL
    uicontrol_issues$refresh <- TRUE
  })

  ## Update //
  observeEvent(input$action_time_update, {
    act_updateTime(
      input_bundles = list(bundle_db_table_times = bundle_db_table_times),
      uids = list(uid_issues = uid_issues, uid_times = uid_times)
    )
    # uicontrol_times$refresh <- uicontrol_times$refresh + 1
    uicontrol_times$refresh <- NULL
    uicontrol_times$refresh <- TRUE

    ## Refresh parent //
    uicontrol_issues$refresh <- NULL
    uicontrol_issues$refresh <- TRUE

    uicontrol_times$case <- "create"
  })

  ## Cancel update //
  observeEvent(input$action_time_update_cancel, {
    uicontrol_times$case <- "create"
  })

  ## Delete //
  observeEvent(input$action_time_delete, {
    act_deleteTime(
      input_bundles = list(bundle_db_table_times = bundle_db_table_times),
      uids = list(uid_issues = uid_issues, uid_times = uid_times)
    )
    # uicontrol_times$refresh <- uicontrol_times$refresh + 1
    uicontrol_times$refresh <- NULL
    uicontrol_times$refresh <- TRUE

    ## Refresh parent //
    uicontrol_issues$refresh <- NULL
    uicontrol_issues$refresh <- TRUE

    uicontrol_times$case <- "create"
  })

  ## Conditional UI: times details //
  output$ui_times <- renderUI({
    if (uicontrol_times$case == "hide")
      return()

    generateUi_timeDetails(input, output, uicontrol = uicontrol_times)
  })

  output$ui_times_table <- renderUI({
    if (!uicontrol_times$show_table)
      return()

    generateUi_displayTimes(input, output,
      uicontrol_ref = uicontrol_issues)
  })

  ## Render //
  output$dt_times <- DT::renderDataTable({
    ## Refresh trigger //
    if (!uicontrol_times$refresh)
      return()

    renderResults_dbTableTimes(uids = list(uid_issues = uid_issues),
      uicontrol_ref = uicontrol_issues)
  },
    # selection = "single",
    selection = input$choice_issues_selectiontype,
    options = list(
    dom = "ltipr",
    scrollY = sprintf("%spx", as.numeric(GLOBAL$ui$main$times$left$height) * 0.55),
    scrollCollapse = TRUE,
    lengthMenu = list(
      c(5, 10, 15, 20, -1),
      c(5, 10, 15, 20, "All")
    ),
    iDisplayLength = 10
  ))

  proxy_dt_times <- DT::dataTableProxy("dt_times")
  observe({
    uicontrol_times$refresh
    DT::selectRows(proxy_dt_times, as.numeric(uicontrol_times$selected))
  })

  ##########################
  ## Dynamic UI: projects ##
  ##########################

  uicontrol_projects <- reactiveValues(
    case = c("hide", "create", "update")[1],
    selected = NULL,
    refresh = 0,
    clear_selected_on_create = FALSE
  )
  observe(if (!is.null(input$dt_projects_rows_selected)) {
    uicontrol_projects$case <- "update"
  })
  observeEvent(input$action_project_create, {
    use_toggle_button <- FALSE
    if (!use_toggle_button) {
      ## Cases with action button //
      ## Own:
      uicontrol_projects$case <- "create"

      ## Dependents:
      uicontrol_times$case <- "hide"
      uicontrol_times$show_table <- FALSE
    } else {
      ## Cases with toggle button //
      if (input$action_project_create) {
        uicontrol_projects$case <- "create"
      } else {
        uicontrol_projects$case <- "hide"
      }
      ## Dependents:
      uicontrol_times$case <- "hide"
      uicontrol_times$show_table <- FALSE
    }

    ## Reset selection //
    uicontrol_projects$clear_selected_on_create <- NULL
    uicontrol_projects$clear_selected_on_create <- TRUE
  })
  observe({
    idx <- input$dt_projects_rows_selected
    uicontrol_projects$selected <- idx
  })
  observe({
    idx <- uicontrol_projects$selected
    if (!is.null(idx)) {
      uicontrol_projects$case <- "update"
    } else {
      if (!uicontrol_projects$clear_selected_on_create) {
        uicontrol_projects$case <- "hide"
      } else {
        uicontrol_projects$case <- "create"
      }
    }
  })
  observeEvent(input$choice_projects_selectiontype, {
    uicontrol_projects$refresh <- NULL
    uicontrol_projects$refresh <- TRUE
  })

  ## Reset project-related inputs //
  observe(if (uicontrol_projects$case == "create") {
    act_resetProjectInput(session)
  })
  observeEvent(input$action_project_create_2, {
    # uicontrol_projects$clear_selected_on_create <- NULL
    uicontrol_projects$clear_selected_on_create <- FALSE
  })

  ## Create project //
  observeEvent(input$action_project_create_2, {
    act_createProject(
      input_bundles = list(bundle_db_table_projects = bundle_db_table_projects),
      uids = list(uid_projects = uid_projects)
    )
    uicontrol_projects$refresh <- uicontrol_projects$refresh + 1

    uicontrol_projects$case <- "hide"
    uicontrol_projects$selected <- NULL
  })

  # Cancel create project //
  observeEvent(input$action_project_create_cancel, {
    uicontrol_projects$case <- "hide"
  })

  # Update project //
  observeEvent(input$action_project_update, {
    act_updateProject(
      input_bundles = list(bundle_db_table_projects = bundle_db_table_projects),
      uids = list(uid_projects = uid_projects)
    )
    uicontrol_projects$refresh <- uicontrol_projects$refresh + 1
    uicontrol_projects$case <- "hide"
  })

  # Delete project //
  observeEvent(input$action_project_delete, {
    act_deleteProject(
      input_bundles = list(bundle_db_table_projects = bundle_db_table_projects),
      uids = list(uid_projects = uid_projects)
    )
    uicontrol_projects$refresh <- uicontrol_projects$refresh + 1
    uicontrol_projects$case <- "hide"
  })

  ## Conditional UI: project details //
  output$ui_projects <- renderUI({
    if (uicontrol_projects$case == "hide")
      return()

    generateUi_projectDetails(input, output,
      uicontrol = uicontrol_projects)
  })

  ## Render //
  output$dt_projects <- DT::renderDataTable({
    ## Refresh trigger //
    uicontrol_projects$refresh

    renderResults_dbTableProjects()
  }, filter = "top",
    width = "50%", class = "cell-border stripe",
    # selection = "single",
    # selection = "multiple",
    selection = input$choice_projects_selectiontype,
    options = list(
      dom = "ltipr",
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = sprintf("%spx", as.numeric(GLOBAL$ui$main$projects$left$height) * 0.55),
      scrollCollapse = TRUE,
      # columnDefs = list(list(width = '25%', targets = "_all")),
      lengthMenu = list(
        c(5, 10, 15, 20, -1),
        c(5, 10, 15, 20, "All")
      ),
      iDisplayLength = 10
    ))

  ## Proxy //
  proxy_dt_projects <- DT::dataTableProxy("dt_projects")
  observe({
    uicontrol_projects$refresh
    DT::selectRows(proxy_dt_projects, as.numeric(uicontrol_projects$selected))
  })
  observe({
    uicontrol_projects$clear_selected_on_create
    DT::selectRows(proxy_dt_projects, NULL)
  })

  ##################
  ## Action links ##
  ##################

  observeEvent(input$goto_info_from_issues, {
    newvalue <- "info"
    updateTabItems(session, "tabs", newvalue)
  })
  observeEvent(input$goto_info_from_times, {
    newvalue <- "info"
    updateTabItems(session, "tabs", newvalue)
  })

  observeEvent(input$goto_issues, {
    newvalue <- "tasks"
    updateTabItems(session, "tabs", newvalue)
  })
  observeEvent(input$goto_issues_from_projects, {
    newvalue <- "tasks"
    updateTabItems(session, "tabs", newvalue)
  })

  observeEvent(input$goto_projects, {
    newvalue <- "projects"
    updateTabItems(session, "tabs", newvalue)
  })

  output$selection_info <- renderPrint({
    c(
      paste0("Issue: ", uicontrol_issues$selected),
      paste0("Time: ", uicontrol_times$selected)
    )
  })

}

# Launch  ---------------------------------------------------------------

shinyApp(ui, server)
