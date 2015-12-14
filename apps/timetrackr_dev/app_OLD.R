##------------------------------------------------------------------------------
## Meta //
##------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(DT)
# library(readxl)
library(RNeo4j)
library(digest)

library(idata)
library(eventr)
# install.packages("rsconnect")
# library(rsconnect)

## App name //
app_name <- "EventModelling"

## App stage //
app_stage <- "v1.0"
app_stage <- "v1.1"
app_stage <- "v1.2"

app_stages <- c(
  "v1.0",
  "v1.1",
  "v1.2"
)

## App stages -----

validateAppStage <- function(
  stage,
  .stages
) {
  match.arg(stage, .stages)
}
app_stage <- validateAppStage(app_stage, .stages = app_stages)

# Stage v1.0 --------------------------------------------------------------

if (app_stage == "v1.0") {

  ##----------------------------------------------------------------------------
  ## Global //
  ##----------------------------------------------------------------------------

  library(idata)
  # setwd("apps/conditional_ui")
  dir_structure <- mapDirectory(dir = "data", include_root = FALSE)
  # dir_structure <- mapDirectory(dir = "data", drop_root = FALSE)
  print(dir_structure)

  GLOBALS <- dir_structure

  ns_values <- GLOBALS$namespaces
  ns_default <- names(ns_values)[1]

  ts_values <- GLOBALS$namespaces[[ns_default]]$timeseries
  ts_default <- names(ts_values)[1]

  project_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects
  project_default <- names(project_values)[1]

  variant_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects[[project_default]]$variants
  variant_default <- names(variant_values)[1]

  ##----------------------------------------------------------------------------
  ## UI //
  ##----------------------------------------------------------------------------

  ui <- dashboardPage(
    ## Header //
    dashboardHeader(title = app_name),


    ## Sidebar content //
    dashboardSidebar(
      sidebarMenu(
        selectInput("ns", "Select namespace",
          choices = names(ns_values),
          selected = ns_default
        ),
        menuItem("Variants meta", tabName = "variants", icon = icon("th"))
      )
    ),

    ## Body content
    dashboardBody(
      tabItems(
        # Option 1 //
        tabItem(
          tabName = "variants",
          fluidRow(
            ## Time series //
            box(
              selectInput("ts", "Time series",
                choices = names(ts_values),
                selected = ts_default
              ),
              width = 2
            ),

            ## Select IF project //
            box(
              selectInput("project", "Project",
                choices = names(project_values),
                selected = project_default
                # width = "300px"
              ),
              width = 4
            ),

            ## Select variant specification //
            box(
              selectInput("variant", "Model Variant",
                choices = names(variant_values),
                selected = variant_default
              ),
              width = 4
            ),

            box(actionButton("go", "Go"), status = "primary", width = 2,
              height = "100px")
          )
        )
      ),

      tabItem(
        tabName = "Path",
        h2("Path"),
        fluidRow(
          tabBox(
            width = 12,
            height = "500px",
            title = "Path",
            id = "path",
            tabPanel("Result", textOutput("path"))
          )
        )
      )
    )
  )

  ##----------------------------------------------------------------------------
  ## Server //
  ##----------------------------------------------------------------------------

  server <- function(input, output, session) {

    observe({
      # input$update
      input_ns <- input$ns
      # print(input_ns)
      updateSelectInput(session, "ts",
        choices = names(GLOBALS$namespaces[[input_ns]]$timeseries)
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # print(input_ts)
      updateSelectInput(session, "project",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects
        )
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # input_ts <- isolate(input$ts)
      input_project <- input$project
      #       print(input_project)
      updateSelectInput(session, "variant",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects[[input_project]]$variants
        )
      )
    })

    path <- eventReactive(input$go, {
      # paste0(input$ns, "/", input$ts, "/", input$project, "/", input$variant, "/")
      # print(capture.output(GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$projects[[input$project]]$variants[[input$variant]][[1]]))
      value <- GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$projects[[input$project]]$variants[[input$variant]]
      if (!length(value)) {
        value <- ""
      }

      paste(
        c(
          dir,
          "namespaces",
          input$ns,
          "timeseries",
          input$ts,
          "projects",
          input$project,
          "variants",
          input$variant,
          value
        ),
        collapse = "/"
      )
    })

    output$path <- renderText({
      path <- path()
      print(file.exists(path))
      path
    })

  }
}

# Stage v1.1 --------------------------------------------------------------

if (app_stage == "v1.1") {

  ##----------------------------------------------------------------------------
  ## Global //
  ##----------------------------------------------------------------------------

  library(idata)
  # setwd("apps/conditional_ui")
  dir_structure <- mapDirectory(dir = "data", include_root = FALSE)
  # dir_structure <- mapDirectory(dir = "data", drop_root = FALSE)
  print(dir_structure)

  GLOBALS <- dir_structure

  ns_values <- GLOBALS$namespaces
  ns_default <- names(ns_values)[1]

  ts_values <- GLOBALS$namespaces[[ns_default]]$timeseries
  ts_default <- names(ts_values)[1]

  project_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects
  project_default <- names(project_values)[1]

  variant_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects[[project_default]]$variants
  variant_default <- names(variant_values)[1]

  ## Neo4j //
  graph <- startGraph("http://localhost:7474/db/data")
  # clear(graph, input = FALSE)
  # browse(graph)
  #       tmp <- cypherToList(graph, "MATCH (n) RETURN n")
  #       summary(tmp[[1]])
  #       names(tmp[[1]]$n)
  #       class(tmp[[1]])

  getDbContent <- function(graph) {
    query <- "
      MATCH (ts_meta:TimeseriesMeta)
      RETURN ts_meta
    "
    content <- cypherToList(graph, query)
    content <- sapply(content, function(ii) {
      paste0(ii$ts_meta$name, " (", ii$ts_meta$id, ")")
    })
    if (!length(content)) {
      "database still empty"
    } else {
      content
    }
  }

  ##----------------------------------------------------------------------------
  ## UI //
  ##----------------------------------------------------------------------------

  ui <- dashboardPage(
    ## Header //
    dashboardHeader(title = app_name),


    ## Sidebar content //
    dashboardSidebar(
      sidebarMenu(
        menuItem("Available time series", tabName = "available_ts",
          icon = icon("database")),

        menuItem("Import time series", tabName = "import_ts",
          icon = icon("cloud-upload")),

        menuItem("Check", tabName = "check", icon = icon("file")),

        hr(),

        menuItem("Variants meta", tabName = "variants", icon = icon("th")),

        selectInput("ns", "Select namespace",
          choices = names(ns_values),
          selected = ns_default
        )
      )
    ),

    ## Body content
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "available_ts",
          h2("Available time series"),
          fluidRow(
            ## Time series //
            box(
              # fileInput("ts_file", "Select time series file")
              selectInput("ts_in_db", "Time series in database",
                choices = getDbContent(graph)
              ),
              actionButton("use", "Use",
                icon = icon("download-alt", lib = "glyphicon")
              ),
              # status = "primary",
              width = 12
            )
          ),
          fluidRow(
            box(
              tabPanel("Selected time series", dataTableOutput("selected_ts",
                height = "350px")),
              width = 12
            )
          )
        ),

        tabItem(
          tabName = "import_ts",
          h2("Import time series"),
          fluidRow(
            ## Time series //
            box(
              # fileInput("ts_file", "Select time series file")
              selectInput("import_file", "File",
                choices = list.files("data/import")
              ),
              actionButton("refresh", "Refresh",
                # icon = icon("refresh", lib = "glyphicon"),
                icon = icon("refresh")
              ),
              p(),
              actionButton("import", "Import",
                icon = icon("cloud-upload"))
            )
          ),
          h2("Imported data"),
          fluidRow(
            box(
              tabPanel("Imported data", dataTableOutput("imported_data",
                height = "350px")),
              width = 12
            )
          )
        ),

        tabItem(
          tabName = "check",
          h2("Check"),
          fluidRow(
            tabBox(
              width = 12,
              height = "500px",
              title = "Path",
              id = "path",
              tabPanel("Result", textOutput("check"))
            )
          )
        ),

        tabItem(
          tabName = "variants",
          fluidRow(
            ## Time series //
            box(
              selectInput("ts", "Time series",
                choices = names(ts_values),
                selected = ts_default
              ),
              width = 2
            ),

            ## Select IF project //
            box(
              selectInput("project", "Project",
                choices = names(project_values),
                selected = project_default
                # width = "300px"
              ),
              width = 4
            ),

            ## Select variant specification //
            box(
              selectInput("variant", "Model Variant",
                choices = names(variant_values),
                selected = variant_default
              ),
              width = 4
            ),

            box(
              actionButton("go", "Go"), status = "primary", width = 2,
              height = "100px")
          )
        )
      )
    )
  )

  ##----------------------------------------------------------------------------
  ## Server //
  ##----------------------------------------------------------------------------

  server <- function(input, output, session) {

    ##--------------------------------------------------------------------------
    ## Tabset import_ts //
    ##--------------------------------------------------------------------------

    observe({
      input$refresh
      updateSelectInput(session, "import_file",
        choices = list.files("data/import")
      )
    })

    import_file <- eventReactive(input$import, {
      file.path("data/import", input$import_file)
    })

    imported_data <- reactive({
      withProgress(
        message = 'Importing data',
        detail = "This may take a while...",
        {
          data <- loadData(path = import_file())
          incProgress(0.5)
          data <- formatData(data, extended = TRUE, with_ids = TRUE)
          incProgress(0.5)
          data <- makeNeo4jProof(data)
        }
      )
      data
    })

    ## Show imported data //
    output$imported_data <- renderDataTable({
      imported_data()
    }, options = list(
      scrollX = "100%",
      scrollY = "350px",
      scrollCollapse = TRUE
    ))

    ## Write to DB //
    observe({
      data <- imported_data()
      file <- import_file()

      meta_id <- digest(Sys.time())
      meta_name <- gsub("\\.csv$", "", basename(file))

      if (is.null(getConstraint(graph, "Timeseries"))) {
        addConstraint(graph, "Timeseries", "meta_id")
        addConstraint(graph, "Timeseries", "meta_name")
      }
      if (is.null(getConstraint(graph, "TimeseriesMeta"))) {
        addConstraint(graph, "TimeseriesMeta", "id")
        addConstraint(graph, "TimeseriesMeta", "name")
      }

      withProgress(
        message = 'Writing to database',
        detail = "This may take a while...",
        expr = {

          ts_meta <- getNodes(graph, sprintf("
            MATCH (ts_meta:TimeseriesMeta)
            WHERE ts_meta.name = '%s'
            RETURN ts_meta
          ", meta_name)
          )
          if (is.null(ts_meta)) {
            ts_meta <- getOrCreateNode(graph, "TimeseriesMeta",
              name = meta_name, id = meta_id)
            props <- c(
              meta_id = meta_id,
              meta_name = meta_name,
              as.list(data)
            )
            ts <- getOrCreateNode(graph, "Timeseries", props)
            incProgress(0.50)

            query <- sprintf("
              MATCH (ts_meta:TimeseriesMeta)-[h:HAS_TS]->(ts:Timeseries)
              WHERE ts_meta.meta_id = '%s' AND ts.meta_id = '%s'
              RETURN count(h) AS count
              ", props$meta_id, props$meta_id
            )
            count <- unlist(cypher(graph, query))

            if(count == 0) {
              createRel(ts_meta, "HAS_TS", ts)
            }
            incProgress(0.25)
          } else {
            setProgress(0.75)
          }
          #
          #           query <- "
          #             MATCH (ts_meta:TimeseriesMeta)
          #             RETURN ts_meta
          #           "
          #           content <- cypherToList(graph, query)
          #           content <- sapply(content, function(ii) {
          #             paste0(ii$ts_meta$name, " (", ii$ts_meta$id, ")")
          #           })
          content <- getDbContent(graph)
          incProgress(0.25)
          content
        }
      )

      print(content)

      updateSelectInput(session, "ts_in_db", choices = content)
    })

    ##--------------------------------------------------------------------------
    ## Tabset available_ts //
    ##--------------------------------------------------------------------------

    selected_ts <- eventReactive(input$use, {
      meta_id <- gsub(".*\\(|\\).*$", "", input$ts_in_db)
      # print(meta_id)
    })

    ## Show imported data //
    output$selected_ts <- renderDataTable({
      meta_id <- selected_ts()
      ts <- getNodes(graph, sprintf("
            MATCH (ts:Timeseries)
            WHERE ts.meta_id = '%s'
            RETURN ts
          ", meta_id)
      )
      #       print(summary(ts[[1]]))
      #       print(names(ts[[1]]))
      #       print(class(ts[[1]]))
      #       data.frame(a = letters)
      as.data.frame(unclass(ts[[1]]))
    }, options = list(
      scrollX = "100%",
      scrollY = "350px",
      scrollCollapse = TRUE
    ))

    ##--------------------------------------------------------------------------
    ## Tabset import_ts //
    ##--------------------------------------------------------------------------

    observe({
      # input$update
      input_ns <- input$ns
      # print(input_ns)
      updateSelectInput(session, "ts",
        choices = names(GLOBALS$namespaces[[input_ns]]$timeseries)
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # print(input_ts)
      updateSelectInput(session, "project",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects
        )
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # input_ts <- isolate(input$ts)
      input_project <- input$project
      #       print(input_project)
      updateSelectInput(session, "variant",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects[[input_project]]$variants
        )
      )
    })

    path <- eventReactive(input$go, {
      # paste0(input$ns, "/", input$ts, "/", input$project, "/", input$variant, "/")
      # print(capture.output(GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$projects[[input$project]]$variants[[input$variant]][[1]]))
      value <- GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$projects[[input$project]]$variants[[input$variant]]
      if (!length(value)) {
        value <- ""
      }

      paste(
        c(
          dir,
          "namespaces",
          input$ns,
          "timeseries",
          input$ts,
          "projects",
          input$project,
          "variants",
          input$variant,
          value
        ),
        collapse = "/"
      )
    })

    output$check <- renderText({
      print(selected_ts())
      print(import_file())
    })

  }
}

# Version v1.2 ------------------------------------------------------------

if (app_stage == "v1.2") {

## Global -----
  source("global.R")
  app <- App.EventStudio$new()

## UI ----------------------------------------------------------------------

  ui <- dashboardPage(
    ## Header //
    dashboardHeader(title = app_name),

    ## Sidebar content //
    dashboardSidebar(
      sidebarMenu(
        menuItem("Available time series", tabName = "available_ts",
          icon = icon("database")),

        menuItem("Import time series", tabName = "import_ts",
          icon = icon("cloud-upload")),

        menuItem("Check", tabName = "check", icon = icon("file"))
      )
    ),

    ## Body content
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "available_ts",
          h2("Available time series"),
          fluidRow(
            ## Time series //
            box(
              # fileInput("ts_file", "Select time series file")
              selectInput("ts_in_db", "Time series in database",
                choices = getDbContent(graph)
              ),
              actionButton("use", "Use",
                icon = icon("download-alt", lib = "glyphicon")
              ),
              # status = "primary",
              width = 12
            )
          ),
          fluidRow(
            box(
              tabPanel("Selected time series", dataTableOutput("selected_ts",
                height = "350px")),
              width = 12
            )
          )
        ),

        tabItem(
          tabName = "import_ts",
          h2("Import time series"),
          fluidRow(
            ## Time series //
            box(
              # fileInput("ts_file", "Select time series file")
              selectInput("import_file", "File",
                choices = list.files("data/import")
              ),
              actionButton("refresh", "Refresh",
                # icon = icon("refresh", lib = "glyphicon"),
                icon = icon("refresh")
              ),
              p(),
              actionButton("import", "Import",
                icon = icon("cloud-upload"))
            )
          ),
          h2("Imported data"),
          fluidRow(
            box(
              tabPanel("Imported data", dataTableOutput("imported_data",
                height = "350px")),
              width = 12
            )
          )
        ),

        tabItem(
          tabName = "check",
          h2("Check"),
          fluidRow(
            tabBox(
              width = 12,
              height = "500px",
              title = "Path",
              id = "path",
              tabPanel("Result", textOutput("check"))
            )
          )
        )
      )
    )
  )

## Server ------------------------------------------------------------------

  server <- function(input, output, session) {

    observe({
      input$refresh
      updateSelectInput(session, "import_file",
        choices = list.files("data/import")
      )
    })

#     import_file <- eventReactive(input$import, {
#       file.path("data/import", input$import_file)
#     })
    # import_file <- app$setImportFile(input, session)
    imported_data <- app$importData(input, session, import_file = import_file)

    ## Show imported data //
    output$imported_data <- renderDataTable({
      imported_data()
    }, options = list(
      scrollX = "100%",
      scrollY = "350px",
      scrollCollapse = TRUE
    ))

    ## Write to DB //
    observe({
      data <- imported_data()
      file <- import_file()

      meta_id <- digest(Sys.time())
      meta_name <- gsub("\\.csv$", "", basename(file))

      if (is.null(getConstraint(graph, "Timeseries"))) {
        addConstraint(graph, "Timeseries", "meta_id")
        addConstraint(graph, "Timeseries", "meta_name")
      }
      if (is.null(getConstraint(graph, "TimeseriesMeta"))) {
        addConstraint(graph, "TimeseriesMeta", "id")
        addConstraint(graph, "TimeseriesMeta", "name")
      }

      withProgress(
        message = 'Writing to database',
        detail = "This may take a while...",
        expr = {

          ts_meta <- getNodes(graph, sprintf("
            MATCH (ts_meta:TimeseriesMeta)
            WHERE ts_meta.name = '%s'
            RETURN ts_meta
            ", meta_name)
          )
          if (is.null(ts_meta)) {
            ts_meta <- getOrCreateNode(graph, "TimeseriesMeta",
              name = meta_name, id = meta_id)
            props <- c(
              meta_id = meta_id,
              meta_name = meta_name,
              as.list(data)
            )
            ts <- getOrCreateNode(graph, "Timeseries", props)
            incProgress(0.50)

            query <- sprintf("
              MATCH (ts_meta:TimeseriesMeta)-[h:HAS_TS]->(ts:Timeseries)
              WHERE ts_meta.meta_id = '%s' AND ts.meta_id = '%s'
              RETURN count(h) AS count
              ", props$meta_id, props$meta_id
            )
            count <- unlist(cypher(graph, query))

            if(count == 0) {
              createRel(ts_meta, "HAS_TS", ts)
            }
            incProgress(0.25)
          } else {
            setProgress(0.75)
          }
          #
          #           query <- "
          #             MATCH (ts_meta:TimeseriesMeta)
          #             RETURN ts_meta
          #           "
          #           content <- cypherToList(graph, query)
          #           content <- sapply(content, function(ii) {
          #             paste0(ii$ts_meta$name, " (", ii$ts_meta$id, ")")
          #           })
          content <- getDbContent(graph)
          incProgress(0.25)
          content
        }
      )

      print(content)

      updateSelectInput(session, "ts_in_db", choices = content)
    })

    ## Tabset available_ts //

    selected_ts <- eventReactive(input$use, {
      meta_id <- gsub(".*\\(|\\).*$", "", input$ts_in_db)
      # print(meta_id)
    })

    ## Show imported data //
    output$selected_ts <- renderDataTable({
      meta_id <- selected_ts()
      ts <- getNodes(graph, sprintf("
        MATCH (ts:Timeseries)
        WHERE ts.meta_id = '%s'
        RETURN ts
        ", meta_id)
      )
      #       print(summary(ts[[1]]))
      #       print(names(ts[[1]]))
      #       print(class(ts[[1]]))
      #       data.frame(a = letters)
      as.data.frame(unclass(ts[[1]]))
    }, options = list(
      scrollX = "100%",
      scrollY = "350px",
      scrollCollapse = TRUE
    ))

    output$check <- renderText({
      print(selected_ts())
      print(import_file())
    })

  }
}

##------------------------------------------------------------------------------
## Start app //
##------------------------------------------------------------------------------

shinyApp(ui, server)
