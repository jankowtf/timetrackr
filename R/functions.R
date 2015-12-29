
# Prepare -----------------------------------------------------------------

#' @title
#' Prepare database
#'
#' @description
#' Prepare database.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-prepareDatabase.R
#' @export
prepareDatabase <- function(
  sqlite_path,
  public_fields_compact,
  public_fields_details,
  private_fields,
  times_public_fields_compact,
  times_public_fields_details,
  times_private_fields,
  ...
) {
  if (!file.exists(sqlite_path)) {
    dir.create(dirname(sqlite_path), recursive = TRUE, showWarnings = FALSE)
    file.create(sqlite_path)
  }
  db <- dbConnect(SQLite(), sqlite_path)

  #   if (!dbExistsTable(db, "responses")) {
  #     query <- sprintf(
  #       "CREATE TABLE responses(name TEXT, used_shiny TEXT, r_num_years TEXT,
  #       _uid TEXT)"
  #     )
  #     # Submit the update query and disconnect
  #     dbGetQuery(db, query)
  #   }

  ## Main //
  name <- GLOBAL$db$tables$issues$tablename
  if (!dbExistsTable(db, name)) {
    fields <- c(public_fields_compact, public_fields_details, private_fields)
    value <- sapply(fields, function(ii) {
      paste(ii$name, ii$datatype)
    })
    query <- sprintf(
      "CREATE TABLE %s(%s)",
      name,
      paste(value, collapse = ", ")
    )

    # Submit the update query and disconnect
    dbGetQuery(db, query)
  }

  ## Times //
  name <- GLOBAL$db$tables$times$tablename
  if (!dbExistsTable(db, name)) {
    fields <- c(
      times_public_fields_compact,
      times_public_fields_details,
      times_private_fields
    )
    # print(fields)
    value <- sapply(fields, function(ii) {
      paste(ii$name, ii$datatype)
    })
    query <- sprintf(
      "CREATE TABLE %s(%s)",
      name,
      paste(value, collapse = ", ")
    )

    # Submit the update query and disconnect
    dbGetQuery(db, query)
  }

  dbDisconnect(db)
}

# Load --------------------------------------------------------------------

#' @title
#' Load from database
#'
#' @description
#' Load from database.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-loadData.R
#' @export
loadData <- function(table, uid = character(), refuid = character()) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlite_path)
  # Construct the fetching query

  uid <- if (length(uid)) {
    sprintf("_uid = '%s'", uid)
  }
  refuid <- if (length(refuid)) {
    sprintf("_refuid = '%s'", refuid)
  }
  where_expr <- paste(uid, refuid, collapse = " AND ")

  if (where_expr == "") {
    query <- sprintf("SELECT * FROM %s", table)
  } else {
    query <- sprintf(
      "SELECT * FROM %s WHERE %s",
      table,
      where_expr
    )
  }
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

# Prepare for display -----------------------------------------------------

#' @title
#' Prepare DB table for display
#'
#' @description
#' Prepare DB table for display.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-prepareForDisplay.R
#' @export
prepareForDisplay <- function(input, fields) {
  #   message("input:")
  #   print(input)
  #   message("fields:")
  #   print(fields)

  tmp <- input[, sapply(fields, "[[", "name")]
  names(tmp) <- sapply(fields, "[[", "nicename")
  tmp
}

# Handle time input -------------------------------------------------------

#' @title
#' Handle time input
#'
#' @description
#' Transforms time input in a way that results in a numeric corresponding to
#' hours.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-handleTime.R
#' @export
handleTime <- function(
  input,
  globals = list(hours_per_workday = 8)
) {
  if (is.null(input)) {
    0
  } else {
    input <- as.character(input)
    ## Ensure character format

    spl <- unlist(strsplit(input, split = " "))
    if (!length(spl)) {
      0
    } else {
      idx_d <- grep("\\d+d", spl)
      idx_h <- grep("\\d+h", spl)
      idx_m <- grep("\\d+m", spl)
      if (!length(idx_d) && !length(idx_h) && !length(idx_m)) {
        ## Default: hours //
      } else {
        spl[idx_d] <- if (length(idx_d)) {
          this <- spl[idx_d]
          as.numeric(gsub("d", "", this)) * globals$hours_per_workday
        }
        spl[idx_h] <- if (length(idx_h)) {
          this <- spl[idx_h]
          as.numeric(gsub("h", "", this))
        }
        spl[idx_m] <-if (length(idx_m)) {
          this <- spl[idx_m]
          as.numeric(gsub("m", "", this)) / 60
        }
      }
      sum(as.numeric(spl))
    }
  }
}

# Save --------------------------------------------------------------------

#' @title
#' Save to database
#'
#' @description
#' Writes stuff to the database in the appropriate format/way.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-saveData.R
#' @export
saveData <- function(data,
  table,
  uid = character(),
  globals = list(hours_per_workday = 8)
) {
  ## Prepare data //
  data <- as.list(data)

  data$issue_time_estimated <- handleTime(data$issue_time_estimated,
    globals = globals)
  data$issue_time_spent <- handleTime(data$issue_time_spent, globals = globals)
  data$issue_time_until_done <- data$issue_time_estimated - data$issue_time_spent

  ## Connect to the database //
  db <- dbConnect(SQLite(), sqlite_path)
  now <- Sys.time()

  data$issue_date <- as.character(data$issue_date)
  data$issue_week = format(as.Date(data$issue_date), "%V")

  values <- c(
    data
  )

  ## Query //
  if (!length(uid)) {
    ## --> create
    ## Ensure //
    values$issue_time_spent <- 0
    values$time_until_done <- values$time_estimated
    ## TODO: this is sort of a bug, shouldn't be necessary

    values <- c(
      values,
      list("_uid" = digest(now)),
      list("_time_created" = as.character(now))
    )
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table,
      paste(names(values), collapse = ", "),
      paste(values, collapse = "', '")
    )
  } else {
    ## --> update
    values <- c(
      values,
      list("_time_modified" = as.character(now))
    )
    query <- sprintf(
      "UPDATE %s
      SET %s
      WHERE _uid = %s",
      table,
      paste(paste(names(values), paste0("'", values, "'"), sep = " = "),
        collapse = ", "),
      paste0("'", uid, "'")
    )
  }

  ## Submit query and disconnect //
  dbGetQuery(db, query)
  dbDisconnect(db)
}

# Log time ----------------------------------------------------------------

#' @title
#' Log time
#'
#' @description
#' log time.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-logTime.R
#' @export
logTime <- function(
  db,
  table,
  uid = character(),
  values = list()
) {
  db <- sqlite_path
  table_times <- GLOBAL$db$tables$times$tablename
  ## TODO: refactor global dependency

  ## Query //
  if (length(uid)) {
    now <- Sys.time()
    date <- as.Date(values$issue_time_logged_date)

    record <- loadData(table = table, uid = uid)
    issue_time_spent <- record$issue_time_spent
    issue_time_spent_new <- handleTime(values$issue_time_logged)
    issue_time_spent <- issue_time_spent + issue_time_spent_new

    values_this <- list(issue_time_spent = issue_time_spent)

    ## Connect to the database //
    db <- dbConnect(SQLite(), db)

    ## Table: times //
    query <- sprintf(
      "UPDATE %s
      SET %s
      WHERE _uid = '%s'",
      table,
      paste(paste(names(values_this),
        paste0("'", values_this, "'"), sep = " = "), collapse = ", "),
      uid
    )
    ## Submit query and disconnect //
    dbGetQuery(db, query)

    ## Table: times //
    values_this <- list(
      # date = as.character(Sys.Date()),
      # date = format(now, "%F"),
      issue_time_logged_date = as.character(date),
      issue_time_logged_week = format(date, "%V"),
      issue_time_logged = issue_time_spent_new,
      issue_time_logged_description = as.character(values$issue_time_logged_description),
      "_uid" = digest(now),
      "_refuid" = uid,
      "_time_created" = as.character(now)
    )

    # print(values_this)
    # print(values)

    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table_times,
      paste(names(values_this), collapse = ", "),
      paste(values_this, collapse = "', '")
    )
    ## Submit query and disconnect //
    dbGetQuery(db, query)

    dbDisconnect(db)
  }

  calculateTimeUntilComplete(table = table, uid = uid)
}

# Update specific ---------------------------------------------------------

#' @title
#' Update specific
#'
#' @description
#' Update specific.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-updateSpecific.R
#' @export
updateSpecific <- function(
  table,
  uid = character(),
  values,
  refuid = character()
) {
  db <- sqlite_path
  ## TODO: refactor: remove global dependency

# print(values)

  ## Query //
  if (length(uid)) {
    ## Connect to the database //
    db <- dbConnect(SQLite(), db)

    if (!length(refuid)) {
      ## --> update this table only

      query <- sprintf(
        "UPDATE %s
        SET %s
        WHERE _uid = '%s'",
        table,
        paste(paste(names(values), paste0("'", values, "'"), sep = " = "),
          collapse = ", "),
        uid
      )
    } else {
      ## --> update dependent table(s)

      query <- sprintf(
        "UPDATE %s
        SET %s
        WHERE _uid = '%s' AND _refuid = '%s'",
        table,
        paste(paste(names(values), paste0("'", values, "'"), sep = " = "),
          collapse = ", "),
        uid,
        refuid
      )
    }
    ## Submit query and disconnect //
    dbGetQuery(db, query)
    dbDisconnect(db)
  }

  if (table == GLOBAL$db$tables$issues$tablename) {
    calculateTimeUntilComplete(table = table, uid = uid)
  }
  if (table == GLOBAL$db$tables$times$tablename) {
    calculateWeek(table = table, uid = uid)
  }
  ## TODO: refactor: should not be dependent on specific table names
}

# Calculate ---------------------------------------------------------------

#' @title
#' Calculate time difference
#'
#' @description
#' Calculate time difference.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-calculateTimeUntilComplete.R
#' @export
calculateTimeUntilComplete <- function(
  table,
  uid = character()
) {
  record <- loadData(table = table, uid = uid)

  issue_time_estimated <- record$issue_time_estimated
  issue_time_spent <- record$issue_time_spent
  issue_time_until_done <- issue_time_estimated - issue_time_spent

  values <- list(issue_time_until_done = issue_time_until_done)

  ## Connect to the database //
  db <- dbConnect(SQLite(), sqlite_path)

  ## Query //
  if (length(uid)) {
    query <- sprintf(
      "UPDATE %s
      SET %s
      WHERE _uid = %s",
      table,
      paste(paste(names(values), paste0("'", values, "'"), sep = " = "),
        collapse = ", "),
      paste0("'", uid, "'")
    )
    ## Submit query and disconnect //
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
}

#' @title
#' Calculate calendar week
#'
#' @description
#' Calculate calendar week.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-calculateWeek.R
#' @export
calculateWeek <- function(
  table,
  uid = character()
) {
  record <- loadData(table = table, uid = uid)
  date <- as.Date(record$issue_time_logged_date)
  values <- list(issue_time_logged_week = format(as.Date(date), "%V"))

  ## Connect to the database //
  db <- dbConnect(SQLite(), sqlite_path)

  ## Query //
  if (length(uid)) {
    query <- sprintf(
      "UPDATE %s
      SET %s
      WHERE _uid = '%s'",
      table,
      paste(paste(names(values), paste0("'", values, "'"), sep = " = "),
        collapse = ", "),
      uid
    )
    ## Submit query and disconnect //
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
}

# Delete ------------------------------------------------------------------

#' @title
#' Delete database entry
#'
#' @description
#' Delete database entry.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-deleteData.R
#' @export
deleteData <- function(
  table,
  uid = character(),
  dependent = character()
) {
  if (length(uid)) {
    db <- dbConnect(SQLite(), sqlite_path)
    query <- sprintf(
      "DELETE FROM %s WHERE %s",
      table,
      paste0("_uid = '", uid, "'")
    )
    dbGetQuery(db, query)
    dbDisconnect(db)

    if (length(dependent)) {
      sapply(dependent, deleteDependent, refuid = uid)
    }
  }
}

#' @title
#' Delete dependent
#'
#' @description
#' Delete dependent.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-deleteDependent.R
#' @export
deleteDependent <- function(
  table,
  refuid = character()
) {
  if (length(refuid)) {
    db <- dbConnect(SQLite(), sqlite_path)
    query <- sprintf(
      "DELETE FROM %s WHERE %s",
      table,
      sprintf("_refuid = '%s'", refuid)
    )
    dbGetQuery(db, query)
    dbDisconnect(db)
    # message(sprintf("Deleted dependent records from '%s'", table))
  }
}

# Auxiliary ---------------------------------------------------------------

#' @title
#' Valid issue statuses
#'
#' @description
#' Valid issue statuses.
#'
#' @details
#' TODO
#'
#' @template authors
#' @template references
#' @example inst/examples/example-VALIDS_issueStatus.R
#' @export
VALIDS_issueStatus <- function(
) {
  c(todo = "to do", inprogress = "in progress", done = "done")
}
