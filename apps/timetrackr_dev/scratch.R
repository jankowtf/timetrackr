library(RNeo4j)

graph <- startGraph("http://localhost:7474/db/data")

## Only needed once //
if (FALSE) {
  clear(graph, input = FALSE)

  ## Constraints //
  addConstraint(graph, "Namespace", "id")
  addConstraint(graph, "Timeseries", "id")
  addConstraint(graph, "Project", "id")
  addConstraint(graph, "Variant", "id")

  ## Nodes //
  createDb <- function(x) {
    ns <- getOrCreateNode(graph, "Namespace", id = x$ns_id)
    ts <- getOrCreateNode(graph, "Timeseries", id = x$ts_id)
    project <- getOrCreateNode(graph, "Project", id = x$project_id)
    variant <- getOrCreateNode(graph, "Variant", id = x$variant_id)

    #       rels <- getRels(graph, "MATCH (:Namespace {id:{id_ns}})-[h:HAS_TS]->(:Timeseries {id:{id_ts}}) RETURN h",
    #         id_ns = "ns_1", id_ts = "a")
    #       rels <- getRels(graph, "MATCH (:Namespace {id:{id_ns}})-[h:HAS_TS]->(:Timeseries {id:{id_ts}}) RETURN h",
    #         id_ns = ns, id_ts = ts)

    query <- sprintf("
      MATCH (ns:Namespace)-[h:HAS_TS]->(ts:Timeseries)
      WHERE ns.id = '%s' AND ts.id = '%s'
      RETURN count(h) AS count
      ", x$ns_id, x$ts_id)
    count <- unlist(cypher(graph, query))

    if(count == 0) {
      createRel(ns, "HAS_TS", ts)
    }

    query <- sprintf("
      MATCH (ts:Timeseries)-[h:HAS_PROJECT]->(project:Project)
      WHERE ts.id = '%s' AND project.id = '%s'
      RETURN count(h) AS count
      ", x$ts_id, x$project_id)
    count <- unlist(cypher(graph, query))

    if(count == 0) {
      createRel(ts, "HAS_PROJECT", project)
    }

    query <- sprintf("
      MATCH (project:Project)-[h:HAS_VARIANT]->(variant:Variant)
      WHERE project.id = '%s' AND variant.id = '%s'
      RETURN count(h) AS count
      ", x$project_id, x$variant_id)
    count <- unlist(cypher(graph, query))

    if(count == 0) {
      createRel(project, "HAS_VARIANT", variant)
    }

    TRUE
  }

  createDb(x = list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
    variant_id = "a_v1.0.1"))
  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
    variant_id = "a_v1.0.2"))
  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
    variant_id = "a_v1.0.3"))

  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
    variant_id = "a_v1.1.1"))
  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
    variant_id = "a_v1.1.2"))
  createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
    variant_id = "a_v1.1.3"))

  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
    variant_id = "b_v1.0.1"))
  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
    variant_id = "b_v1.0.2"))
  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
    variant_id = "b_v1.0.3"))

  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
    variant_id = "b_v1.1.1"))
  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
    variant_id = "b_v1.1.2"))
  createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
    variant_id = "b_v1.1.3"))
}



# New start 2015-10-14 ----------------------------------------------------

graph <- startGraph("http://localhost:7474/db/data")

if (FALSE) {
  clear(graph, input = FALSE)

  data = data.frame(
    Origin = c("SFO", "AUS", "MCI"),
    FlightNum = c(1, 2, 3),
    Destination = c("PDX", "MCI", "LGA"),
    stringsAsFactors = FALSE
  )

  query = "
    MERGE (origin:Airport {name:{origin_name}})
    MERGE (destination:Airport {name:{dest_name}})
    CREATE (origin)<-[:ORIGIN]-(:Flight {number:{flight_num}})-[:DESTINATION]->(destination)
  "

  t = newTransaction(graph)

  for (i in 1:nrow(data)) {
    origin_name = data[i, ]$Origin
    dest_name = data[i, ]$Dest
    flight_num = data[i, ]$FlightNum

    appendCypher(t,
      query,
      origin_name = origin_name,
      dest_name = dest_name,
      flight_num = flight_num)
  }

  commit(t)

  cypher(graph, "MATCH (o:Airport)<-[:ORIGIN]-(f:Flight)-[:DESTINATION]->(d:Airport)
       RETURN o.name, f.number, d.name")
  browse(graph)
}


## Only needed once //
if (FALSE) {
  wd <- setwd("apps/events")
  clear(graph, input = FALSE)
  addConstraint(graph, "Timeseries", "id")

  input <- list(
    timeseries = "data/enbw_longterm_ts_a.csv"
  )
  importIntoDb <- function(input) {

    if (!is.null(ts <- input$timeseries)) {
      if (file.exists(ts)) {
        ts_actual <- loadData(path = ts)
        ts_actual <- formatData(ts_actual, extended = TRUE, with_ids = TRUE)
      }
      ts_node <- getOrCreateNode(graph, "Timeseries", id = 1, date = as.character(ts_actual$date))

summary(ts_node)
      query <- sprintf("
        MATCH (ns:Namespace)-[h:HAS_TS]->(ts:Timeseries)
        WHERE ns.id = '%s' AND ts.id = '%s'
        RETURN count(h) AS count
        ", x$ns_id, x$ts_id)
      count <- unlist(cypher(graph, query))

      if(count == 0) {
        createRel(ns, "HAS_TS", ts)
      }
    }

    TRUE
  }
}
