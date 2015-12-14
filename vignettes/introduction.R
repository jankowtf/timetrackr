## ---- include = FALSE----------------------------------------------------
library(crudr)
run <- FALSE
root_dir <- paste(rep("../", 1), collapse = "")
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
knitr::opts_knit$set(root.dir = root_dir)

## ----echo=FALSE----------------------------------------------------------
crudr_uml <- '
  @startuml
  ICrud *-- Crud: implements <
  Crud <|-- Crud.Settings: extends <

  class ICrud {
    +init()
    +has()
    +create()
    +read()
    +update()
    +delete()
    +reset()
    -stopIfInterface()
  }
  class Crud {
    +main: environment
    -initial_state: list
    -stopIfEmpty()
    -createMessage(),
    -cacheInitialState()
  }
  class Crud.Settings {
    +main: function
    -cacheInitialState()
  }
  @enduml
'
# knitr::opts_knit$set(root.dir = root_dir)
uml_file <- crudr:::plantuml3(
  crudr_uml, 'Architecture', 
  filename = "vignettes/uml",
  jarfile = "lib/plantuml.jar",
  normalize = TRUE
)
# print(uml_file)
# uml_file <- normalizePath(uml_file)
# print(uml_file)
# print(getwd())
# knitr::opts_knit$set(root.dir = getwd())

## ---- eval = run---------------------------------------------------------
#  inst <- ICrud$new()
#  try(inst$init())
#  try(inst$has())
#  try(inst$create())
#  try(inst$read())
#  try(inst$update())
#  try(inst$delete())
#  try(inst$reset())

## ---- eval = run---------------------------------------------------------
#  inst <- Crud$new()
#  class(inst)
#  inherits(inst, "ICrud")

## ---- eval = run---------------------------------------------------------
#  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
#  
#  value <- inst$getMain()
#  as.list(value, sorted = TRUE)
#  
#  inst$setMain(as.environment(list(a = 10, b = 20)))
#  inst$getMain()$a
#  inst$getMain()$b

## ---- eval = run---------------------------------------------------------
#  inst <- Crud$new()

## ---- eval = run---------------------------------------------------------
#  inst$init(a = 1, b = 2, "c/foo/bar" = 3, list(d = 4, "e/foo/bar" = 5))

## ---- eval = run---------------------------------------------------------
#  inst$has("a")
#  inst$has("c/foo")
#  inst$has("a", "b")
#  inst$has("a", "e/foo/bar")
#  
#  inst$has("c")
#  inst$has("a", "c", "c/bar")

## ---- eval = run---------------------------------------------------------
#  inst$create(f = TRUE)
#  inst$create(g = TRUE, h = letters[1:3])
#  inst$create("i/foo" = 1, "c/foo/bar/new" = 1)
#  ## TODO 2015-11-04: review behavior for preserving values; turning everything
#  ## into a list would probably be nicer as would happen when calling
#  ## `inst$create("i/foo" = 1, "c/foo/bar/new/something" = 1)`
#  as.list(inst$getMain(), sorted = TRUE)
#  
#  (inst$create(c = 30, "c/foo" = 30))
#  ## --> c already existed with value = 3
#  as.list(inst$getMain(), sorted = TRUE)
#  inst$create(c = 30, strict = 1)
#  try(inst$create(c = 30, strict = 2))
#  try(inst$create(c = 30, strict = 3))
#  
#  ## Overwrite //
#  inst$create(c = 30, "c/foo" = 30, overwrite = TRUE)
#  ## TODO 2015-11-04: review behavior for preserving values; turning everything
#  ## into a list would probably be nicer
#  as.list(inst$getMain(), sorted = TRUE)
#  ## --> c overwritten

## ---- eval = run---------------------------------------------------------
#  inst$read()
#  inst$read("a")
#  inst$read("a", "b", "c/foo")
#  
#  inst$read("a", "b", "x")
#  try(inst$read("a", "b", "x", strict = 3))

## ---- eval = run---------------------------------------------------------
#  inst$read("a")
#  inst$update(a = 10)
#  inst$read("a")
#  
#  inst$read("a", "b")
#  inst$update(a = 1000, b = 2000)
#  inst$read("a", "b")
#  
#  inst$update(x = 99)
#  inst$read("x")
#  
#  inst$read("a", "x")
#  inst$update(a = 1, x = 99)
#  try(inst$update(a = 1, x = 99, strict = 3))
#  inst$read("a", "x")

## ---- eval = run---------------------------------------------------------
#  inst$read()
#  inst$delete("f")
#  
#  inst$delete("d", "c")
#  inst$read()
#  
#  inst$delete("x")
#  inst$read()
#  inst$delete("b", "x")
#  try(inst$delete("b", "x", strict = 3))
#  inst$read()

## ---- eval = run---------------------------------------------------------
#  inst$reset()
#  inst$read()
#  
#  inst$reset(type = "hard")
#  inst$read()

