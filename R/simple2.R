source("R/heavy.R")

# Just by renaming its columns, testModSimple can be run using classes 
# defined in heavy.R

hiSimple2 <- function(){
  module <- new.env(parent = emptyenv())
  # Read swirl 1.0 course content into the module environment
  module$mod2 <- read.csv("data/testModSimple2.csv", as.is=TRUE)
  # As a convenience, store mod2's number of rows there too.
  module$rows <- nrow(module$mod2)
  module$suspended <- FALSE
  assign("module", module, envir=globalenv())
  removeTaskCallback(which(getTaskCallbackNames() == "heavy"))
  # Register function cback() as that to be called
  # upon completion of any "top level" task, i.e., a command the
  # user enters from the R prompt.
  addTaskCallback(cback, name = "heavy")
  module$state <- makeState(1)
  invisible()
}

byeSimple2 <- function(){
  removeTaskCallback(which(getTaskCallbackNames() == "heavy"))
  module$state <- NULL
  invisible()
}