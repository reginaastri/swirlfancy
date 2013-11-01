library(stringr)

# Implement the module content and state as an empty environment,
# following Hadley.
module <- new.env(parent = emptyenv())

# Read swirl 1.0 course content into the module environment
module$mod2 <- read.csv("data/mod2.csv", as.is=TRUE)
# As a convenience, store mod2's number of rows there too.
module$rows <- nrow(module$mod2)
# Indicate that we are not waiting for user input
module$suspended <- TRUE

# Read the cars dataset from the openintro package into the global env.
# (It's stored as a csv in case openintro is not installed.)
cars <- read.csv("data/cars.csv", as.is=TRUE, comment.char="#")

makeState <- function(n){
  if(n > nrow(module$mod2))return(NULL)
  cls <- c("state")
  content <- module$mod2[n,]
  if(content[,"Output.Type"] != "text"){
    cls <- c(content[,"Output.Type"], cls)
  }
  if(content[,"Output.Type"] == "question"){
    cls <- c(content[,"Answer.Type"], cls)
  }
  structure(list(content=content, row=n, stage=1), class = cls)
}

nxt <- function(){
  invisible()
}

hi <- function(){
  removeTaskCallback(which(getTaskCallbackNames() == "heavy"))
  # Register function cback() as that to be called
  # upon completion of any "top level" task, i.e., a command the
  # user enters from the R prompt.
  addTaskCallback(cback, name = "heavy")
  module$state <- makeState(1)
  invisible()
}

bye <- function(){
  removeTaskCallback(which(getTaskCallbackNames() == "heavy"))
  module$state <- NULL
  invisible()
}

cback <- function(...){
  if(identical(..1, parse(text="nxt()")[[1]]) ||
       identical(..1, parse(text="hi()")[[1]])){
    module$suspended <- FALSE
  }
  if(module$suspended)return(TRUE)
  
  n <- 1 # to avoid any infinte loop
  while(n < 20){
    n <- n+1
    state <- module$state
    response <- doStage(state)
    module$suspended <- response$suspend
    if(response$finished)module$state <- nextState(state)
    if(response$prompt)break
  } 
  return(TRUE)
}

### STATES

doStage <- function(state)UseMethod("doStage")
nextState <- function(state)UseMethod("nextState")

doStage.default <- function(state){
  frndlyOut(state$content[1,"Output"])
  state$stage <- state$stage+1
  temp <- readline("Press <enter> to continue: ")
  suspend <- temp != ""
  if(suspend)frndlyOut("Type nxt() to continue.")
  return(list(finished=TRUE, prompt=suspend, suspend=suspend, state=state))
}

nextState.default <- function(state){
  return(makeState(1+state$row))
}

### UTILS

# Hadley's display function
frndlyOut <- function(...) {
  # Format the argument for pretty display.
  # getOption("width") gives screen width in characters.
  wrapped <- strwrap(str_c(..., sep = " "), 
                     width = getOption("width") - 2)
  # Start each line with "| " (str_c is in package stringr)
  # and display the result.
  message(str_c("| ", wrapped, collapse = "\n"))
}
