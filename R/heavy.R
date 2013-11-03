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

cback <- function(expr, val, ok, vis){
  if(identical(expr, parse(text="nxt()")[[1]]) ||
       identical(expr, parse(text="hi()")[[1]])){
    module$suspended <- FALSE
  }
  if(module$suspended)return(TRUE)
  
  n <- 1 # to avoid any infinte loop
  while(n < 20){
    n <- n+1
    state <- module$state
    response <- doStage(state, expr, val)
    module$suspended <- response$suspend
    if(response$finished){
      module$state <- nextState(state)
    } else {
      module$state <- response$state
    }
    if(response$prompt)break
  } 
  return(TRUE)
}

### STATES

doStage <- function(state, expr, val)UseMethod("doStage")
nextState <- function(state)UseMethod("nextState")

doStage.default <- function(state, expr, val){
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

doStage.video <- function(state, expr, val){
  frndlyOut(state$content[,"Output"])
  resp <- readline("ANSWER: ")
  state$stage <- -1
  if(resp %in% c("y", "Y", "yes", "Yes")){
    browseURL(state$content[,"Video"])
    frndlyOut("Type nxt() to continue.")
    return(list(finished=TRUE, prompt=TRUE, suspend=TRUE, state=state))
  } else {
    suspend <- suspendQ()
    return(list(finished=TRUE, prompt=suspend, suspend=suspend, state=state))
  }
}

doStage.figure <- function(state, exper, val){
  frndlyOut(state$content[,"Output"])
  file.path <- paste("R",state$content[,"Figure"],sep="/")
  source(file=file.path, local=TRUE)
  if(state$content[,"Figure.Type"] == "addition"){
    frndlyOut("I'm displaying the previous plot in case you need to refer back to it.")}
  suspend <- suspendQ()
  state$stage <- -1
  return(list(finished=TRUE, prompt=suspend, suspend=suspend, state=state))
}

doStage.multiple <- function(state, expr, val){
  if(state$stage==1){
    frndlyOut(state$content[,"Output"])
  } else {
    # There may not be a hint, but we handle that below
    frndlyOut(state$content[,"Hint"])
  }
  choices <- str_trim(unlist(strsplit(state$content[,"Choices"], ";")))
  correct <- select.list(choices) == state$content[,"Correct.Answer"]
  respond(correct)
  if(correct){
    state$stage <- -1
    suspend <- suspendQ()
    return(list(finished=TRUE, prompt=suspend, suspend=suspend, state=state))
  } else {
    if(!is.na(state$content[,"Hint"])){
      state$stage <- 2 # we take care of the hint hers
    }
    suspend <- suspendQ()
    return(list(finished=FALSE, prompt=suspend, suspend=suspend, state=state))
  }
}

doStage.command <- function(state, expr, val){
  if(state$stage == 1){
    frndlyOut(state$content[1,"Output"])
    state$stage <- 2
    return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
  } else if(state$stage == 2){
    correct.expr <- parse(text=state$content[,"Correct.Answer"])
    ans.is.correct <- identical(val, eval(correct.expr))
    call.is.correct <- identical(expr, correct.expr[[1]])
    if(ans.is.correct && call.is.correct){
      respond(TRUE)
      suspend <- suspendQ()
      state$stage <- -1
      return(list(finished=TRUE, prompt=suspend, suspend=suspend, state=state))
    } else if(ans.is.correct && !call.is.correct){
      state$stage <- -1
      frndlyOut(paste("You got the right value but used a different expression for the purpose. You entered ", as.character(as.expression(expr)),", while I had expected", state$content[,"Correct.Answer"]))
      suspend <- suspendQ()
      return(list(finished=TRUE, prompt=suspend, suspend=suspend, state=state))
    } else {
      respond(FALSE)
      state$stage <- 2 # Try again, after hint
      frndlyOut(state$content[1,"Hint"])
      return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
    }
  }
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

respond <- function(correct){
  if(correct){
    frndlyOut("That is correct. Brilliant!")
  } else {
    frndlyOut("Sorry. That's not quite what I need.")
  }
}

suspendQ <- function(){
  temp <- readline("Press <enter> to continue: ")
  suspend <- temp != ""
  if(suspend)frndlyOut("Type nxt() to continue.")
  return(suspend)
}