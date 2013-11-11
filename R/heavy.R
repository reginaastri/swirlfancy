library(stringr)
library(plotrix)

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

hiHeavy <- function(course, module_of_data){
  if (missing(course)){
    course <- chooseCourse()
  }
  if (missing(module_of_data)){
    module_of_data <- getModule()
    cat("\nOn which module would you like to begin?")
    module.start <- readline("\nANSWER: ")
  }

  module <- new.env(parent = emptyenv())
  # Read swirl 1.0 course content into the module environment
  module$mod2 <- read.csv(module_of_data, as.is=TRUE)
  # As a convenience, store mod2's number of rows there too.
  module$rows <- nrow(module$mod2)
  module$suspended <- FALSE
  assign("module", module, envir=globalenv())
  assign("cars", read.csv("data/cars.csv", as.is=TRUE, comment.char="#"), envir=globalenv())
  removeTaskCallback(which(getTaskCallbackNames() == "heavy"))
  # Register function cback() as that to be called
  # upon completion of any "top level" task, i.e., a command the
  # user enters from the R prompt.
  addTaskCallback(cback, name = "heavy")
  module$state <- makeState(1)
  invisible()
}

byeHeavy <- function(){
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
    if(is.null(state))return(FALSE) # will unregister callback
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


chooseCourse <- function() {
  #courseDirList <- list.files(path=file.path(path.package("swirl"), "Courses"))
  courseList <- list("Data_Analysis", "Mathematical_Biostatistics_Boot_Camp_2","Open_Intro")

  cat("\nWhich course would you like to take?\n")
  courseName <- select.list(courseNames)
 
  #return(list(courseDirName, courseName))
  return(courseName)
}

getModule <- function(courseName){
  if ((identical(courseName,"Data_Analysis")) ||(identical(courseName,"Mathematical_Biostatistics_Boot_Camp_2")))
    {
    modChoice <- paste("Module",1:3,sep="")
  }
  else if (identical(courseName,"Open_Intro")){
    modChoice <- "Module1"
  }
  else{
    frndlyOut("That's not an option. Try again")
    byeHeavy()
    return(TRUE)
  }
  modList <- list(modChoice)
  
  cat("\nWhich course would you like to take?\n")
  modNumber <- select.list(modList)
 
  return(modNumber)
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
    return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
  }
}

doStage.figure <- function(state, exper, val){
  frndlyOut(state$content[,"Output"])
  file.path <- paste("R",state$content[,"Figure"],sep="/")
  source(file=file.path, local=TRUE)
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
    frndlyOut("") # put in an empty line for visibility
    return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
  } else {
    if(!is.na(state$content[,"Hint"])){
      state$stage <- 2 # we take care of the hint here
    }
    return(list(finished=FALSE, prompt=FALSE, suspend=FALSE, state=state))
  }
}

doStage.text <- function(state, expr, val){
  if(state$stage==1){
    frndlyOut(state$content[,"Output"])
  } else {
    # There may not be a hint, but we handle that below
    frndlyOut(state$content[,"Hint"])
  }
  correct.ans <- str_trim(unlist(strsplit(state$content[,"Correct.Answer"],",")))
  user.ans <- str_trim(unlist(strsplit(readline("ANSWER: "),",")))
  correct <- identical(sort(correct.ans), sort(user.ans))
  respond(correct)
  if(correct){
    state$stage <- -1
    frndlyOut("")
    return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
  } else {
    if(!is.na(state$content[,"Hint"])){
      state$stage <- 2 # we take care of the hint here
    }
    suspend <- suspendQ()
    return(list(finished=FALSE, prompt=suspend, suspend=suspend, state=state))
  }
}

doStage.exact <- function(state, expr, val){
  if(state$stage == 1){
    frndlyOut(state$content[1,"Output"])
    state$stage <- 2
    return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
  } else if(state$stage == 2){
    is.correct <- FALSE
    if(is.numeric(val)){
      correct.ans <- eval(parse(text=state$content[,"Correct.Answer"]))
      epsilon <- 0.01*abs(correct.ans)
      is.correct <- abs(val-correct.ans) < epsilon
    }
    if(is.correct){
      respond(TRUE)
      frndlyOut("")
      state$stage <- -1
      return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
    } else {
      respond(FALSE)
      state$stage <- 2 # Try again, after hint
      frndlyOut(state$content[1,"Hint"])
      return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
    }
  }
}

doStage.range <- function(state, expr, val){
  if(state$stage == 1){
    frndlyOut(state$content[1,"Output"])
    state$stage <- 2
    return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
  } else if(state$stage == 2){
    is.correct <- FALSE
    if(is.numeric(val)){
      temp <- as.numeric(unlist(strsplit(state$content[,"Correct.Answer"],";")))
      is.correct <- temp[1] <= val && val <= temp[2]
    }
    if(is.correct){
      respond(TRUE)
      frndlyOut("")
      state$stage <- -1
      return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
    } else {
      respond(FALSE)
      state$stage <- 2 # Try again, after hint
      frndlyOut(state$content[1,"Hint"])
      return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
    }
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
      frndlyOut("")
      state$stage <- -1
      return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
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