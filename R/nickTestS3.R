source("R/heavy.R")

hiNickTest <- function() {
  removeTaskCallback(which(getTaskCallbackNames() == "nickTest"))
  addTaskCallback(cback, name = "nickTest")
  assign("module", new.env(parent = emptyenv()), envir=globalenv())
  module$state <- state1
  module$suspended <- FALSE
  invisible()
}

byeNickTest <- function() {
  removeTaskCallback(which(getTaskCallbackNames() == "nickTest"))
  invisible()
}


### STATES

new_state <- function(msg, next_state = NULL, test = NULL) {
  cls <- c("nickNoTestState")
  if(!is.null(test))cls <- c("nickTestState", cls)
  structure(list(msg = msg, stage=1, next_state = next_state, test = test), 
            class = cls)
}

nextState.nickNoTestState <- function(state){
  name <- str_c("state", state$next_state)
  if(exists(name)){
    return(get(name))
  } else {
    return(NULL)
  }
}

doStage.nickNoTestState <- function(state, expr, val){
  frndly_out(state$msg)
  state$stage <- -1
  readline("Press <enter> to continue... ")
  return(list(return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
))
}

doStage.nickTestState <- function(state, expr, val){
  if(state$stage == 1){
    frndly_out(state$msg)
    state$stage <- 2
    return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
  } else {
    if(state$test()){
      state$stage <- -1
      return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
    } else {
      return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
    }  
  }
}

state1 <- new_state("Hi! I'm frndly, your friendly introduction to R. I'm
                    going to guide you through a quick introduction to R. You'll know it's me
                    talking whenever you see output that starts with |. Otherwise you'll
                    interacting with R in exactly the same way you will when I'm not around.", 2)

state2 <- new_state("In R, you create variables with the arrow: <-,
                    like a <- 1 or b <- 2.  To continue, create a new variable called d with the 
                    value 10, or type nxt().", 3, function() {
                      exists("d", globalenv()) && get("d", globalenv()) == 10 
                    })

state3 <- new_state("You are doing so well!", 4)

state4 <- new_state("Now create a variable called f with 4 times the value of d.", 5, function() {
  exists("f", globalenv()) && get("f", globalenv()) == 40 
})

state5 <- new_state("Good work!", 100)


### HELPER FUNCTIONS

frndly_out <- function(...) {
  message()
  wrapped <- strwrap(str_c(..., sep = " "), 
                     width = getOption("width") - 2)
  message(str_c("| ", wrapped, collapse = "\n"))
  message()
}