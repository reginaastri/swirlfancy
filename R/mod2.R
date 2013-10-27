library(stringr)

module <- new.env()

# Read swirl 1.0 course content into the module environment
module$mod2 <- read.csv("data/mod2.csv", as.is=TRUE)
# As a convenience, store mod2's number of rows there too.
module$rows <- nrow(module$mod2) 

# Read the cars dataset from the openintro package into the global env.
# (It's stored as a csv in case openintro is not installed.)
cars <- read.csv("data/cars.csv", as.is=TRUE, comment.char="#")

makeState <- function(n){
 module$row.nr <- n
  if(n > module$row.nr)return(NULL)
  module$hint <- FALSE
  content <- module$mod2[n,]
  msg <- content[1,"Output"]
  skip.prompt <- !(content[1,"Output.Type"]=="question" && 
                     content[1,"Answer.Type"]=="command")
  structure(list(msg = msg, next_state = n+1, test = runState, skip.prompt=skip.prompt), class = c("mod2","state"))
}

firstState.mod2 <- function(){
  makeState(1)
}

nextState.mod2 <- function(state){
  makeState(state$next_state)
}

runState <- function(tree){
  if(module$row.nr <= module$rows){
    content <- module$mod2[module$row.nr,]
    if(content[,1]=="text"){
      return(runTextState(content))
    } else if(content[,1]=="figure"){
      return(runFigureState(content))
    } else if(content[,1]=="video"){
      return(runVideoState(content))
    } else if(content[,1]=="question"){
      return(runQuestionState(content, tree))
    } else {
      message("Can't do this one; run nxt()")
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

runTextState <- function(content){
  return(TRUE)
}

runFigureState <- function(content){
  file.path <- paste("R",content[,"Figure"],sep="/")
  source(file=file.path, local=TRUE)
  if(content[,"Figure.Type"] == "addition"){
    frndlyOut("I'm displaying the previous plot in case you need to refer back to it.")}
  return(TRUE)
}

runVideoState <- function(content){
  resp <- readline("ANSWER: ")
  if(resp %in% c("y", "Y", "yes", "Yes")){
    browseURL(content[,"Video"])
  }
  return(TRUE)
}

runQuestionState <- function(content, tree){
  ans <- TRUE
  if(content[,"Answer.Type"] == "text"){
    ans <- answerText(content)
  } else if (content[,"Answer.Type"] == "multiple"){
    ans <- answerMultiple(content)
  } else if (content[,"Answer.Type"] == "exact"){
    ans <- answerExact(content)
  } else if (content[,"Answer.Type"] == "range"){
    ans <- answerRange(content)
  } else if (content[,"Answer.Type"] == "command"){
    
  }
  
}

answerText <-function(content){
  respond(readline("ANSWER: ") == content[,"Correct.Answer"])
}

answerMultiple <- function(content){
  choices <- str_trim(unlist(strsplit(content[,"Choices"], ";")))
  respond(select.list(choices) == content[,"Correct.Answer"])
}

answerExact <- function(content){
  suppressWarnings(
    ans <- try(as.numeric(readline("ANSWER: ")), silent=TRUE)
  )
  respond(is.numeric(ans) && ans == as.numeric(content[,"Correct.Answer"]))
}

answerRange <- function(content){
  suppressWarnings(
    ans <- try(as.numeric(readline("ANSWER: ")), silent=TRUE)
  )
  # assume the correct answer will convert correctly to numeric
  temp <- as.numeric(unlist(str_split(content[,"Correct.Answer"],";")))
  # use is.logical in case the user types a non-digit which converts to NA's
  respond(is.logical(ans >= temp[1] && ans <= temp[2]))
}

respond <- function(correct){
  if(correct){
    swirl_out("That is correct. Brilliant!")
  } else {
    swirl_out("Sorry. That's not quite what I need. Type nxt() for a hint and another try.")
  }
}

hint <- function(content){
  if(!is.na(content[,"Hint"]))swirl_out(content[,"Hint"])
}

answerCmd <- function(content, tree){
  if(!tree$ok){
    # Whatever the user entered caused an error
    swirl_out(paste("Oops!"))
    hint(content)
    return(FALSE)
  }
  # Evaluate the correct answer within environment "module"
  # so as not to interfere with what the user has done in
  # the global environment. Assign the result to "ans" within
  # "module" in order to make it accessible.
  temp <- paste("ans <-", content[,"Correct.Answer"])
  eval(parse(text=temp))
  ans.is.correct <- identical(tree$val, module$ans)
  correct.expr <- parse(text=content[,"Correct.Answer"])
  call.is.correct <- identical(tree$expr, correct.expr[[1]])
  if(!call.is.correct && !ans.is.correct){
    respond(FALSE)
    hint(content)
    return(FALSE)
  } else if(!call.is.correct && ans.is.correct){
    swirl_out(paste("You got the right value but used a different expression for the purpose. You entered ", as.character(as.expression(..1)),", while I had expected", content[,"Correct.Answer"],"."))
    hint(content)
    return(FALSE)
  } else {
    respond(TRUE)
    return(TRUE)
  }
  return(TRUE)
}
