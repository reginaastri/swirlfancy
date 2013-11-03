library(stringr)

removeTaskCallback(which(getTaskCallbackNames() == "frndly"))

module <- new.env(parent = emptyenv())

module$mod <- read.csv("data/testModSimplest.csv", as.is=TRUE)
module$rows <- nrow(module$mod)

hi <- function() {
  addTaskCallback(auto_advance, name = "frndly")
  jmp(1)
  invisible()
}

bye <- function() {
  frndly_out("All done! Use jmp(1) to restart.")
  removeTaskCallback(which(getTaskCallbackNames() == "frndly"))
  invisible()
}

jmp <- function(state) {
  if (is.null(state)) return(bye())
  
  state <- module$mod[1, ]
  
  if (!is.null(state$test) && !state$test()) {
    module$test <- state$test    
  }
  
  frndly_out(state$msg)
  
  if(is.null(state$test)) {
    readline("Press <enter> to continue... ")
    nxt()
  }
}

nxt <- function() {
  jmp(module$next_state)
}
attr(nxt, "source") <- "| Hey you! To evaluate a function in R, you need to put () on the end."

auto_advance <- function(...) {
  if (is.null(module$test)) return(TRUE)
  
  # Auto-advance if test is true
  if (module$test()) {
    nxt()    
  }
  
  return(TRUE)
}


### STATES

new_state <- function(msg, next_state = NULL, test = NULL) {
  structure(list(msg = msg, next_state = next_state, test = test), 
            class = "state")
}

is.state <- function(x) inherits(x, "state")

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

state5 <- new_state("Good work!", NULL)


### HELPER FUNCTIONS

frndly_out <- function(...) {
  message()
  wrapped <- strwrap(str_c(..., sep = " "), 
                     width = getOption("width") - 2)
  message(str_c("| ", wrapped, collapse = "\n"))
  message()
}