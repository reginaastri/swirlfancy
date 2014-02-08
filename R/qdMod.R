testmods <- "inst/Courses/Test_Modules"

nxtMod <- function(){
  if(length(testmods)==0){
    last = "module0"
  } else {
    last <- sort(dir(testmods), decreasing=TRUE)[1]
  }
  1 + as.numeric(substr(last, nchar(last), nchar(last)))
}

# Creates a new qdmod.R source file in the Test Module directory
# @param n the number of a module subdirectory; default is next available
newSrc <- function(n = nxtMod()){
  nwrow <- '\nnewrow <- function(Class=NA, Output=NA, CorrectAnswer=NA, AnswerChoices=NA, AnswerTests=NA, 
                               Hint=NA, Figure=NA, FigureType=NA, VideoLink=NA){
  temp <- data.frame(Class=Class, Output=Output, CorrectAnswer=CorrectAnswer, 
                     AnswerChoices=AnswerChoices, AnswerTests=AnswerTests, Hint=Hint,
                     Figure=Figure, FigureType=FigureType, VideoLink=VideoLink)
}\n'
  svmod <- '\nsavemod <- function(){
  write.csv(module, modpath, row.names=FALSE)
}\n'
  moddir <- file.path(testmods, paste0("module",n))
  if(file.exists(moddir)){
    ans <- readline(paste(moddir, "already exists. Continue? "))
    if(ans != yes)return()
  }
  dir.create(moddir)
  sourcefile <<- file.path(moddir, paste0("mod",n,".R"))
  file.create(sourcefile)
  modpath <- file.path(moddir, paste0("mod_", n, ".csv"))
  cat(paste0('modpath <- "', modpath,'"\n'), file=sourcefile, append=TRUE)
  cat(svmod, file=sourcefile, append=TRUE)
  cat(nwrow, file=sourcefile, append=TRUE )
  defmod <- paste(c('\nmodule <- newrow(', rep("NULL, ", 8), "NULL" , ')'), collapse="")
  cat(defmod, file=sourcefile, append=TRUE)
  file.edit(sourcefile)
}

# qdmod help
hlp <- function(){
  print("txt -- just text, no question")
  print("qmult -- multiple choice question")
  print("qcmd -- command line question")
  print("vid -- video")
  print("fig -- figure")
  print("qx -- question requiring exact numerical answer")
  print("qtxt -- question requiring one-word text answer")
}

# template for presentation without a question
txt <- function(){
cat('\n
# just text, no question
module <- rbind(module, newrow(
  Class="text", 
  Output="commentary"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# template for multiple choice question
qmult <- function(){
cat('\n
# multiple choice question
module <- rbind(module, newrow(
  Class="mult_question",
  Output="stuff",
  Choices="ANS,2,3",
  CorrectAnswer="ANS"
  AnswerTests="omnitest(correctVal=ANS)"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# template for command question
qcmd <- function(){
cat('\n
# requires user to enter a command
module <- rbind(module, newrow(
  Class="cmd_question",
  Output="stuff",
  CorrectAnswer="whatever",
  AnswerTests="omnitest(correctExpr=???, correctVal=???)",
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# template for video
vid <- function(){
cat('\n
# asks if user would like to watch a video
module <- rbind(module, newrow(
  Class="video",
  Output="Would you like to watch a video?",
  VideoLink="http://address.of.video"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# template for figure

fig <- function(){
cat('\n
# presents a figure with commentary
# code to draw figure must be in sourcefile.R
module <- rbind(module, newrow(
  Class="figure",
  Output="commentary",
  Figure="sourcefile.R",
  FigureType="new or old"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# template for question requiring an exact numerical
# answer
qx<- function(){
cat('\n
# User must give an exact, numerical answer
module <- rbind(module, newrow(
  Class="exact_question",
  Output="stuff",
  CorrectAnswer=n,
  AnswerTests="omnitest(correctVal=n)",
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# template for question requiring an exact textual answer 
qtxt <- function(){
cat('\n
# requires the user to respond with text via readline 
module <- rbind(module, newrow(
  Class="text_question",
  Output="stuff",
  CorrectAnswer="answer",
  AnswerTests="omnitest(correctVal=answer)"
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}
