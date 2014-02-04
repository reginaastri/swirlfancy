testmods <- "inst/Courses/Test_Modules"

nxtMod <- function(){
  if(length(testmods)==0){
    last = "module0"
  } else {
    last <- sort(dir(testmods), decreasing=TRUE)[1]
  }
  1 + as.numeric(substr(lastMod(), nchar(last), nchar(last)))
}

saveAs <- function(n = nxtMod()){
  if(!exists("module", globalenv()))stop("No module present in global environment")
  moddir <- file.path(testmods, paste0("module",n))
  if(!dir.exists(moddir))dir.create(moddir)
  fpath <- file.path(moddir, paste0("mod",n,"_new.csv"))
  write.csv(mod, file=fpath, row.names=FALSE)
}

newrow <- function(Class=NA, Output=NA, CorrectAnswer=NA, AnswerChoices=NA, AnswerTests=NA, Hint=NA, Figure=NA, FigureType=NA, VideoLink=NA){
  temp <- data.frame(Class=Class, Output=Output, CorrectAnswer=CorrectAnswer, 
                     AnswerChoices=AnswerChoices, AnswerTests=AnswerTests, Hint=Hint,
                     Figure=Figure, FigureType=FigureType, VideoLink=VideoLink)
}

newMod <- function(sourcefile){
  if(exists("module", globalenv())){
    print("A module already exists, do you want to overwrite it?")
    ans <- readline("yes or no: ")
    if(ans != "yes")stop("Exiting to save module")
  }
  if(exists("module", globalenv())){
    print(paste(sourcefile, "already exists, do you want to add to it?")
    ans <- readline("yes or no: ")
    if(ans != "yes")stop("Exiting to change source file")
  }
  assign("module", newrow(), globalenv())
  assign("sourcefile", sourcefile, globalenv())
}

hlp <- function(){
  print("txt -- just text, no question")
  print("qmult -- multiple choice question")
  print("qcmd -- command line question")
  print("vid -- video")
  print("fig -- figure")
  print("qx -- question requiring exact numerical answer")
  print("")
}

# text
txt <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="text", 
  Output="stuff"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# mult_question
qmult <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="mult_question",
  Output="stuff",
  Choices="ANS,2,3",
  CorrectAnswer="ANS"
  AnswerTests="matches=ANS"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# cmd_question
qcmd <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="cmd_question",
  Output="stuff",
  CorrectAnswer="whatever",
  AnswerTests="uses_func=;equivalent=;creates_var=;equals=expr,name;identical=expr,val_length=",
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# video
vid <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="video",
  Output="stuff",
  VideoLink="http://address.of.video"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# figure
fig <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="figure",
  Output="stuff",
  Figure="sourcefile.R",
  FigureType="new or old"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# exact_question
qx<- function(){
cat('\n
module <- rbind(module, newrow(
  Class="exact_question",
  Output="stuff",
  CorrectAnswer="n",
  AnswerTests="exact=n"
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# range_question
qrng <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="range_question",
  Output="stuff",
  CorrectAnswer="n1,n2",
  AnswerTests="range=n1,n2"
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}


# text_question
qtxt <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="text_question",
  Output="stuff",
  CorrectAnswer="word",
  AnswerTests="matches=word"
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# text_many_question
qmany <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="text_many_question",
  Output="stuff",
  CorrectAnswer="word1,word2,word3",
  AnswerTests="word_many=word1,word2,word3",
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}

# text_order_question       
qord <- function(){
cat('\n
module <- rbind(module, newrow(
  Class="text_order_question",
  Output="stuff",
  CorrectAnswer="word1,word2,word3",
  AnswerTests="word_order=word1,word2,word3",
  Hint="hint"
  ))', file=sourcefile, append=TRUE)
invisible()
}