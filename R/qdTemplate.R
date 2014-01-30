modpath <- "inst/Courses/Test_Modules/module?/mod?_new.csv"
if(!file.exists(dirname(modapth)))file.create(dirname(modpath), recursive=TRUE)

newrow <- function(Class=NA, Output=NA, CorrectAnswer=NA, AnswerChoices=NA, AnswerTests=NA, Hint=NA, Figure=NA, FigureType=NA, VideoLink=NA){
  temp <- data.frame(Class=Class, Output=Output, CorrectAnswer=CorrectAnswer, 
                     AnswerChoices=AnswerChoices, AnswerTests=AnswerTests, Hint=Hint,
                     Figure=Figure, FigureType=FigureType, VideoLink=VideoLink)
}

module <- newrow(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)


# text
module <- rbind(module, newrow(
  Class="text", 
  Output="stuff"
  ))


# mult_question
module <- rbind(module, newrow(
  Class="mult_question",
  Output="stuff",
  Choices="ANS,2,3",
  CorrectAnswer="ANS"
  AnswerTests="matches=ANS"
  ))


# cmd_question
module <- rbind(module, newrow(
  Class="cmd_question",
  Output="stuff",
  CorrectAnswer="whatever",
  AnswerTests="uses_func=;equivalent=;creates_var=;equals=expr,name;identical=expr,val_length=",
  Hint="hint"
  ))


# video              
module <- rbind(module, newrow(
  Class="video",
  Output="stuff",
  VideoLink="http://address.of.video"
  ))


# figure
module <- rbind(module, newrow(
  Class="figure",
  Output="stuff",
  Figure="sourcefile.R",
  FigureType="new or old"
  ))


# exact_question
module <- rbind(module, newrow(
  Class="exact_question",
  Output="stuff",
  CorrectAnswer="n",
  AnswerTests="exact=n"
  Hint="hint"
  ))



# range_question
module <- rbind(module, newrow(
  Class="range_question",
  Output="stuff",
  CorrectAnswer="n1,n2",
  AnswerTests="range=n1,n2"
  Hint="hint"
  ))


# text_question
module <- rbind(module, newrow(
  Class="text_question",
  Output="stuff",
  CorrectAnswer="word",
  AnswerTests="matches=word"
  Hint="hint"
  ))


# text_many_question
module <- rbind(module, newrow(
  Class="text_many_question",
  Output="stuff",
  CorrectAnswer="word1,word2,word3",
  AnswerTests="word_many=word1,word2,word3",
  Hint="hint"
  ))

# text_order_question       
module <- rbind(module, newrow(
  Class="text_order_question",
  Output="stuff",
  CorrectAnswer="word1,word2,word3",
  AnswerTests="word_order=word1,word2,word3",
  Hint="hint"
  ))

write.csv(module, file=modpath, row.names=FALSE)