



batch_answers<-function(texts, names){
  
  x<-as.list(as.character(texts))
  names(x)<-as.character(names)
  # media<-as.character(media)
  lapply(x,answer)
}





