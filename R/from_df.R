


#' Create many answers at once
#'
#' @param texts vector of response texts
#' @param names names of the answers
#' @return list of answers
batch_answers<-function(texts, names){

  x<-as.list(as.character(texts))
  names(x)<-as.character(names)
  # media<-as.character(media)
  lapply(x,answer)
}





