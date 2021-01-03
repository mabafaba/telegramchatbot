
#' Bot Answer
#' @param text the response text of the answer
#' @details You can use basic Markdown syntax
#' @return an answer object
answer<-function(text){
  if(!is.character(text)){stop("text must be character")}
  if(length(text)!=1){stop("text must have length 1")}
  class(text)<-"telbot_answer"
  attributes(text)$uuid <- uuid::UUIDgenerate(use.time = FALSE)
  attributes(text)$links<-data.frame(uuid = character(0), label = character(0))
  # attributes(text)$media <- media
  text
}


#' print telbot answers
#' @param x an answer object
#' @param ... ignored
#' @export
print.telbot_answer<-function(x, ...){
  cat(as.character(x))
  cat("\n\n")
  cat(paste("uuid:", attributes(x)$uuid))
  cat("\nlinks:\n")
  print(knitr::kable(data.frame(uuid = attributes(x)$links_uuid,
                   label = attributes(x)$links_label)))
  # if(!is.null(media)){
    # cat("\nmedia:\n")
  # print(media)}
}


# get attributes

#' check if an answer has buttons attached
#' @param answer an answer object
#' @return logical TRUE if answer has buttons
has_buttons<-function(answer){
  nrow(get_answer_buttons(answer))>0
}




#' extract buttons from answer
#'
#' @param answer answer object
#' @return data frame of buttons needed for this answer
get_answer_buttons<-function(answer){
  unique(data.frame(uuid = attributes(answer)$links_uuid,
                    label = attributes(answer)$links_label))
}


#' create keyboard from answer
#'
#' @param answer answer to generate keyboard from
#' @return telegram.bot keyboard object
answer_keyboard<-function(answer){
  if(!has_buttons(answer)){
    # stop("can't make a keyboard for an answer without buttons.")
    return(NULL)
  }
  links <- get_answer_buttons(answer)
  links_list<- as.list(links$label)
  names(links_list) = links$uuid
  do.call(quick_keyboard,links_list)


}

#' check if an object is a telegram bot "answer" object
#' @param x object
#' @return logical, TRUE if the class of `x` includes "telbot_answer"
is.answer<-function(x){
  "telbot_answer" %in% class(x)
}

#' Add Buttons
#' @param answer the answer to add the button to
#' @param to the answer to which the button should link
#' @param label the label on the button
#' @return the answer with the added button
add_button <- function(answer,to, label){
  if(is.null(to)){warning(paste("to is null - not adding button. (label:",label,")"));return(answer)}
  if(!is.answer(answer)){stop("answer must be an answer object")}
  if(!is.answer(to)){stop("to must be an answer object")}
  if(!is.character(label)){stop("label must be character")}
  if(length(label)!=1){stop("label must be length 1 character vector")}
  to_uuid <- attributes(to)$uuid
  attributes(answer)$links_uuid<-c(attributes(answer)$links_uuid, to_uuid)
  attributes(answer)$links_label<-c(attributes(answer)$links_label, label)
  answer
}



















