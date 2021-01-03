
# class
answer<-function(text){
  if(!is.character(text)){stop("text must be character")}
  if(length(text)!=1){stop("text must have length 1")}
  class(text)<-"telbot_answer"
  attributes(text)$uuid <- uuid::UUIDgenerate(use.time = FALSE)
  attributes(text)$links<-data.frame(uuid = character(0), label = character(0))
  # attributes(text)$media <- media
  text
}



print.telbot_answer<-function(x){
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

has_buttons<-function(answer){
  nrow(get_answer_buttons(answer))>0
}





get_answer_buttons<-function(answer){
  unique(data.frame(uuid = attributes(answer)$links_uuid,
                    label = attributes(answer)$links_label))
}


# transform


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


is.answer<-function(x){
  "telbot_answer" %in% class(x)
}

# modify
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



















