

#' check if answer is requested by telegram.bot update
#' @param answer answer to check
#' @param update telgram.bot update object
#' @return logical TRUE if answer is requested
is_requested<-function(answer, update){
  cb_data<-update$callback_query$data
  if(length(cb_data)>0){
    if(attributes(answer)$uuid == cb_data){
      return(TRUE)
    }
  }
  return(FALSE)
}


#' send answer object
#' @param answer answer to send
#' @param bot telegram.bot bot object
#' @param update telegram.bot update object
send_answer<-function(answer, bot, update){
  print(answer)
  text <- as.character(answer)
  chat_id <-update$from_chat_id()
  if(has_buttons(answer)){
    keyboard <- answer_keyboard(answer)
  }else{
    keyboard<-NULL

  }
  bot$sendMessage(chat_id,
                  text,
                  reply_markup = answer_keyboard(answer),
                  parse_mode = "Markdown",
                  disable_web_page_preview = TRUE
                  )


}


#' react to request for answers
#' @param answer the answer
#' @param bot telegram.bot bot object
#' @param update telegram.bot update object
answer_with<-function(answer,bot,update){
  if(is_requested(answer, update)){send_answer(answer,bot,update)}
}

#' Answer handler
#' @param ... all answers
#' @export
answer_handler <- function(...){
  answers<-list(...)
  answer_callback<-function(bot, update){
    lapply(answers, answer_with, bot, update)
    # telegram.bot:::answerCallbackQuery(update$callback_query$id, text = "cbq - answer", show_alert = FALSE,
    #                     url = NULL, cache_time = NULL)

  }

  telegram.bot::CallbackQueryHandler(answer_callback)
}


