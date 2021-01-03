#' start the telegram chatbot
#'
#' @param token your telegram bot api token
#' @param ... answers and commands
start_bot<-function(token,...){
  updater<-telegram.bot::Updater(token)
  handlers<-list(...)

  for(handler in handlers){
    updater = updater + handler
  }
  updater = updater + telegram.bot::ErrorHandler(error_callback)
  updater$start_polling()

}
