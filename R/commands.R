#' Answer to Command
#'  @export
command<-function(command, answer){
  handler<- telegram.bot::CommandHandler(command,
                                         function(bot, update){
                                           send_answer(answer,bot,update)
                                         })
  return(handler)
}
