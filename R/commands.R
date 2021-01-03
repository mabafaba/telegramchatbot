#' Answer to Command
#' @param command character string with the name of the command (without the forward slash)
#' @param answer the answer object to be linked to the command
#' @return the corresponding telegram.bot command handler
#' @export
command<-function(command, answer){
  handler<- telegram.bot::CommandHandler(command,
                                         function(bot, update){
                                           send_answer(answer,bot,update)
                                         })
  return(handler)
}
