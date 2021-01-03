
# default handler
error_callback <- function(bot, error) {
  warning(simpleWarning(conditionMessage(error), call = "Updates polling"))
}
