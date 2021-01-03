
#' default error handler
#' Catches all errors to ensure a live bot does not crash for all users when issues occur
#' @param bot telegram.bot bot object
#' @param error The Error
#' @return corresponding warning
error_callback <- function(bot, error) {
  warning(simpleWarning(conditionMessage(error), call = "Updates polling"))
}
