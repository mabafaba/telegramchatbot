
#' shorthand to create telegram.bot keyboard from key/value pairs
#' @param ... argument name becomes data, value becomes label: quick_keyboard(data_send_back = "a button label")
#' @return a telegram.bot inline keyboard object
quick_keyboard<-function(...){
  buttons <- list(...)

  # map in parallel over argument values and names and pass to inlinekeyboardbutton

  buttons <- purrr::map2(unname(unlist(buttons)), names(buttons),
                  function(x,y){
                    list(telegram.bot::InlineKeyboardButton(text = x,callback_data = y))}
  )

  telegram.bot::InlineKeyboardMarkup(buttons)

}
















