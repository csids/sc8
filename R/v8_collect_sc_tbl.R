#' collect
#' @param x tbl object
#' @param ... dots
#' @method collect sc_tbl_v8
#' @export
#' @importFrom dplyr collect
collect.sc_tbl_v8 <- function(x, ...){
  collect(NextMethod())
}


