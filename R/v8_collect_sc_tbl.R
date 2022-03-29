#' collect
#' @param x tbl object
#' @method collect sc_tbl_v8
#' @export
#' @importFrom dplyr collect
collect.sc_tbl_v8 <- function(x, order_by_keys = TRUE){
  retval <- retval %>%
    dplyr::collect()
}


