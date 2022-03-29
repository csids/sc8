#' collect
#' @param x tbl object
#' @param ... dots
#' @method collect sc_tbl_v8
#' @export
#' @importFrom dplyr collect
collect.sc_tbl_v8 <- function(x, ...){
  keys <- attr(x, "sc_keys")
  names_exist <- colnames(x)
  keys <- keys[keys %in% names_exist]
  arrange_order <- unique(c(keys, names_exist))

  retval <- NextMethod() %>%
    collect() %>%
    as.data.table() %>%
    setorderv(arrange_order)
}


