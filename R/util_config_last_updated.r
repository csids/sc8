update_config_last_updated_internal <- function(type, tag, date = NULL, datetime = NULL) {
  stopifnot(type %in% c("task", "data"))
  if (is.null(config$schemas$config_last_updated$conn)) config$schemas$config_last_updated$connect()

  if (!is.null(datetime)) datetime <- as.character(datetime)

  if (is.null(date) & is.null(datetime)) {
    date <- lubridate::today()
    datetime <- as.character(lubridate::now())
  }
  if (is.null(date) & !is.null(datetime)) {
    date <- stringr::str_sub(datetime, 1, 10)
  }
  if (!is.null(date) & is.null(datetime)) {
    datetime <- paste0(date, " 00:01:00")
  }
  to_upload <- data.table(
    type = type,
    tag = tag,
    date = date,
    datetime = datetime
  )
  config$schemas$config_last_updated$upsert_data(to_upload)
}

#' update_config_last_updated
#' Updates the config_last_updated db tables
#' @param type a
#' @param tag a
#' @param date Date to set in config_last_updated
#' @param datetime Datetime to set in config_last_updated
#' @export
update_config_last_updated <- function(type, tag, date = NULL, datetime = NULL) {
  if (!stringr::str_detect(tag, "^tmp") & !tag %in% c("config_datetime", "config_last_updated", "permission", "rundate")) update_config_last_updated_internal(type = type, tag = tag)
}

#' get_config_last_updated
#' Gets the config_last_updated db table
#' @param type a
#' @param tag a
#' @export
get_config_last_updated <- function(type = NULL, tag = NULL) {
  if (is.null(config$schemas$config_last_updated$conn)) config$schemas$config_last_updated$connect()

  if (!is.null(tag)) {
    temp <- config$schemas$config_last_updated$tbl() %>%
      dplyr::filter(tag == !!tag) %>%
      dplyr::collect() %>%
      as.data.table()
  } else {
    temp <- config$schemas$config_last_updated$tbl() %>%
      dplyr::collect() %>%
      as.data.table()
  }
  if (!is.null(type)) {
    x_type <- type
    temp <- temp[type == x_type]
  }
  return(temp)
}
