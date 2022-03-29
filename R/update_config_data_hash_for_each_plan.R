update_config_data_hash_for_each_plan <- function(task, index_plan, hash, date = NULL, datetime = NULL) {
  config$schemas$config_data_hash_for_each_plan$disconnect()
  config$schemas$config_data_hash_for_each_plan$connect()
  on.exit(config$schemas$config_data_hash_for_each_plan$disconnect())

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
    "task" = task,
    "index_plan" = index_plan,
    "date" = date,
    "datetime" = datetime,
    "hash" = hash
  )
  config$schemas$config_data_hash_for_each_plan$upsert_data(to_upload)
}

#' get_config_data_hash_for_each_plan
#' Gets the config_data_hash_for_each_plan db table
#' @param task a
#' @param index_plan a
#' @export
get_config_data_hash_for_each_plan <- function(task = NULL, index_plan = NULL) {
  config$schemas$config_data_hash_for_each_plan$disconnect()
  config$schemas$config_data_hash_for_each_plan$connect()
  on.exit(config$schemas$config_data_hash_for_each_plan$disconnect())

  if (!is.null(task)) {
    temp <- config$schemas$config_data_hash_for_each_plan$tbl() %>%
      dplyr::filter(task == !!task) %>%
      dplyr::collect() %>%
      as.data.table()
  } else {
    temp <- config$schemas$config_data_hash_for_each_plan$tbl() %>%
      dplyr::collect() %>%
      as.data.table()
  }
  if (!is.null(index_plan)) {
    x_index_plan <- index_plan
    temp <- temp[index_plan == x_index_plan]
  }

  return(temp)
}
