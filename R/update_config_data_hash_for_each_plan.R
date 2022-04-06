update_config_data_hash_for_each_plan <- function(task, index_plan, element_tag, all_hash = NULL, element_hash = NULL, date = NULL, datetime = NULL) {
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

  if(is.null(all_hash)) all_hash <- "NULL"
  if(is.null(element_hash)) element_hash <- "NULL"

  to_upload <- data.table(
    "task" = task,
    "index_plan" = index_plan,
    "element_tag" = element_tag,
    "date" = date,
    "datetime" = datetime,
    "all_hash" = all_hash,
    "element_hash" = element_hash
  )
  config$schemas$config_data_hash_for_each_plan$upsert_data(to_upload)
}

#' get_config_data_hash_for_each_plan
#' Gets the config_data_hash_for_each_plan db table
#' @param task a
#' @param index_plan a
#' @param element_tag a
#' @export
get_config_data_hash_for_each_plan <- function(task = NULL, index_plan = NULL, element_tag = NULL) {
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
  if (!is.null(element_tag)) {
    x_element_tag <- element_tag
    temp <- temp[element_tag == x_element_tag]
  }

  return(temp)
}

# this uses get_config_data_hash_for_each_plan to put it into plnr format
# i.e. hash$last_run and hash$last_run_elements$blah
get_last_run_data_hash_split_into_plnr_format <- function(task, index_plan){
  hash <- get_config_data_hash_for_each_plan(task = task_name, index_plan = index_plan)
  retval <- list()
  if(nrow(hash)==0){
    retval$last_run <- as.character(runif(1))
    retval$last_run_elements <- list()
  } else {
    hash <- hash[datetime==max(datetime)]
    retval$last_run <- hash$all_hash[1]
    retval$last_run_elements <- list()
    for(i in seq_len(nrow(hash$all_hash))){
      retval$last_run_elements[[hash$element_tag[i]]] <- hash$element_hash[i]
    }
  }
  return(retval)
}
