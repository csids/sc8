create_pool_connection <- function(db_config, use_db = TRUE){
  if(db_config$id %in% names(pools) & use_db == TRUE) return()
  use_trusted <- FALSE
  if(!is.null(db_config$trusted_connection)) if(db_config$trusted_connection=="yes") use_trusted <- TRUE

  if(use_trusted & db_config$driver %in% c("ODBC Driver 17 for SQL Server")){
    pool <- pool::dbPool(
      odbc::odbc(),
      driver = db_config$driver,
      server = db_config$server,
      port = db_config$port,
      trusted_connection = "yes"
    )
  } else if(db_config$driver %in% c("ODBC Driver 17 for SQL Server")){
    pool <- pool::dbPool(
      odbc::odbc(),
      driver = db_config$driver,
      server = db_config$server,
      port = db_config$port,
      uid = db_config$user,
      pwd = db_config$password,
      encoding = "utf8"
    )
  } else {
    pool <- pool::dbPool(
      odbc::odbc(),
      driver = db_config$driver,
      server = db_config$server,
      port = db_config$port,
      user = db_config$user,
      password = db_config$password,
      encoding = "utf8"
    )
  }
  if(use_db){
    use_db(pool, db_config$db)
    pools[[db_config$id]] <- pool
  } else {
    pools$no_db <- pool
  }
}

get_pool_id <- function(x){
  prefix <- stringr::str_extract(x, "^[A-Za-z]+_") |>
    stringr::str_remove("_")
  if(is.na(prefix)) prefix <- stringr::str_extract(x, "^[A-Za-z]+")

  if(prefix %in% names(config$db_configs)){
    id <- config$db_configs[[prefix]]$id
  } else {
    id <- config$db_configs[["anon"]]$id
  }
  return(id)
}

get_access_levels_from_id <- function(id){
  access_levels <- c()
  for(i in config$db_configs){
    if(id == i$id){
      access_levels <- c(access_levels, i$access)
    }
  }
  return(access_levels)
}

get_db_and_schema_from_id <- function(id){
  x <- id |>
    stringr::str_split("\\.") |>
    unlist() |>
    stringr::str_remove_all("\\[") |>
    stringr::str_remove_all("]")

  return(
    list(
      db = x[1],
      schema = x[2]
    )
  )

}


get_db_and_schema_from_access_level <- function(access_level){
  for(i in config$db_configs){
    if(access_level == i$access){
      return(
        list(
          db = i$db,
          schema = i$schema
        )
      )
    }
  }
}

get_access_level_from_table_name <- function(table_name){
  access_levels <- lapply(config$db_configs, function(x) x$access) |>
    unique() |>
    unlist()

  retval <- stringr::str_extract(table_name, "^[A-Za-zA-Za-z]+_") |>
    stringr::str_remove("_")
  if(is.na(retval)) return("anon")

  if(retval=="specific"){
    retval <- stringr::str_extract(table_name, "^[A-Za-zA-Za-z]+_") |>
      stringr::str_sub(1, -2)
  }

  if(!retval %in% access_levels) return("anon")
  return(retval)
}

get_fully_specified_table_name <- function(table_name){
  db_schema <- table_name |>
    get_access_level_from_table_name() |>
    get_db_and_schema_from_access_level()

  x_table <- paste0("[", paste(db_schema$db, db_schema$schema, table_name,sep="].["), "]") |>
    stringr::str_remove_all("\\[]\\.")

  return(x_table)
}

get_table_name_info <- function(table_name){
  db_schema <- table_name |>
    get_access_level_from_table_name() |>
    get_db_and_schema_from_access_level()

  x_table <- paste0("[", paste(db_schema$db, db_schema$schema, table_name,sep="].["), "]") |>
    stringr::str_remove_all("\\[]\\.")

  return(list(
    access = get_access_level_from_table_name(table_name),
    db = db_schema$db,
    schema = db_schema$schema,
    table_name_fully_specified = x_table,
    table_name = table_name,
    pool = pools[[get_pool_id(table_name)]]
  ))

}

list_tables_int <- function(id) {
  sql <- glue::glue("select * from [{get_db_and_schema_from_id(id)$db}].information_schema.tables")
  retval <- DBI::dbGetQuery(pools$no_db, sql) |> setDT()
  retval <- retval[TABLE_SCHEMA == get_db_and_schema_from_id(id)$schema]
  retval <- retval$TABLE_NAME
  if(length(retval)==0) return()

  # remove airflow tables
  retval <- retval[
    which(!retval %in% c(
      "alembic_version",
      "chart",
      "connection",
      "dag",
      "dag_pickle",
      "dag_run",
      "dag_tag",
      "import_error",
      "job",
      "known_event",
      "known_event_type",
      "kube_resource_version",
      "kube_worker_uuid",
      "log",
      "serialized_dag",
      "sla_miss",
      "slot_pool",
      "task_fail",
      "task_instance",
      "task_reschedule",
      "users",
      "variable",
      "xcom"
    ))
  ]
  return(sort(retval))
}

#' get_tables
#' @param access_level access_level
#' @export
get_tables <- function(access_level = NULL) {
  ids <- lapply(config$db_configs, function(x) x$id) |>
    unique() |>
    unlist()

  retval <- list()
  for(id in ids){
    access_levels <- get_access_levels_from_id(id)
    for(a in access_levels){
      retval[[a]] <- list_tables_int(id)
    }
  }
  if(!is.null(access_level)) retval <- retval[[access_level]]
  return(invisible(retval))
}

#' list_tables
#' @export
list_tables <- function() {
  retval <- get_tables() |>
    unlist() |>
    unique()

  return(retval)
}

#' list_tables
#' @export
print_tables <- function() {
  ids <- lapply(config$db_configs, function(x) x$id) |>
    unique() |>
    unlist()

  for(id in ids){
    access_levels <- get_access_levels_from_id(id)
    cat("\n----------\n")
    cat(paste0("ID: ", id, "\n"))

    cat("Access level: \n")
    for(i in access_levels) cat(paste0("* ",i),"\n")
    cat("----------\n")

    cat("DB Tables:\n")
    x <- list_tables_int(id)
    print(x, width = 10)
    cat("\n")
  }
}
