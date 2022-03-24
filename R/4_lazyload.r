get_db_acess_from_env <- function() {
  retval <- Sys.getenv("SYKDOMSPULSEN_DB_CONFIG_ACCESS") |>
    stringr::str_split("/") |>
    unlist()
  retval <- retval[retval != ""]
  return(retval)
}

get_db_from_env <- function(access = NULL) {
  if (is.null(access)) {
    retval <- list(
      access = "anon",
      driver = Sys.getenv("SYKDOMSPULSEN_DB_DRIVER"),
      server = Sys.getenv("SYKDOMSPULSEN_DB_SERVER"),
      port = as.integer(Sys.getenv("SYKDOMSPULSEN_DB_PORT")),
      user = Sys.getenv("SYKDOMSPULSEN_DB_USER"),
      password = Sys.getenv("SYKDOMSPULSEN_DB_PASSWORD"),
      db = Sys.getenv("SYKDOMSPULSEN_DB_DB"),
      trusted_connection = Sys.getenv("SYKDOMSPULSEN_DB_TRUSTED_CONNECTION"),
      schema = "dbo"
    )
  } else {
    retval <- list(
      access = access,
      driver = Sys.getenv("SYKDOMSPULSEN_DB_CONFIG_DRIVER"),
      port = as.integer(Sys.getenv("SYKDOMSPULSEN_DB_CONFIG_PORT")),
      user = Sys.getenv("SYKDOMSPULSEN_DB_CONFIG_USER"),
      password = Sys.getenv("SYKDOMSPULSEN_DB_CONFIG_PASSWORD"),
      trusted_connection = Sys.getenv("SYKDOMSPULSEN_DB_CONFIG_TRUSTED_CONNECTION"),
      server = Sys.getenv("SYKDOMSPULSEN_DB_CONFIG_SERVER"),
      schema = Sys.getenv(paste0("SYKDOMSPULSEN_DB_CONFIG_SCHEMA_", toupper(access))),
      db = Sys.getenv(paste0("SYKDOMSPULSEN_DB_CONFIG_DB_", toupper(access)))
    )
  }
  # retval <- config$db_config
  retval$schema <- gsub("\\\\", "\\\\", retval$schema)

  retval$id <- paste0("[", retval$db, "].[", retval$schema, "]")

  return(retval)
}

set_db <- function() {
  config$db_config <- get_db_from_env()

  config$db_configs <- list()
  for (i in get_db_acess_from_env()) {
    config$db_configs[[i]] <- get_db_from_env(i)
  }
  if (is.null(config$db_configs$config) & !is.null(config$db_configs$anon)) {
    config$db_configs$config <- config$db_configs$anon
    config$db_configs$config$access <- "config"
  }
  if (is.null(config$db_configs$restr)) {
    config$db_configs$restr <- config$db_config
    config$db_configs$restr$access <- "restr"
  }
  if (is.null(config$db_configs$anon)) config$db_configs$anon <- config$db_config
  if (is.null(config$db_configs$config)) {
    config$db_configs$config <- config$db_config
    config$db_configs$config$access <- "config"
  }

  # make sure that legacy db schemas save to anon
  config$db_config <- config$db_configs$anon

  create_pool_connection(config$db_configs$anon, use_db = FALSE)

  # which db is preferred?
  sql <- glue::glue("SELECT name, has_dbaccess(name) FROM sys.databases")
  db_names <- DBI::dbGetQuery(pools$no_db, sql)
  names(db_names) <- c("name", "access")
  db_names <- db_names$name[db_names$access == 1]
  if (config$db_configs$restr$db %in% db_names) {
    config$db_config_preferred <- "restr"
  } else {
    config$db_config_preferred <- "anon"
  }

  # create pool connections for all dbs
  for (i in config$db_configs) try(create_pool_connection(i, use_db = TRUE), silent = T)

  packageStartupMessage("Preferred sc database access level: ", config$db_config_preferred)

  # config_last_updated ----
  add_schema_v8(
    name_access = c("config"),
    name_grouping = "last_updated",
    name_variant = NULL,
    db_configs = config$db_configs,
    field_types = c(
      "type" = "TEXT",
      "tag" = "TEXT",
      "date" = "DATE",
      "datetime" = "DATETIME"
    ),
    keys = c(
      "type",
      "tag"
    ),
    censors = list(
      config = list()
    ),
    validator_field_types = validator_field_types_blank,
    validator_field_contents = validator_field_contents_blank,
    info = "This db table is used for..."
  )

  # config_structure_time ----
  add_schema(
    schema = Schema$new(
      db_config = config$db_config,
      db_table = "config_structure_time",
      db_field_types = c(
        "granularity_time" = "TEXT",
        "isoyear" = "TEXT",
        "isoyearweek" = "TEXT",
        "date" = "DATE"
      ),
      db_load_folder = tempdir(check = T),
      keys = c(
        "type",
        "tag"
      )
    )
  )

  # config_data_hash_for_each_plan ----
  add_schema_v8(
    name_access = c("config"),
    name_grouping = "data_hash",
    name_variant = "for_each_plan",
    db_configs = config$db_configs,
    field_types = c(
      "task" = "TEXT",
      "index_plan" = "INTEGER",
      "date" = "DATE",
      "datetime" = "DATETIME",
      "hash" = "TEXT"
    ),
    keys = c(
      "task",
      "index_plan",
      "date",
      "datetime"
    ),
    censors = list(
      config = list()
    ),
    validator_field_types = validator_field_types_blank,
    validator_field_contents = validator_field_contents_blank,
    info = "This db table is used for..."
  )
}
