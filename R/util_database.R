is_mssql <- function(conn) {
  return(conn@info$dbms.name == "Microsoft SQL Server")
}

#' use_db
#' @param conn a
#' @param db a
#' @export
use_db <- function(conn, db) {
  tryCatch(
    {
      a <- DBI::dbExecute(conn, glue::glue({
        "USE {db};"
      }))
    },
    error = function(e) {
      a <- DBI::dbExecute(conn, glue::glue({
        "CREATE DATABASE {db};"
      }))
      a <- DBI::dbExecute(conn, glue::glue({
        "USE {db};"
      }))
    }
  )
}

set_db_recovery_simple <- function(conn = get_db_connection(), db = config$db_config$db, i_am_sure_i_want_to_do_this = FALSE) {
  stopifnot(i_am_sure_i_want_to_do_this == T)

  a <- DBI::dbExecute(conn, glue::glue({
    "ALTER DATABASE  {db} SET RECOVERY SIMPLE;"
  }))
}

#' get_field_types
#' @param conn a
#' @param dt a
get_field_types <- function(conn, dt) {
  field_types <- vapply(dt, DBI::dbDataType,
    dbObj = conn,
    FUN.VALUE = character(1)
  )
  return(field_types)
}

random_uuid <- function() {
  x <- uuid::UUIDgenerate(F)
  x <- gsub("-", "", x)
  x <- paste0("a", x)
  x
}

random_file <- function(folder, extension = ".csv") {
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  fs::path(folder, paste0(random_uuid(), extension))
}

write_data_infile <- function(dt,
                              file = paste0(tempfile(), ".csv"),
                              colnames = T,
                              eol = "\n",
                              quote = "auto",
                              na = "\\N",
                              sep = ",") {
  # infinites and NANs get written as text
  # which destroys the upload
  # we need to set them to NA
  for (i in names(dt)) {
    dt[is.infinite(get(i)), (i) := NA]
    dt[is.nan(get(i)), (i) := NA]
    if (inherits(dt[[i]], "POSIXt")) dt[, (i) := as.character(get(i))]
  }
  fwrite(dt,
    file = file,
    logical01 = T,
    na = na,
    col.names = colnames,
    eol = eol,
    quote = quote,
    sep = sep
  )
}

load_data_infile <- function(conn,
                             db_config,
                             table,
                             dt,
                             file,
                             force_tablock) {
  UseMethod("load_data_infile")
}

load_data_infile.default <- function(conn = NULL,
                                     db_config = NULL,
                                     table,
                                     dt = NULL,
                                     file = "/xtmp/x123.csv",
                                     force_tablock = FALSE) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  t0 <- Sys.time()


  if (is.null(conn) & is.null(db_config)) {
    stop("conn and db_config both have error")
  } else if (is.null(conn) & !is.null(db_config)) {
    conn <- get_db_connection(db_config = db_config)
    use_db(conn, db_config$db)
    on.exit(DBI::dbDisconnect(conn))
  }

  correct_order <- DBI::dbListFields(conn, table)
  if (length(correct_order) > 0) dt <- dt[, correct_order, with = F]
  write_data_infile(dt = dt, file = file)
  on.exit(unlink(file), add = T)

  sep <- ","
  eol <- "\n"
  quote <- '"'
  skip <- 0
  header <- T
  path <- normalizePath(file, winslash = "/", mustWork = TRUE)

  sql <- paste0(
    "LOAD DATA INFILE ", DBI::dbQuoteString(conn, path), "\n",
    "INTO TABLE ", DBI::dbQuoteIdentifier(conn, table), "\n",
    "CHARACTER SET utf8", "\n",
    "FIELDS TERMINATED BY ", DBI::dbQuoteString(conn, sep), "\n",
    "OPTIONALLY ENCLOSED BY ", DBI::dbQuoteString(conn, quote), "\n",
    "LINES TERMINATED BY ", DBI::dbQuoteString(conn, eol), "\n",
    "IGNORE ", skip + as.integer(header), " LINES \n",
    "(", paste0(correct_order, collapse = ","), ")"
  )
  DBI::dbExecute(conn, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if (config$verbose) message(glue::glue("Uploaded {nrow(dt)} rows in {dif} seconds to {table}"))

  invisible()
}

`load_data_infile.Microsoft SQL Server` <- function(conn = NULL,
                                                    db_config = NULL,
                                                    table,
                                                    dt,
                                                    file = tempfile(),
                                                    force_tablock = FALSE) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }
  if (is.null(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  } else if (!DBI::dbIsValid(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  }

  a <- Sys.time()

  if (is.null(conn) & is.null(db_config)) {
    stop("conn and db_config both have error")
  } else if (is.null(conn) & !is.null(db_config)) {
    conn <- get_db_connection(db_config = db_config)
    use_db(conn, db_config$db)
    on.exit(DBI::dbDisconnect(conn))
  }

  # dont do a validation check if running in parallel
  # because there will be race conditions with different
  # instances competing
  if (!config$in_parallel & interactive()) {
    sql <- glue::glue("SELECT COUNT(*) FROM {table};")
    n_before <- DBI::dbGetQuery(conn, sql)[1, 1]
  }

  correct_order <- DBI::dbListFields(conn, table)
  if (length(correct_order) > 0) dt <- dt[, correct_order, with = F]
  write_data_infile(
    dt = dt,
    file = file,
    colnames = F,
    eol = "\n",
    quote = FALSE,
    na = "",
    sep = "\t"
  )
  on.exit(unlink(file), add = T)

  format_file <- tempfile(tmpdir = tempdir(check = TRUE))
  on.exit(unlink(format_file), add = T)

  args <- c(
    table,
    "format",
    "nul",
    "-q",
    "-c",
    # "-t,",
    "-f",
    format_file,
    "-S",
    db_config$server,
    "-d",
    db_config$db,
    "-U",
    db_config$user,
    "-P",
    db_config$password
  )
  if (db_config$trusted_connection == "yes") {
    args <- c(args, "-T")
  }
  system2(
    "bcp",
    args = args,
    stdout = NULL
  )

  # TABLOCK is used by bcp by default. It is disabled when performing inserts in parallel.
  # Upserts can use TABLOCK in parallel, because they initially insert to a random table
  # before merging. This random db table will therefore not be in use by multiple processes
  # simultaneously
  if (!config$in_parallel | force_tablock) {
    # sometimes this results in the data not being
    # uploaded at all, so for the moment I am disabling this
    # until we can spend more time on it
    # hint_arg <- "TABLOCK"
    hint_arg <- NULL
  } else {
    hint_arg <- NULL
  }

  if (!is.null(key(dt))) {
    hint_arg <- c(hint_arg, paste0("ORDER(", paste0(key(dt), " ASC", collapse = ", "), ")"))
  }
  if (length(hint_arg) > 0) {
    hint_arg <- paste0(hint_arg, collapse = ", ")
    hint_arg <- paste0("-h '", hint_arg, "'")
  }

  args <- c(
    table,
    "in",
    file,
    "-a 16384",
    hint_arg,
    "-S",
    db_config$server,
    "-d",
    db_config$db,
    "-U",
    db_config$user,
    "-P",
    db_config$password,
    "-f",
    format_file,
    "-m",
    0
  )
  if (db_config$trusted_connection == "yes") {
    args <- c(args, "-T")
  }
  # print(args)
  system2(
    "bcp",
    args = args,
    stdout = NULL
  )

  # dont do a validation check if running in parallel
  # because there will be race conditions with different
  # instances competing
  if (!config$in_parallel & interactive()) {
    sql <- glue::glue("SELECT COUNT(*) FROM {table};")
    n_after <- DBI::dbGetQuery(conn, sql)[1, 1]
    n_inserted <- n_after - n_before

    if (n_inserted != nrow(dt)) stop("Wanted to insert ", nrow(dt), " rows but only inserted ", n_inserted)
  }
  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
  if (config$verbose) message(glue::glue("Uploaded {nrow(dt)} rows in {dif} seconds to {table}"))

  update_config_last_updated(type = "data", tag = table)

  invisible()
}

######### upsert_load_data_infile

upsert_load_data_infile <- function(conn = NULL,
                                    db_config,
                                    table,
                                    dt,
                                    file,
                                    fields,
                                    keys,
                                    drop_indexes) {
  if (is.null(conn) & is.null(db_config)) {
    stop("conn and db_config both have error")
  } else if (is.null(conn) & !is.null(db_config)) {
    conn <- get_db_connection(db_config = db_config)
    use_db(conn, db_config$db)
    on.exit(DBI::dbDisconnect(conn))
  }

  upsert_load_data_infile_internal(
    conn = conn,
    db_config = db_config,
    table = table,
    dt = dt,
    file = file,
    fields = fields,
    keys = keys,
    drop_indexes = drop_indexes
  )
}

upsert_load_data_infile_internal <- function(conn,
                                             db_config,
                                             table,
                                             dt,
                                             file,
                                             fields,
                                             keys,
                                             drop_indexes) {
  UseMethod("upsert_load_data_infile_internal")
}

upsert_load_data_infile_internal.default <- function(conn = NULL,
                                                     db_config = NULL,
                                                     table,
                                                     dt,
                                                     file = "/tmp/x123.csv",
                                                     fields,
                                                     keys = NULL,
                                                     drop_indexes = NULL) {
  temp_name <- random_uuid()
  # ensure that the table is removed **FIRST** (before deleting the connection)
  on.exit(DBI::dbRemoveTable(conn, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("CREATE TEMPORARY TABLE {temp_name} LIKE {table};")
  DBI::dbExecute(conn, sql)

  # TO SPEED UP EFFICIENCY DROP ALL INDEXES HERE
  if (!is.null(drop_indexes)) {
    for (i in drop_indexes) {
      try(
        DBI::dbExecute(
          conn,
          glue::glue("ALTER TABLE `{temp_name}` DROP INDEX `{i}`")
        ),
        TRUE
      )
    }
  }

  load_data_infile(
    conn = conn,
    db_config = db_config,
    table = temp_name,
    dt = dt,
    file = file
  )

  t0 <- Sys.time()

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql <- glue::glue("
    INSERT INTO {table} SELECT {vals_fields} FROM {temp_name}
    ON DUPLICATE KEY UPDATE {vals};
    ")
  DBI::dbExecute(conn, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if (config$verbose) message(glue::glue("Upserted {nrow(dt)} rows in {dif} seconds from {temp_name} to {table}"))

  invisible()
}

`upsert_load_data_infile_internal.Microsoft SQL Server` <- function(conn = NULL,
                                                                    db_config = NULL,
                                                                    table,
                                                                    dt,
                                                                    file = tempfile(),
                                                                    fields,
                                                                    keys,
                                                                    drop_indexes = NULL) {
  # conn <- schema$output$conn
  # db_config <- config$db_config
  # table <- schema$output$db_table
  # dt <- data_clean
  # file <- tempfile()
  # fields <- schema$output$db_fields
  # keys <- schema$output$keys
  # drop_indexes <- NULL
  if (is.null(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  } else if (!DBI::dbIsValid(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  }

  temp_name <- paste0("tmp", random_uuid())

  # ensure that the table is removed **FIRST** (before deleting the connection)
  on.exit(DBI::dbRemoveTable(conn, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE 1 = 0;")
  DBI::dbExecute(conn, sql)

  load_data_infile(
    conn = conn,
    db_config = db_config,
    table = temp_name,
    dt = dt,
    file = file,
    force_tablock = TRUE
  )

  a <- Sys.time()
  add_index(
    conn = conn,
    table = temp_name,
    keys = keys
  )

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql_on_keys <- glue::glue("{t} = {s}", t = paste0("t.", keys), s = paste0("s.", keys))
  sql_on_keys <- paste0(sql_on_keys, collapse = " and ")

  sql_update_set <- glue::glue("{t} = {s}", t = paste0("t.", fields), s = paste0("s.", fields))
  sql_update_set <- paste0(sql_update_set, collapse = ", ")

  sql_insert_fields <- paste0(fields, collapse = ", ")
  sql_insert_s_fields <- paste0(paste0("s.", fields), collapse = ", ")

  sql <- glue::glue("
  MERGE {table} t
  USING {temp_name} s
  ON ({sql_on_keys})
  WHEN MATCHED
  THEN UPDATE SET
    {sql_update_set}
  WHEN NOT MATCHED BY TARGET
  THEN INSERT ({sql_insert_fields})
    VALUES ({sql_insert_s_fields});
  ")

  DBI::dbExecute(conn, sql)

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
  if (config$verbose) message(glue::glue("Upserted {nrow(dt)} rows in {dif} seconds from {temp_name} to {table}"))

  update_config_last_updated(type = "data", tag = table)
  invisible()
}

######### create_table
create_table <- function(conn, table, fields, keys) UseMethod("create_table")

create_table.default <- function(conn, table, fields, keys = NULL) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "TEXT CHARACTER SET utf8 COLLATE utf8_unicode_ci"

  sql <- DBI::sqlCreateTable(conn, table, fields_new,
    row.names = F, temporary = F
  )
  DBI::dbExecute(conn, sql)
}

`create_table.Microsoft SQL Server` <- function(conn, table, fields, keys = NULL) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "NVARCHAR (1000)"
  fields_new[fields == "DOUBLE"] <- "FLOAT"
  fields_new[fields == "BOOLEAN"] <- "BIT"

  if (!is.null(keys)) fields_new[names(fields_new) %in% keys] <- paste0(fields_new[names(fields_new) %in% keys], " NOT NULL")

  sql <- DBI::sqlCreateTable(
    conn,
    table,
    fields_new,
    row.names = F,
    temporary = F
  ) |>
    stringr::str_replace("\\\\", "\\") |>
    stringr::str_replace("\"", "") |>
    stringr::str_replace("\"", "")
  DBI::dbExecute(conn, sql)
}

######### add_constraint
add_constraint <- function(conn, table, keys) UseMethod("add_constraint")

add_constraint.default <- function(conn, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- glue::glue("PK_{table}")
  sql <- glue::glue("
          ALTER table {table}
          ADD CONSTRAINT {constraint} PRIMARY KEY CLUSTERED ({primary_keys});")
  # print(sql)
  a <- DBI::dbExecute(conn, sql)
  # DBI::dbExecute(conn, "SHOW INDEX FROM x");
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if (config$verbose) message(glue::glue("Added constraint {constraint} in {dif} seconds to {table}"))
}

######### drop_constraint
drop_constraint <- function(conn, table) UseMethod("drop_constraint")

drop_constraint.default <- function(conn, table) {
  constraint <- glue::glue("PK_{table}")
  sql <- glue::glue("
          ALTER table {table}
          DROP CONSTRAINT {constraint};")
  # print(sql)
  try(a <- DBI::dbExecute(conn, sql), TRUE)
}

drop_index <- function(conn, table, index) UseMethod("drop_index")

drop_index.default <- function(conn, table, index) {
  try(
    DBI::dbExecute(
      conn,
      glue::glue("ALTER TABLE `{table}` DROP INDEX `{index}`")
    ),
    TRUE
  )
}

`drop_index.Microsoft SQL Server` <- function(conn, table, index) {
  try(
    DBI::dbExecute(
      conn,
      glue::glue("DROP INDEX {table}.{index}")
    ),
    TRUE
  )
}

add_index <- function(conn, table, index, keys) UseMethod("add_index")

add_index.default <- function(conn, table, keys, index) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  sql <- glue::glue("
    ALTER TABLE `{table}` ADD INDEX `{index}` ({keys})
    ;")
  # print(sql)
  try(a <- DBI::dbExecute(conn, sql), T)
}

`add_index.Microsoft SQL Server` <- function(conn, table, keys, index) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  try(
    DBI::dbExecute(
      conn,
      glue::glue("CREATE INDEX {index} ON {table} ({keys});")
    ),
    T
  )
}

#' Keeps the rows where the condition is met
#' @param conn A db connection
#' @param table_from Table name
#' @param table_to Table name
#' @param condition A string SQL condition
#' @param columns The columns to be copied
#' @export
copy_into_new_table_where <- function(conn = NULL,
                                      table_from,
                                      table_to,
                                      condition = "1=1",
                                      columns = "*") {
  if (is.null(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  } else if (!DBI::dbIsValid(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  }

  info_from <- get_table_name_info(table_from)
  info_to <- get_table_name_info(table_to)

  if (info_from$db != info_to$db) {
    stop("Cannot copy directly between databases because they are of different levels of data sensitivity.")
  }

  t0 <- Sys.time()
  temp_name <- paste0("[", info_to$db, "].[", info_to$schema, "].[tmp", random_uuid(), "]")

  # create the table first (needs to be created or we cant use tablock)
  sql <- glue::glue("SELECT {columns} INTO {temp_name} FROM {table_from} WHERE 0=1")
  DBI::dbExecute(info_from$pool, sql)
  # then insert (using tablock to make it faster)
  sql <- glue::glue("INSERT INTO {temp_name} WITH (tablock) SELECT {columns} FROM {table_from} WHERE {condition}")
  DBI::dbExecute(info_from$pool, sql)

  try(DBI::dbRemoveTable(info_from$pool, name = table_to), TRUE)

  sql <- glue::glue("EXEC sp_rename '{temp_name}', '{table_to}'")
  DBI::dbExecute(info_from$pool, sql)
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if (config$verbose) message(glue::glue("Copied rows in {dif} seconds from {table_from} to {table_to}"))

  # applying indexes if possible
  if (table_from %in% names(sc8::config$schemas)) {
    for (i in names(sc8::config$schemas[[table_from]]$indexes)) {
      if (config$verbose) message(glue::glue("Adding index {i}"))

      add_index(
        conn = conn,
        table = table_to,
        index = i,
        keys = sc8::config$schemas[[table_from]]$indexes[[i]]
      )
    }
  }

  update_config_last_updated(type = "data", tag = table_to)
}

#' drop_all_rows
#' Drops all rows
#' @param conn A db connection
#' @param table Table name
#' @export
drop_all_rows <- function(conn = NULL, table) {
  if (is.null(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  } else if (!DBI::dbIsValid(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  }

  info <- get_table_name_info(table)

  a <- DBI::dbExecute(conn, glue::glue({
    "TRUNCATE TABLE {table};"
  }))

  update_config_last_updated(type = "data", tag = table)
}

#' Drops the rows where the condition is met
#' @param conn A db connection
#' @param table Table name
#' @param condition A string SQL condition
#' @export
drop_rows_where <- function(conn = NULL, table, condition) {
  if (is.null(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  }
  t0 <- Sys.time()

  # find out how many rows to delete
  numrows <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT COUNT(*) FROM {table} WHERE {condition};"
  )) %>%
    as.numeric()
  message(numrows, " rows remaining to be deleted")

  num_deleting <- 100000
  # need to do this, so that we dont get scientific format in the SQL command
  num_deleting_character <- formatC(num_deleting, format = "f", drop0trailing = T)
  num_delete_calls <- ceiling(numrows / num_deleting)
  message("We will need to perform ", num_delete_calls, " delete calls of ", num_deleting_character, " rows each.")

  indexes <- splutil::easy_split(1:num_delete_calls, number_of_groups = 10)
  notify_indexes <- unlist(lapply(indexes, max))

  i <- 0
  while (numrows > 0) {

    # delete a large number of rows
    # database must be in SIMPLE recovery mode
    # "ALTER DATABASE sykdomspulsen_surv SET RECOVERY SIMPLE;"
    # checkpointing will ensure transcation log is cleared after each delete operation
    # http://craftydba.com/?p=3079
    #
    #

    b <- DBI::dbExecute(conn, glue::glue(
      "DELETE TOP ({num_deleting_character}) FROM {table} WHERE {condition}; ",
      "CHECKPOINT; "
    ))

    numrows <- DBI::dbGetQuery(conn, glue::glue(
      "SELECT COUNT(*) FROM {table} WHERE {condition};"
    )) %>%
      as.numeric()
    i <- i + 1
    if (i %in% notify_indexes) message(i, "/", num_delete_calls, " delete calls performed. ", numrows, " rows remaining to be deleted")
  }

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if (config$verbose) message(glue::glue("Deleted rows in {dif} seconds from {table}"))

  update_config_last_updated(type = "data", tag = table)
}

#' keep_rows_where
#' Keeps the rows where the condition is met
#' @param conn A db connection
#' @param table Table name
#' @param condition A string SQL condition
#' @export
keep_rows_where <- function(conn = NULL, table, condition) {
  if (is.null(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  } else if (!DBI::dbIsValid(conn)) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn))
  }
  t0 <- Sys.time()
  temp_name <- paste0("tmp", random_uuid())

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE {condition}")
  DBI::dbExecute(conn, sql)

  DBI::dbRemoveTable(conn, name = table)

  sql <- glue::glue("EXEC sp_rename '{temp_name}', '{table}'")
  DBI::dbExecute(conn, sql)
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if (config$verbose) message(glue::glue("Kept rows in {dif} seconds from {table}"))

  update_config_last_updated(type = "data", tag = table)
}




#' get_db_connection
#' @param driver driver
#' @param server server
#' @param port port
#' @param user user
#' @param password password
#' @param db database
#' @param trusted_connection trusted connection yes/no
#' @param db_config A list containing driver, server, port, user, password
#' @export get_db_connection
get_db_connection <- function(driver = NULL,
                              server = NULL,
                              port = NULL,
                              user = NULL,
                              password = NULL,
                              db = NULL,
                              trusted_connection = NULL,
                              db_config = config$db_config) {
  if (!is.null(db_config) & is.null(driver)) {
    driver <- db_config$driver
  }
  if (!is.null(db_config) & is.null(server)) {
    server <- db_config$server
  }
  if (!is.null(db_config) & is.null(port)) {
    port <- db_config$port
  }
  if (!is.null(db_config) & is.null(user)) {
    user <- db_config$user
  }
  if (!is.null(db_config) & is.null(password)) {
    password <- db_config$password
  }
  if (!is.null(db_config) & is.null(db)) {
    db <- db_config$db
  }

  if (!is.null(db_config) & is.null(trusted_connection)) {
    trusted_connection <- db_config$trusted_connection
  }

  use_trusted <- FALSE
  if (!is.null(trusted_connection)) if (trusted_connection == "yes") use_trusted <- TRUE

  if (use_trusted & driver %in% c("ODBC Driver 17 for SQL Server")) {
    conn <- DBI::dbConnect(
      odbc::odbc(),
      driver = driver,
      server = server,
      port = port,
      trusted_connection = "yes"
    )
  } else if (driver %in% c("ODBC Driver 17 for SQL Server")) {
    conn <- DBI::dbConnect(
      odbc::odbc(),
      driver = driver,
      server = server,
      port = port,
      uid = user,
      pwd = password,
      encoding = "utf8"
    )
  } else {
    conn <- DBI::dbConnect(
      odbc::odbc(),
      driver = driver,
      server = server,
      port = port,
      user = user,
      password = password,
      encoding = "utf8"
    )
  }
  if (!is.null(db)) use_db(conn, db)
  return(conn)
}

#' tbl
#' @param table table
#' @export
tbl <- function(table) {
  x <- get_table_name_info(table)

  if (!DBI::dbIsValid(x$pool) | config$in_parallel) {
    # message("sc8::tbl connection was not valid, or is being run in parallel. Recreating.")
    create_pool_connection(config$db_configs[[x$access]], use_db = T)
    x <- get_table_name_info(table)
  } else {
    # message("sc8::tbl connection is valid.")
  }

  return(dplyr::tbl(x$pool, x$table_name))
}

#' list_indexes
#' @param table tbl
#' @param conn conn
#' @export
list_indexes <- function(table, conn = NULL) {
  if (is.null(conn)) {
    db <- config$db_config$db
    if (is.null(connections[[db]])) {
      connections[[db]] <- get_db_connection(db = db)
      use_db(connections[[db]], db)
      conn <- connections
    }
  }
  retval <- DBI::dbGetQuery(
    conn,
    glue::glue("select * from sys.indexes where object_id = (select object_id from sys.objects where name = '{table}')")
  )
  return(retval)
}




#' drop_table
#' @param table table
#' @export
drop_table <- function(table) {
  id <- get_pool_id(table)
  return(try(DBI::dbRemoveTable(pools[[id]], name = table), TRUE))
}
