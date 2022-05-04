#' Hashing data structure (Schema_v8)
#'
#' An implementation of spltidy::identify_data_structure for Schema_v8
#' @param x A Schema_v8 object
#' @param col The column to be hashed
#' @param ... Unused
#' @importFrom spltidy identify_data_structure
#' @method identify_data_structure Schema_v8
#' @export
identify_data_structure.Schema_v8 <- function(x, col, ...) {
  spltidy::identify_data_structure(x$tbl(), col)
}

#' shortcut to get available schema names
#' @export
tm_get_schema_names <- function() {
  config$schemas |>
    names()
}

#' Blank field_types validator
#' @param db_field_types db_field_types passed to schema
#' @export
validator_field_types_blank <- function(db_field_types) {
  return(TRUE)
}

#' Blank data validator
#' @param data data passed to schema
#' @export
validator_field_contents_blank <- function(data) {
  return(TRUE)
}

#' validator_field_types_sykdomspulsen
#' An example (schema) validator of field_types used in Sykdomspulsen
#' @param db_field_types db_field_types passed to schema
#' @export
validator_field_types_sykdomspulsen <- function(db_field_types) {
  if (!inherits(db_field_types, "character")) {
    return(FALSE)
  }
  if (!length(db_field_types) >= 12) {
    return(FALSE)
  }
  if (!(
    # identical(
    #   db_field_types[1:16],
    #   c(
    #     "granularity_time" = "TEXT",
    #     "granularity_geo" = "TEXT",
    #     "country_iso3" = "TEXT",
    #     "location_code" = "TEXT",
    #     "border" = "INTEGER",
    #     "age" = "TEXT",
    #     "sex" = "TEXT",
    #
    #     "date" = "DATE",
    #
    #     "isoyear" = "INTEGER",
    #     "isoweek" = "INTEGER",
    #     "isoyearweek" = "TEXT",
    #     "season" = "TEXT",
    #     "seasonweek" = "DOUBLE",
    #
    #     "calyear" = "INTEGER",
    #     "calmonth" = "INTEGER",
    #     "calyearmonth" = "TEXT"
    #   )
    # ) |
    identical(
      db_field_types[1:12],
      c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "country_iso3" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "date" = "DATE",
        "isoyear" = "INTEGER",
        "isoweek" = "INTEGER",
        "isoyearweek" = "TEXT",
        "season" = "TEXT"
      )
    ) |
      identical(
        db_field_types[1:12],
        c(
          "granularity_time" = "TEXT",
          "granularity_geo" = "TEXT",
          "location_code" = "TEXT",
          "border" = "INTEGER",
          "age" = "TEXT",
          "sex" = "TEXT",
          "isoyear" = "INTEGER",
          "isoweek" = "INTEGER",
          "isoyearweek" = "TEXT",
          "season" = "TEXT",
          "seasonweek" = "DOUBLE",
          "date" = "DATE"
        )
      ) |
      identical(
        db_field_types[1:12],
        c(
          "granularity_time" = "TEXT",
          "granularity_geo" = "TEXT",
          "location_code" = "TEXT",
          "border" = "INTEGER",
          "age" = "TEXT",
          "sex" = "TEXT",
          "year" = "INTEGER",
          "week" = "INTEGER",
          "yrwk" = "TEXT",
          "season" = "TEXT",
          "x" = "DOUBLE",
          "date" = "DATE"
        )
      )
  )) {
    return(FALSE)
  }

  return(TRUE)
}

#' validator_field_contents_sykdomspulsen
#' An example (schema) validator of database data used in Sykdomspulsen
#' @param data data passed to schema
#' @export
validator_field_contents_sykdomspulsen <- function(data) {
  for (i in unique(data$granularity_time)) {
    if (sum(stringr::str_detect(
      i,
      c(
        "total",
        "isoyear",
        "calyear",
        "year",
        "season",
        "month",
        "isoweek",
        "week",
        "day",
        "hour",
        "minute",
        "^event"
      )
    )) == 0) {
      retval <- FALSE
      attr(retval, "var") <- "granularity_time"
      return(retval)
    }
  }

  if (sum(!unique(data$granularity_geo) %in% c(
    "nation",
    "region",
    "hospitaldistrict",
    "county",
    "municip",
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
    "ward",
    "station",
    "baregion",
    "missingcounty",
    "missingmunicip",
    "notmainlandcounty",
    "notmainlandmunicip",
    "lab"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "granularity_geo"
    return(retval)
  }

  if (sum(!unique(data$border) %in% c(
    "2020"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "border"
    return(retval)
  }

  if (sum(!unique(data$sex) %in% c(
    "male",
    "female",
    "total"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "sex"
    return(retval)
  }

  if (!inherits(data$date, "Date")) {
    retval <- FALSE
    attr(retval, "var") <- "date"
    return(retval)
  }

  return(TRUE)
}

# schema_v8 ----
#' R6 Class representing a DB schema/table
#'
#' @description
#' The fundamental way to communicate with database tables.
#'
#' @details
#' This class is a representation of a database table. It is the way that you can
#' access data (e.g. `tbl()`), manipulate data (e.g. `insert_data`, `upsert_data`),
#' and manipulate structural aspects of the database table (e.g. `add_indexes`, `drop_indexes`).
#'
#' @import data.table
#' @import R6
#' @export Schema_v8
Schema_v8 <- R6Class(
  "Schema_v8",

  # public ----
  public = list(
    #' @field conn Database connection.
    conn = NULL,
    #' @field db_config Configuration details of the database.
    db_config = NULL,
    #' @field table_name Name of the table in the database.
    table_name = NULL,
    #' @field table_name_fully_specified Fully specified name of the table in the database (e.g. \[db\].\[dbo\].\[table_name\]).
    table_name_fully_specified = NULL,
    #' @field field_types The types of each column in the database table (INTEGER, DOUBLE, TEXT, BOOLEAN, DATE, DATETIME).
    field_types = NULL,
    #' @field field_types_with_length The same as \code{field_types} but with \code{(100)} added to the end of all TEXT fields.
    field_types_with_length = NULL,
    #' @field keys The combination of variables that uniquely identify each row in the database.
    keys = NULL,
    #' @field keys_with_length The same as \code{keys} but with \code{(100)} added to the end of all TEXT fields.
    keys_with_length = NULL,
    #' @field indexes A named list of vectors (generally "ind1", "ind2", etc.) that improves the speed of data retrieval operations on a database table.
    indexes = NULL,
    #' @field validator_field_contents A function that validates the data before it is inserted into the database.
    validator_field_contents = NULL,
    #' @field info Free text information about the DB schema.
    info = "No information given in schema definition",
    #' @field load_folder A temporary folder that is used to write data to before inserting into the database.
    load_folder = tempdir(check = T),
    #' @field censors A named list of censors.
    censors = NULL,

    #' @description
    #' Create a new Schema_v8 object.
    #'
    #' This function is not used directly. You should use \code{\link{add_schema_v8}} instead.
    #' @param conn Database connection (generally leave empty).
    #' @param db_config Configuration details of the database.
    #' @param table_name Name of the table in the database.
    #' @param field_types The types of each column in the database table (INTEGER, DOUBLE, TEXT, BOOLEAN, DATE, DATETIME).
    #' @param censors censors A named list of censors.
    #' @param indexes A named list of vectors (generally "ind1", "ind2", etc.) that improves the speed of data retrieval operations on a database table.
    #' @param validator_field_types A function that validates the \code{field_types} before the DB schema is created.
    #' @param validator_field_contents A function that validates the data before it is inserted into the database.
    #' @return A new `Schema_v8` object.
    initialize = function(conn = NULL,
                          db_config = NULL,
                          table_name,
                          field_types,
                          keys,
                          censors = NULL,
                          indexes = NULL,
                          validator_field_types = validator_field_types_blank,
                          validator_field_contents = validator_field_contents_blank,
                          info = NULL) {
      force(conn)
      self$conn <- conn

      force(db_config)
      self$db_config <- db_config

      force(table_name)
      self$table_name <- table_name

      self$table_name_fully_specified <- paste0("[", paste(db_config$db, db_config$schema, table_name, sep = "].["), "]") |>
        stringr::str_remove_all("\\[]\\.")

      force(field_types)
      self$field_types <- field_types
      self$field_types_with_length <- field_types

      force(keys)
      self$keys <- keys
      self$keys_with_length <- keys

      force(censors)
      self$censors <- censors

      force(indexes)
      self$indexes <- indexes

      # validators
      if (!is.null(validator_field_types)) if (!validator_field_types(self$field_types)) stop(glue::glue("field_types not validated in {table_name}"))
      self$validator_field_contents <- validator_field_contents

      # info
      if (!is.null(info)) self$info <- info

      # db_field_types_with_lengths
      ind <- self$field_types == "TEXT"
      ind_text_with_specific_length <- stringr::str_detect(self$field_types, "TEXT")
      ind_text_with_specific_length[ind] <- FALSE
      if (sum(ind) > 0) {
        self$field_types_with_length[ind] <- paste0(self$field_types_with_length[ind], " (100)")
      }
      if (sum(ind_text_with_specific_length) > 0) {
        lengths <- stringr::str_extract(self$field_types[ind_text_with_specific_length], "\\([0-9]*\\)")
        self$field_types_with_length[ind_text_with_specific_length] <- paste0(self$field_types_with_length[ind_text_with_specific_length], " ", lengths)
      }

      # remove numbers from field_types
      naming <- names(self$field_types)
      self$field_types <- stringr::str_remove(self$field_types, " \\([0-9]*\\)")
      names(self$field_types) <- naming
      # fixing indexes
      self$keys_with_length <- self$field_types_with_length[self$keys]
      if (!is.null(self$conn)) self$create_table()
    },

    #' @field load_folder_fn A function that generates \code{load_folder}.
    load_folder_fn = function() tempdir(check = T),

    #' @description
    #' Class-specific print function.
    print = function(...) {
      needs_to_connect <- FALSE
      if (is.null(self$conn)) {
        needs_to_connect <- TRUE
      } else if (!DBI::dbIsValid(self$conn)) {
        needs_to_connect <- TRUE
      }

      if (needs_to_connect) {
        cat(self$table_name_fully_specified, " ", crayon::bgRed(crayon::white("(disconnected)\n\n")))
        for (i in seq_along(self$field_types)) {
          cat(names(self$field_types)[i], " (", self$field_types[i], ")", "\n", sep = "")
        }
      } else {
        cat(self$table_name_fully_specified, " ", crayon::bgCyan(crayon::white("(connected)\n\n")))
        for (i in seq_along(self$field_types)) {
          var <- names(self$field_types)[i]
          details <- ""
          if (
            var %in% c(
              "granularity_time",
              "granularity_geo",
              "country_iso3",
              # "location_code",
              "border",
              "age",
              "sex",
              "isoyear",
              # "isoweek",
              # "isoyearweek",
              "season",
              "tag",
              "type"
            ) |
              stringr::str_detect(var, "^tag_") |
            stringr::str_detect(var, "^_tag$") |
            stringr::str_detect(var, "^_status$") |
            stringr::str_detect(var, "^_forecast$")
          ) {
            # config$schemas$config_last_updated$connect(); var <- "tag";  details <- dplyr::tbl(config$schemas$config_last_updated$conn, config$schemas$config_last_updated$table_name) |>  dplyr::select(val = !!var) |> dplyr::group_by(val) |> dplyr::summarize(n = n()) |>  dplyr::distinct() |> dplyr::collect() |> setDT() |> setorder(val)
            # library(sykdomspulsen); sc::config$schemas$anon_covid19_autoreport_vaccination_by_time_age_sex_location_data$connect(); sc::config$schemas$anon_covid19_autoreport_vaccination_by_time_age_sex_location_data
            details <- dplyr::tbl(self$conn, self$table_name) |>
              dplyr::select(val = !!var) |>
              dplyr::group_by(val) |>
              dplyr::summarize(n = n()) |>
              dplyr::collect() |>
              setDT()

            # manually specify some ordering requirements
            levels <- sort(details$val)
            extra_levels <- c(
              "nation",
              "county",
              "notmainlandcounty",
              "missingcounty",
              "municip",
              "notmainlandmunicip",
              "missingmunicip",
              "wardoslo",
              "wardbergen",
              "wardstavanger",
              "wardtrondheim",
              "extrawardoslo",
              "missingwardbergen",
              "missingwardoslo",
              "missingwardstavanger",
              "missingwardtrondheim",
              "baregion",
              "region",
              "faregion"
            )
            reordered_levels <- unique(c(extra_levels, levels))
            reordered_levels <- reordered_levels[reordered_levels %in% levels]
            details[, val := factor(val, levels = reordered_levels)]
            setorder(details, val)

            # create display (n + padding)
            details[, len := stringr::str_length(val)]
            details[, max_len := max(len)]
            details[, val := stringr::str_pad(val, max_len, side = "right")]

            details[, n := fhiplot::format_nor(n)]
            details[, len := stringr::str_length(n)]
            details[, max_len := max(len)]
            details[, n := stringr::str_pad(n, max_len, side = "left")]

            details[, display := paste0(val, " (n = ", n, ")")]
            details <- details$display

            for (j in seq_along(details)) details[j] <- paste0("\n\t- ", paste0(details[j], collapse = ""))
            details <- paste0(details, collapse = "")
            details <- paste0(":", details)
          }
          cat(names(self$field_types)[i], " (", self$field_types[i], ")", details, "\n", sep = "")
        }
      }
      cat("\n")

      invisible(self)
    },

    #' @description
    #' Displays the status (connected/disconnected)
    cat_status = function() {
      if (self$is_connected()) {
        cat(self$table_name_fully_specified, " ", crayon::bgRed(crayon::white("(disconnected)")))
      } else {
        cat(self$table_name_fully_specified, " ", crayon::bgCyan(crayon::white("(connected)")))
      }
    },

    #' @description
    #' Is the DB schema connected?
    #' @return TRUE/FALSE
    is_connected = function() {
      is_connected <- FALSE
      if (is.null(self$conn)) {
        is_connected <- TRUE
      } else if (!DBI::dbIsValid(self$conn)) {
        is_connected <- TRUE
      }
      return(is_connected)
    },

    #' @description
    #' Connect to the database
    #' @param db_config db_config
    connect = function(db_config = self$db_config) {
      needs_to_connect <- FALSE
      if (is.null(self$conn)) {
        needs_to_connect <- TRUE
      } else if (!DBI::dbIsValid(self$conn)) {
        needs_to_connect <- TRUE
      }
      if (needs_to_connect) {
        self$conn <- get_db_connection(db_config = db_config)
        use_db(self$conn, db_config$db)
        self$create_table()
      }
    },

    #' @description
    #' Disconnect from the database
    disconnect = function() {
      if (!is.null(self$conn)) {
        if (DBI::dbIsValid(self$conn)) {
          DBI::dbDisconnect(self$conn)
        }
      }
    },

    #' @description
    #' Create the database table
    create_table = function() {
      self$connect()
      create_tab <- TRUE
      if (DBI::dbExistsTable(self$conn, self$table_name)) {
        if (!private$check_fields_match()) {
          message(glue::glue("Dropping table {self$table_name} because fields dont match"))
          self$drop_table()
        } else {
          create_tab <- FALSE
        }
      }
      if (create_tab) {
        message(glue::glue("Creating table {self$table_name}"))
        create_table(self$conn, self$table_name_fully_specified, self$field_types, self$keys)
        private$add_constraint()
      }
    },

    #' @description
    #' Drop the database table
    drop_table = function() {
      self$connect()
      if (DBI::dbExistsTable(self$conn, self$table_name)) {
        message(glue::glue("Dropping table {self$table_name}"))
        DBI::dbRemoveTable(self$conn, self$table_name)
      }
    },

    #' @description
    #' @param newdata The data to insert.
    #' @param verbose Boolean.
    #' Inserts data into the database table
    insert_data = function(newdata, verbose = TRUE) {
      self$connect()
      if (is.null(newdata)) {
        return()
      }
      if (nrow(newdata) == 0) {
        return()
      }

      newdata <- private$make_censored_data(newdata)

      validated <- self$validator_field_contents(newdata)
      if (!validated) stop(glue::glue("load_data_infile not validated in {self$table_name}. {attr(validated,'var')}"))

      # this will make the insert go faster, because
      # the data will be sorted
      # setkeyv(newdata, self$keys)

      infile <- random_file(self$load_folder_fn())
      load_data_infile(
        conn = self$conn,
        db_config = self$db_config,
        table = self$table_name,
        dt = newdata,
        file = infile
      )
    },

    #' @description
    #' Upserts data into the database table
    #' @param newdata The data to insert.
    #' @param drop_indexes A vector containing the indexes to be dropped before upserting (can increase performance).
    #' @param verbose Boolean.
    upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      self$connect()
      if (is.null(newdata)) {
        return()
      }
      if (nrow(newdata) == 0) {
        return()
      }

      newdata <- private$make_censored_data(newdata)

      validated <- self$validator_field_contents(newdata)
      if (!validated) stop(glue::glue("upsert_load_data_infile not validated in {self$table_name}. {attr(validated,'var')}"))

      # this will make the insert go faster, because
      # the data will be sorted

      infile <- random_file(self$load_folder_fn())
      upsert_load_data_infile(
        # conn = self$conn,
        db_config = self$db_config,
        table = self$table_name,
        dt = newdata[, names(self$field_types), with = F],
        file = infile,
        fields = names(self$field_types),
        keys = self$keys,
        drop_indexes = drop_indexes
      )
    },

    #' @description
    #' Drops all rows in the database table
    drop_all_rows = function() {
      self$connect()
      drop_all_rows(self$conn, self$table_name_fully_specified)
    },

    #' @description
    #' Drops rows in the database table according to the SQL condition.
    #' @param condition SQL text condition.
    drop_rows_where = function(condition) {
      self$connect()
      drop_rows_where(self$conn, self$table_name, condition)
    },

    #' @description
    #' Keeps rows in the database table according to the SQL condition.
    #' @param condition SQL text condition.
    keep_rows_where = function(condition) {
      self$connect()
      keep_rows_where(self$conn, self$table_name, condition)
      private$add_constraint()
    },

    #' @description
    #' Drops all rows in the database table and then upserts data.
    #' @param newdata The data to insert.
    #' @param drop_indexes A vector containing the indexes to be dropped before upserting (can increase performance).
    #' @param verbose Boolean.
    drop_all_rows_and_then_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      self$drop_all_rows()
      self$upsert_data(
        newdata = newdata,
        drop_indexes = drop_indexes,
        verbose = verbose
      )
    },

    #' @description
    #' Drops all rows in the database table and then inserts data.
    #' @param newdata The data to insert.
    #' @param verbose Boolean.
    drop_all_rows_and_then_insert_data = function(newdata, verbose = TRUE) {
      self$drop_all_rows()
      self$insert_data(
        newdata = newdata,
        verbose = verbose
      )
    },

    #' @description
    #' Provides access to the database table via dplyr::tbl.
    tbl = function() {
      self$connect()
      retval <- self$conn %>%
        dplyr::tbl(self$table_name)
      class(retval) <- unique(c("sc_tbl_v8", class(retval)))
      attr(retval, "sc_keys") <- self$keys

      return(retval)
    },

    #' @description
    #' Prints a template dplyr::select call that you can easily copy/paste for all your variables.
    print_dplyr_select = function() {
      x <- self$tbl() %>%
        head() %>%
        dplyr::collect() %>%
        names() %>%
        paste0(., collapse = ",\n  ")
      x <- paste0("dplyr::select(\n  ", x, "\n) %>%")
      cat(x)
    },

    #' @description
    #' Extracts the corresponding row of data from \code{\link{get_config_last_updated}}.
    get_config_last_updated = function() {
      get_config_last_updated(type = "data", tag = self$table_name)
    },

    #' @description
    #' Adds indexes to the database table from `self$indexes`
    add_indexes = function() {
      self$connect()
      for (i in names(self$indexes)) {
        message(glue::glue("Adding index {i}"))

        add_index(
          conn = self$conn,
          table = self$table_name,
          index = i,
          keys = self$indexes[[i]]
        )
      }
    },

    #' @description
    #' Drops all indees from the database table
    drop_indexes = function() {
      self$connect()
      for (i in names(self$indexes)) {
        message(glue::glue("Dropping index {i}"))
        drop_index(
          conn = self$conn,
          table = self$table_name,
          index = i
        )
      }
    },

    # to be deleted soon ----
    db_connect = function(db_config = self$db_config) {
      .Deprecated("connect")
      self$connect(db_config)
    },
    db_disconnect = function() {
      .Deprecated("disconnect")
      self$disconnect()
    }
  ),

  # private ----
  private = list(
    check_fields_match = function() {
      fields <- DBI::dbListFields(self$conn, self$table_name)
      retval <- identical(fields, names(self$field_types))
      if (retval == FALSE) {
        message(glue::glue(
          "given fields: {paste0(names(self$field_types),collapse=', ')}\n",
          "db fields: {paste0(fields,collapse=', ')}"
        ))
      }
      return(retval)
    },

    #' @description
    #' Add constraint to a db table
    add_constraint = function() {
      add_constraint(
        conn = self$conn,
        table = self$table_name,
        keys = self$keys
      )
    },

    #' @description
    #' Drop constraint from a db table
    drop_constraint = function() {
      drop_constraint(
        conn = self$conn,
        table = self$table_name
      )
    },
    make_censored_data = function(newdata) {
      d <- copy(newdata)
      for (i in seq_along(self$censors)) {
        self$censors[[i]](d)
      }
      return(d)
    },
    finalize = function() {
      # self$db_disconnect()
    }
  )
)


# schema ----
#' schema class description
#'
#' @import data.table
#' @import R6
#' @export Schema
Schema <- R6Class(
  "Schema",
  public = list(
    dt = NULL,
    conn = NULL,
    db_config = NULL,
    db_table = NULL,
    table_name_fully_specified = NULL,
    db_field_types = NULL,
    db_field_types_with_length = NULL,
    db_load_folder = NULL,
    load_folder_fn = function() tempdir(check = T),
    keys = NULL,
    keys_with_length = NULL,
    indexes = NULL,
    validator_field_contents = NULL,
    info = "No information given in schema definition",
    initialize = function(dt = NULL,
                          conn = NULL,
                          db_config = NULL,
                          db_table,
                          db_field_types,
                          db_load_folder,
                          keys,
                          indexes = NULL,
                          validator_field_types = validator_field_types_blank,
                          validator_field_contents = validator_field_contents_blank,
                          info = NULL) {
      self$dt <- dt
      self$conn <- conn
      self$db_config <- db_config
      self$db_table <- db_table
      self$db_field_types <- db_field_types
      self$db_field_types_with_length <- db_field_types
      self$db_load_folder <- db_load_folder
      self$keys <- keys
      self$keys_with_length <- keys
      self$indexes <- indexes

      self$table_name_fully_specified <- paste0("[", paste(db_config$db, db_config$schema, db_table, sep = "].["), "]") |>
        stringr::str_remove_all("\\[]\\.")

      # validators
      if (!is.null(validator_field_types)) if (!validator_field_types(self$db_field_types)) stop(glue::glue("db_field_types not validated in {db_table}"))
      self$validator_field_contents <- validator_field_contents

      # info
      if (!is.null(info)) self$info <- info

      # db_field_types_with_lengths
      ind <- self$db_field_types == "TEXT"
      ind_text_with_specific_length <- stringr::str_detect(self$db_field_types, "TEXT")
      ind_text_with_specific_length[ind] <- FALSE
      if (sum(ind) > 0) {
        self$db_field_types_with_length[ind] <- paste0(self$db_field_types_with_length[ind], " (100)")
      }
      if (sum(ind_text_with_specific_length) > 0) {
        lengths <- stringr::str_extract(self$db_field_types[ind_text_with_specific_length], "\\([0-9]*\\)")
        self$db_field_types_with_length[ind_text_with_specific_length] <- paste0(self$db_field_types_with_length[ind_text_with_specific_length], " ", lengths)
      }

      # remove numbers from db_field_types
      naming <- names(self$db_field_types)
      self$db_field_types <- stringr::str_remove(self$db_field_types, " \\([0-9]*\\)")
      names(self$db_field_types) <- naming
      # fixing indexes
      self$keys_with_length <- self$db_field_types_with_length[self$keys]
      if (!is.null(self$conn)) self$db_create_table()
    },







    #' @description
    #' Connect to a db
    #' @param db_config db_config
    connect = function(db_config = self$db_config) {
      self$db_connect(db_config)
    },
    print = function(...) {
      needs_to_connect <- FALSE
      if (is.null(self$conn)) {
        needs_to_connect <- TRUE
      } else if (!DBI::dbIsValid(self$conn)) {
        needs_to_connect <- TRUE
      }

      if (needs_to_connect) {
        cat(self$table_name_fully_specified, " ", crayon::bgRed(crayon::white("(disconnected)\n\n")))
        for (i in seq_along(self$db_field_types)) {
          cat(names(self$db_field_types)[i], " (", self$db_field_types[i], ")", "\n", sep = "")
        }
      } else {
        cat(self$table_name_fully_specified, " ", crayon::bgCyan(crayon::white("(connected)\n\n")))
        for (i in seq_along(self$db_field_types)) {
          var <- names(self$db_field_types)[i]
          details <- ""
          if (
            var %in% c(
              "granularity_time",
              "granularity_geo",
              "country_iso3",
              # "location_code",
              "border",
              "age",
              "sex",
              "isoyear",
              # "isoweek",
              # "isoyearweek",
              "season",
              "tag",
              "type"
            ) |
            stringr::str_detect(var, "^tag_") |
            stringr::str_detect(var, "^_status$")|
            stringr::str_detect(var, "_tag$")
          ) {
            # config$schemas$config_last_updated$connect(); var <- "tag";  details <- dplyr::tbl(config$schemas$config_last_updated$conn, config$schemas$config_last_updated$table_name) |>  dplyr::select(val = !!var) |> dplyr::group_by(val) |> dplyr::summarize(n = n()) |>  dplyr::distinct() |> dplyr::collect() |> setDT() |> setorder(val)
            details <- dplyr::tbl(self$conn, self$db_table) |>
              dplyr::select(val = !!var) |>
              dplyr::group_by(val) |>
              dplyr::summarize(n = n()) |>
              dplyr::collect() |>
              setDT()

            # manually specify some ordering requirements
            levels <- sort(details$val)
            extra_levels <- c(
              "nation",
              "county",
              "notmainlandcounty",
              "missingcounty",
              "municip",
              "notmainlandmunicip",
              "missingmunicip",
              "wardoslo",
              "wardbergen",
              "wardstavanger",
              "wardtrondheim",
              "extrawardoslo",
              "missingwardbergen",
              "missingwardoslo",
              "missingwardstavanger",
              "missingwardtrondheim",
              "baregion",
              "region",
              "faregion"
            )
            reordered_levels <- unique(c(extra_levels, levels))
            reordered_levels <- reordered_levels[reordered_levels %in% levels]
            details[, val := factor(val, levels = reordered_levels)]
            setorder(details, val)

            # create display (n + padding)
            details[, len := stringr::str_length(val)]
            details[, max_len := max(len)]
            details[, display := paste0(stringr::str_pad(val, max_len, side = "right"), " (n = ", fhiplot::format_nor(n), ")")]
            details <- details$display

            for (j in seq_along(details)) details[j] <- paste0("\n\t- ", paste0(details[j], collapse = ""))
            details <- paste0(details, collapse = "")
            details <- paste0(":", details)
          }
          cat(names(self$db_field_types)[i], " (", self$db_field_types[i], ")", details, "\n", sep = "")
        }
      }
      cat("\n")

      invisible(self)
    },

    #' @description
    #' Disconnect from a db
    disconnect = function() {
      self$db_disconnect()
    },

    #' @description
    #' Create db table
    create_table = function() {
      self$db_create_table()
    },
    drop_table = function() {
      self$db_drop_table()
    },

    #' @description
    #' Inserts data into db table
    insert_data = function(newdata, verbose = TRUE) {
      self$db_insert_data(newdata, verbose)
    },

    #' @description
    #' Upserts data into db table
    upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      self$db_upsert_data(newdata, drop_indexes, verbose)
    },
    drop_all_rows = function() {
      self$db_drop_all_rows()
    },
    drop_rows_where = function(condition) {
      self$db_drop_rows_where(condition)
    },
    keep_rows_where = function(condition) {
      self$db_keep_rows_where(condition)
    },
    drop_all_rows_and_then_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      self$db_drop_all_rows_and_then_upsert_data(newdata, drop_indexes, verbose)
    },
    drop_all_rows_and_then_insert_data = function(newdata, verbose = TRUE) {
      self$drop_all_rows()
      self$insert_data(
        newdata = newdata,
        verbose = verbose
      )
    },
    tbl = function() {
      self$dplyr_tbl()
    },
    db_connect = function(db_config = self$db_config) {
      self$conn <- get_db_connection(db_config = db_config)
      use_db(self$conn, db_config$db)
      self$db_create_table()
    },
    db_disconnect = function() {
      if (!is.null(self$conn)) {
        if (DBI::dbIsValid(self$conn)) {
          DBI::dbDisconnect(self$conn)
        }
      }
    },
    db_add_constraint = function() {
      add_constraint(
        conn = self$conn,
        table = self$db_table,
        keys = self$keys
      )
    },
    db_drop_constraint = function() {
      drop_constraint(
        conn = self$conn,
        table = self$db_table
      )
    },
    db_create_table = function() {
      create_tab <- TRUE
      if (DBI::dbExistsTable(self$conn, self$db_table)) {
        if (!self$db_check_fields_match()) {
          message(glue::glue("Dropping table {self$db_table} because fields dont match"))
          self$db_drop_table()
        } else {
          create_tab <- FALSE
        }
      }

      if (create_tab) {
        message(glue::glue("Creating table {self$db_table}"))
        create_table(self$conn, self$table_name_fully_specified, self$db_field_types, self$keys)
        self$db_add_constraint()
      }
    },
    db_drop_table = function() {
      if (DBI::dbExistsTable(self$conn, self$db_table)) {
        DBI::dbRemoveTable(self$conn, self$db_table)
      }
    },
    db_check_fields_match = function() {
      fields <- DBI::dbListFields(self$conn, self$db_table)
      retval <- identical(fields, names(self$db_field_types))
      if (retval == FALSE) {
        message(glue::glue(
          "given fields: {paste0(names(self$db_field_types),collapse=', ')}\n",
          "db fields: {paste0(fields,collapse=', ')}"
        ))
      }
      return(retval)
    },
    db_insert_data = function(newdata, verbose = TRUE) {
      if (is.null(newdata)) {
        return()
      }
      if (nrow(newdata) == 0) {
        return()
      }

      validated <- self$validator_field_contents(newdata)
      if (!validated) stop(glue::glue("db_load_data_infile not validated in {self$db_table}. {attr(validated,'var')}"))

      infile <- random_file(self$load_folder_fn())
      load_data_infile(
        conn = self$conn,
        db_config = self$db_config,
        table = self$db_table,
        dt = newdata,
        file = infile
      )
    },
    db_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      if (is.null(newdata)) {
        return()
      }
      if (nrow(newdata) == 0) {
        return()
      }

      validated <- self$validator_field_contents(newdata)
      if (!validated) stop(glue::glue("db_upsert_load_data_infile not validated in {self$db_table}. {attr(validated,'var')}"))

      infile <- random_file(self$load_folder_fn())
      upsert_load_data_infile(
        # conn = self$conn,
        db_config = self$db_config,
        table = self$db_table,
        dt = newdata[, names(self$db_field_types), with = F],
        file = infile,
        fields = names(self$db_field_types),
        keys = self$keys,
        drop_indexes = drop_indexes
      )
    },
    db_load_data_infile = function(newdata, verbose = TRUE) {
      .Deprecated(new = "db_insert_data", old = "db_load_data_infile")
      self$db_insert_data(newdata = newdata, verbose = verbose)
    },
    db_upsert_load_data_infile = function(newdata, verbose = TRUE) {
      .Deprecated(new = "db_upsert_data", old = "db_upsert_load_data_infile")
      self$db_upsert_data(newdata = newdata, verbose = verbose)
    },
    db_drop_all_rows = function() {
      drop_all_rows(self$conn, self$db_table)
    },
    db_drop_rows_where = function(condition) {
      drop_rows_where(self$conn, self$db_table, condition)
    },
    db_keep_rows_where = function(condition) {
      keep_rows_where(self$conn, self$db_table, condition)
      self$db_add_constraint()
    },
    db_drop_all_rows_and_then_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      self$db_drop_all_rows()
      self$db_upsert_data(
        newdata = newdata,
        drop_indexes = drop_indexes,
        verbose = verbose
      )
    },
    get_data = function(...) {
      dots <- dplyr::quos(...)
      params <- c()

      for (i in seq_along(dots)) {
        temp <- rlang::quo_text(dots[[i]])
        temp <- stringr::str_extract(temp, "^[a-zA-Z0-9]+")
        params <- c(params, temp)
      }

      if (length(params) > length(keys)) {
        stop("Too many requests")
      }
      if (sum(!params %in% keys)) {
        stop("names(...) not in keys")
      }
      if (nrow(self$dt) > 0 | ncol(self$dt) > 0) {
        x <- self$get_data_dt(...)
      } else {
        x <- self$get_data_db(...)
      }
      return(x)
    },
    get_data_dt = function(...) {
      dots <- dplyr::quos(...)
      txt <- c()
      for (i in seq_along(dots)) {
        txt <- c(txt, rlang::quo_text(dots[[i]]))
      }
      if (length(txt) == 0) {
        return(self$dt)
      } else {
        txt <- paste0(txt, collapse = "&")
        return(self$dt[eval(parse(text = txt))])
      }
    },
    get_data_db = function(...) {
      dots <- dplyr::quos(...)
      retval <- self$conn %>%
        dplyr::tbl(self$db_table) %>%
        dplyr::filter(!!!dots) %>%
        dplyr::collect()
      setDT(retval)
      return(retval)
    },
    dplyr_tbl = function() {
      retval <- self$conn %>%
        dplyr::tbl(self$db_table)
      return(retval)
    },
    list_indexes_db = function() {
      list_indexes(
        conn = self$conn,
        table = self$db_table
      )
    },
    add_indexes = function() {
      for (i in names(self$indexes)) {
        message(glue::glue("Adding index {i}"))

        add_index(
          conn = self$conn,
          table = self$db_table,
          index = i,
          keys = self$indexes[[i]]
        )
      }
    },
    drop_indexes = function() {
      for (i in names(self$indexes)) {
        message(glue::glue("Dropping index {i}"))
        drop_index(
          conn = self$conn,
          table = self$db_table,
          index = i
        )
      }
    },
    identify_dt_that_exists_in_db = function() {
      setkeyv(self$dt, self$keys)
      from_db <- self$get_data_db()
      setkeyv(from_db, self$keys)
      self$dt[, exists_in_db := FALSE]
      self$dt[from_db, exists_in_db := TRUE]
    }
  ),
  private = list(
    finalize = function() {
      # self$db_disconnect()
    }
  )
)
