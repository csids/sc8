#' shortcut to get available task names
#' @export
tm_get_task_names <- function() {
  names(config$tasks$list_task)
}

#' Shortcut to task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_analysis Not used
#' @param index_argset Deprecated
#' @export
tm_get_task <- function(task_name, index_plan = NULL, index_analysis = NULL, index_argset = NULL) {
  retval <- config$tasks$get_task(task_name)
  retval$update_plans()
  return(retval)
}

#' Shortcut to update plans for a task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_analysis Not used
#' @param index_argset Deprecated
#' @export
tm_update_plans <- function(task_name, index_plan = NULL, index_analysis = NULL, index_argset = NULL) {
  task <- tm_get_task(
    task_name = task_name,
    index_plan = index_plan,
    index_analysis = index_analysis
  )
  task$update_plans()
}

#' Shortcut to run task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_analysis Not used
#' @param index_argset Deprecated
#' @param run_as_rstudio_job_loading_from_devtools Run the task as an rstudio job, loading from devtools
#' @export
tm_run_task <- function(task_name,
                        index_plan = NULL,
                        index_analysis = NULL,
                        index_argset = NULL,
                        run_as_rstudio_job_loading_from_devtools = FALSE) {
  # message(glue::glue("spulscore {utils::packageVersion('sc')}"))

  if (!run_as_rstudio_job_loading_from_devtools) {
    task <- tm_get_task(
      task_name = task_name,
      index_plan = index_plan,
      index_analysis = index_analysis
    )
    task$run(log = FALSE)
  } else {
    # get num_analyses
    # tempfile <- fs::path(tempdir(check = T), paste0(task_name, ".r"))
    # cat(glue::glue(
    #   "
    #     devtools::load_all('.')
    #     num_analyses <- sc::tm_get_plans_argsets_as_dt('{task_name}') %>%
    #       nrow()
    #     cat('RETVAL:',num_analyses,'\n')
    #   "
    # ), file = tempfile)
    #
    #
    # x <- system2("Rscript",tempfile, stdout=TRUE, stderr = NULL)
    # x <- stringr::str_extract(x,"RETVAL: [0-9]+")
    # x <- stringr::str_extract(x,"[0-9]+")
    # num_analyses <- as.numeric()
    #
    # rstudioapi::jobAdd(
    #   name = task_name,
    #   progressUnits = num_analyses
    # )

    tempfile <- fs::path(tempdir(check = T), paste0(task_name, ".r"))
    cat(glue::glue(
      "
        devtools::load_all('.')
        sc::config$tasks$list_task[[{task_name}}]$cores <- 1
        tm_run_task('{task_name}')
      "
    ), file = tempfile)
    rstudioapi::jobRunScript(
      path = tempfile,
      name = task_name,
      workingDir = getwd(),
    )
  }
}

#' Shortcut to plan within task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_analysis Not used
#' @param index_argset Deprecated
#' @export
tm_get_plans <- function(task_name, index_plan = NULL, index_analysis = NULL, index_argset = NULL) {
  tm_get_task(task_name = task_name)$plans
}

#' Shortcut to plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_analysis Not used
#' @param index_argset Deprecated
#' @export
tm_get_plan <- function(task_name, index_plan = 1, index_analysis = NULL, index_argset = NULL) {
  tm_get_task(task_name = task_name)$plans[[index_plan]]
}

#' Shortcut to data within plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_analysis Not used
#' @param index_argset Deprecated
#' @return
#' Named list, where most elements come from the data_selector_fn.
#' One extra named element is called 'hash'. 'hash' contains the data hashes of particular datasets/variables, as calculated from the method 'get_data' inside plnr::Plan.
#' 'hash' contains four named elements, each of which have been :
#' - current (the hash of the named list returned from data_selector_fn)
#' - current_elements (the hash of the named elements within the named list returned from data_selector_fn)
#' - last_run (the same as 'current', except for the last successful run)
#' - last_run_elements (the same as 'current_elements', except for the last successful run)
#' @export
tm_get_data <- function(task_name, index_plan = 1, index_analysis = NULL, index_argset = NULL) {
  data <- tm_get_plan(
    task_name = task_name,
    index_plan = index_plan
  )$get_data()
  last_run_hashes <- get_last_run_data_hash_split_into_plnr_format(task = task_name, index_plan = index_plan, expected_element_tags = names(data$hash$current_elements))
  data$hash$last_run <- last_run_hashes$last_run
  data$hash$last_run_elements <- last_run_hashes$last_run_elements

  # https://rstudio.github.io/rstudio-extensions/connections-contract.html?_ga=2.130285583.1223674375.1634876289-790799150.1584566635
  observer <- getOption("connectionObserver")
  # if no observer, or data is empty, just return the data
  if (is.null(observer) | length(data) == 0) {
    return(data)
  }

  # close observer
  observer$connectionClosed(
    type = "sc data",
    host = "sc_data"
  )

  # let observer know that connection has opened
  observer$connectionOpened(
    # connection type
    type = "sc data",

    # name displayed in connection pane
    displayName = paste0("Data (", task_name, ")"),

    # host key
    host = "sc_data",

    # icon for connection
    # icon = icon,

    # connection code
    connectCode = paste0('data <- sc::tm_get_data("', task_name, '", index_plan = ', index_plan, ")"),

    # disconnection code
    disconnect = function() {
      rm("data")
      gc()
    },
    listObjectTypes = function() {
      list(
        schema = list(
          # icon = "path/to/schema.png",
          contains = list(
            table = list(
              contains = "data"
            )
          )
        )
      )
    },

    # table enumeration code
    listObjects = function(...) {
      retval <- data.frame(name = names(data), type = "table")
      for (i in seq_len(nrow(retval))) {
        if(retval$name[i] == "hash"){
          if("current" %in% names(data$hash) & "last_run" %in% names(data$hash)){
            if(data$hash$current == data$hash$last_run){
              retval[i, 1] <- "hash: current == last_run"
            } else {
              retval[i, 1] <- "hash: current != last_run"
            }
          }
        } else {
          size <- pryr::object_size(data[[i]])
          size <- size / (1000^2)
          size <- formatC(round(size, 1), digits = 1, format = "f")
          retval[i, 1] <- paste0(retval[i, 1], " (", size, " MB / n = ", nrow(data[[i]]), ")")
        }
      }
      return(retval)
    },

    # column enumeration code
    listColumns = function(table) {
      x_table <- stringr::str_remove(table, " \\(.*$")
      x <- data.frame(name = names(data[[x_table]]), type = sapply(data[[x_table]], class))
      for (i in seq_len(nrow(x))) {
        var <- x$name[i]
        if (x$name[i] %in% c("granularity_geo")) {
          unique_vals <- data[[x_table]][, .(N = .N), keyby = var]
          setnames(unique_vals, c("val", "N"))
          ordering <- unique(c(
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
            "faregion",
            unique_vals$val
          ))
          unique_vals[, val := factor(val, levels = ordering)]
          setorder(unique_vals, val)
          unique_vals[, lab := paste0(val, " (", N, ")")]
          x$type[i] <- paste0(unique_vals$lab, collapse = " / ")
        } else if (
          x$name[i] %in% c("granularity_time", "border", "age", "sex", "isoyear", "season") |
            (x$type[i] %in% c("character") & length(unique(data[[x_table]][[i]])) <= 10)
        ) {
          unique_vals <- data[[x_table]][, .(N = .N), keyby = var]
          setnames(unique_vals, c("val", "N"))

          unique_vals[, lab := paste0(val, " (", N, ")")]
          x$type[i] <- paste0(unique_vals$lab, collapse = " / ")
        } else if (x$type[i] %in% c("character", "integer", "Date")) {
          x$type[i] <- paste0(min(data[[x_table]][[i]], na.rm = T), " to ", max(data[[x_table]][[i]], na.rm = T))
        } else if (x$type[i] %in% c("numeric")) {
          x$type[i] <- paste0(round(min(data[[x_table]][[i]], na.rm = T), 3), " to ", round(max(data[[x_table]][[i]], na.rm = T), 3))
        }
      }
      x
    },

    # table preview code
    previewObject = function(rowLimit, table, ...) {
      x_table <- stringr::str_remove(table, " \\(.*$")
      data[[x_table]][1:rowLimit, ]
    },

    # other actions that can be executed on this connection
    # actions = odbcConnectionActions(connection),

    # raw connection object
    connectionObject = NULL
  )

  # observer$connectionUpdated(
  #   type = "sc data",
  #   host = paste0("data_", task_name),
  #   hint = as.character(index_plan),
  #   connectionObject = data,
  #   data = data
  # )

  return(data)
}

#' Shortcut to argset within plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_analysis Not used
#' @param index_argset Argset within plan
#' @export
tm_get_argset <- function(task_name, index_plan = 1, index_analysis = 1, index_argset = NULL) {
  tm_get_plan(
    task_name = task_name,
    index_plan = index_plan
  )$get_argset(index_analysis)
}

#' Shortcut to schema within task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_analysis Not used
#' @param index_argset Deprecated
#' @export
tm_get_schema <- function(task_name, index_plan = NULL, index_analysis = NULL, index_argset = NULL) {
  schema <- tm_get_task(
    task_name = task_name
  )$schema
  for (s in schema) s$connect()
  return(schema)
}

analyses_to_dt <- function(analyses) {
  retval <- lapply(analyses, function(x) {
    data.table(t(x$argset))
  })
  retval <- rbindlist(retval)
  # retval[, index_analysis := 1:.N]

  return(retval)
}

plans_to_dt <- function(plans) {
  retval <- lapply(plans, function(x) analyses_to_dt(x$analyses))
  # for (i in seq_along(retval)) retval[[i]][, index_plan := i]
  retval <- rbindlist(retval)
  setcolorder(retval, c("index_plan", "index_analysis"))
  retval
}

#' Gets a data.table overview of index_plan and index_analysis
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_analysis Not used
#' @param index_argset Deprecated
#' @export
tm_get_plans_argsets_as_dt <- function(task_name, index_plan = NULL, index_analysis = NULL, index_argset = NULL) {
  p <- tm_get_plans(task_name)
  plans_to_dt(p)
}


#'
#'
#' TaskManager
#'
#' An R6 Action class contains a function called 'run' that takes three arguments:
#' - data (list)
#' - arg (list)
#' - schema (list)
#'
#' An action is (note: we dont explicitly create these):
#' - one R6 Action class
#' - A plnr::Plan that provide
#'   a) data (from the plan -- `get_data`)
#'   b) arguments (from the plan -- `get_argset`)
#' to the R6 action class
#'
#' A task is:
#' - one R6 Action class
#' - A plnr::Plan that provide
#'   a) data (from the plan -- `get_data`)
#'   b) arguments (from the plan -- `get_argset`)
#' to the R6 action class
#'
#' A TaskManager is:
#' - one R6 Action class
#' - A list of plnr::Plan's
#'
#' @import R6
#' @export TaskManager
TaskManager <- R6::R6Class(
  "TaskManager",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    list_task = list(),
    initialize = function() {
      # nothing
    },
    add_task = function(task) {
      list_task[[task$name]] <<- task
    },
    get_task = function(name) {
      list_task[[name]]
    },
    ## list_plan_get = function(task_name){
    ##   if(is.null(list_task[[task_name]]$list_plan) & is.null(list_task[[task_name]]$fn_plan)){
    ##     retval <- list(x=1)
    ##   } else if(!is.null(list_task[[task_name]]$list_plan) & is.null(list_task[[task_name]]$fn_plan)){
    ##     retval <- list_task[[task_name]]$list_plan
    ##   } else if(is.null(list_task[[task_name]]$list_plan) & !is.null(list_task[[task_name]]$fn_plan)){
    ##     retval <- list_task[[task_name]]$fn_plan()
    ##   }
    ##   return(retval)
    ## },


    run_all = function(log = TRUE) {
      for (task in list_task) {
        task_run(task$name, log = log)
      }
    },
    task_run = function(name, log = TRUE) {
      list_task[[name]]$run(log)
    },
    run = function() {
      stop("run must be implemented")
    }
  )
)
