#' add task
#' @param task a Task R6 class
#' @export
add_task <- function(task) {
  config$tasks$add_task(task)
}

#' Task
#'
#' @import R6
#' @import foreach
#' @export
Task <- R6::R6Class(
  "Task",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    type = NULL,
    permission = NULL,
    plans = list(),
    schema = list(),
    cores = 1,
    upsert_at_end_of_each_plan = FALSE,
    insert_at_end_of_each_plan = FALSE,
    name = NULL,
    name_description = list(),
    update_plans_fn = NULL,
    action_before_fn = NULL,
    action_after_fn = NULL,
    info = "No information given in task definition.",
    info_plan_analysis_fn_name = NULL,
    info_action_fn_name = NULL,
    info_data_selector_fn_name = NULL,
    info_version = "1",
    initialize = function(name = NULL,
                          name_description = NULL,
                          type,
                          permission = NULL,
                          plans = NULL,
                          update_plans_fn = NULL,
                          schema,
                          cores = 1,
                          upsert_at_end_of_each_plan = FALSE,
                          insert_at_end_of_each_plan = FALSE,
                          action_before_fn = NULL,
                          action_after_fn = NULL,
                          info = NULL,
                          info_plan_analysis_fn_name = NULL,
                          info_action_fn_name = NULL,
                          info_data_selector_fn_name = NULL,
                          info_version = NULL) {
      stopifnot(!(is.null(name) & is.null(name_description)))
      if (!is.null(name_description)) {
        stopifnot(is.list(name_description))
        stopifnot(sum(c("grouping", "action", "variant") %in% names(name_description)) == 3)
        name <- paste0(unlist(name_description), collapse = "_")
      } else {
        name_description <- list(
          grouping = NULL,
          action = NULL,
          variant = NULL
        )
      }
      self$name <- name
      self$name_description <- name_description
      self$type <- type
      self$permission <- permission
      self$plans <- plans
      self$update_plans_fn <- update_plans_fn
      self$schema <- schema
      self$cores <- cores
      self$upsert_at_end_of_each_plan <- upsert_at_end_of_each_plan
      self$insert_at_end_of_each_plan <- insert_at_end_of_each_plan
      self$action_before_fn <- action_before_fn
      self$action_after_fn <- action_after_fn
      if (!is.null(info)) self$info <- info
      self$info_plan_analysis_fn_name <- info_plan_analysis_fn_name
      self$info_action_fn_name <- info_action_fn_name
      self$info_data_selector_fn_name <- info_data_selector_fn_name
      self$info_version <- info_version
    },
    insert_first_last_analysis = function() {
      if (is.null(self$plans)) {
        return()
      }
      if (length(self$plans) == 0) {
        return()
      }
      if (is.null(self$plans[[1]]$analyses)) {
        return()
      }
      if (length(self$plans[[1]]$analyses) == 0) {
        return()
      }
      if (is.null(self$plans[[1]]$analyses[[1]]$argset)) {
        return()
      }
      if (!is.null(self$plans[[1]]$analyses[[1]]$argset$first_analysis)) {
        return()
      }

      for (i in seq_along(self$plans)) {
        for (j in seq_along(self$plans[[i]]$analyses)) {
          self$plans[[i]]$set_use_foreach(FALSE)
          self$plans[[i]]$analyses[[j]]$argset$index_plan <- i
          self$plans[[i]]$analyses[[j]]$argset$index_analysis <- j

          if (i == 1 & j == 1) {
            self$plans[[i]]$analyses[[j]]$argset$first_analysis <- TRUE
          } else {
            self$plans[[i]]$analyses[[j]]$argset$first_analysis <- FALSE
          }

          if (i == length(self$plans) & j == length(self$plans[[i]]$analyses)) {
            self$plans[[i]]$analyses[[j]]$argset$last_analysis <- TRUE
          } else {
            self$plans[[i]]$analyses[[j]]$argset$last_analysis <- FALSE
          }

          if(j == 1){
            self$plans[[i]]$analyses[[j]]$argset$within_plan_first_analysis <- TRUE
          } else {
            self$plans[[i]]$analyses[[j]]$argset$within_plan_first_analysis <- FALSE
          }

          if(j == length(self$plans[[i]]$analyses)){
            self$plans[[i]]$analyses[[j]]$argset$within_plan_last_analysis <- TRUE
          } else {
            self$plans[[i]]$analyses[[j]]$argset$within_plan_last_analysis <- FALSE
          }
        }
      }
    },
    update_plans = function() {
      if (!is.null(self$update_plans_fn)) {
        message(glue::glue("Updating plans..."))
        self$plans <- self$update_plans_fn()
        self$update_plans_fn <- NULL
      }
      self$insert_first_last_analysis()
    },
    num_argsets = function() {
      retval <- 0
      for (i in seq_along(plans)) {
        retval <- retval + plans[[i]]$x_length()
      }
      return(retval)
    },
    num_analyses = function() {
      retval <- 0
      for (i in seq_along(plans)) {
        retval <- retval + plans[[i]]$x_length()
      }
      return(retval)
    },
    run = function(log = TRUE, cores = self$cores) {
      # task <- tm_get_task("analysis_norsyss_qp_gastro")

      message(glue::glue("task: {self$name}"))
      if (!is.null(self$permission)) if (!self$permission$has_permission()) {
        return(NULL)
      }

      upsert_at_end_of_each_plan <- self$upsert_at_end_of_each_plan

      self$update_plans()

      message(glue::glue("Running task={self$name} with plans={length(self$plans)} and analyses={self$num_analyses()}"))
      if (self$num_analyses() == 0) {
        message("Quitting because there is nothing to do (0 analyses)")
        return()
      }

      if (cores == 1 | (length(self$plans) >= 2 & length(self$plans) <= 3)) {
        run_type <- "sequential"
        run_description <- "plans=sequential, argset=sequential"
        cores <- 1
      } else if (length(self$plans) == 1) {
        # in theory, the inner loop should be parallelized
        # but this is not implemented yet
        run_type <- "sequential"
        run_description <- "plans=sequential, argset=sequential"
        cores <- 1
      } else if (interactive()) {
        run_type <- "sequential"
        run_description <- "plans=sequential, argset=sequential"
        cores <- 1

        message("\n***** MULTICORE DOES NOT WORK IN RSTUDIO *****")
        message("***** YOU MUST DO THE FOLLOWING: *****")
        message("***** 1. INSTALL THE PACKAGE (SYKDOMSPULSEN) *****")
        message("***** 2. RUN THE FOLLOWING FROM THE TERMINAL: *****")
        message("\nRscript -e 'sykdomspulsen::tm_run_task(\"", self$name, "\")'\n")
        message("***** GOOD LUCK!! *****\n")
      } else {
        run_type <- "parallel_plans"
        run_description <- "plans=multicore, analyses=sequential"
      }

      if (!is.null(self$action_before_fn)) {
        message("Running action_before_fn")
        self$action_before_fn()
      }
      #
      #       if (!run_sequential) {
      #         if(!interactive()) options("future.fork.enable"=TRUE)
      #         doFuture::registerDoFuture()
      #         #doMC::registerDoMC(2)
      #
      #         if (length(self$plans) == 1) {
      #           # parallelize the inner loop
      #           future::plan(list(
      #             future::sequential,
      #             future::multicore,
      #             workers = cores,
      #             earlySignal = TRUE
      #           ))
      #
      #           parallel <- "plans=sequential, argset=multicore"
      #         } else {
      #           # parallelize the outer loop
      #           future::plan(future::multicore, workers = cores)
      #
      #           parallel <- "plans=multicore, argset=sequential"
      #         }
      #       } else {
      #         data.table::setDTthreads()
      #
      #         parallel <- "plans=sequential, argset=sequential"
      #       }

      message(glue::glue("{run_description} with cores={cores}"))

      a0 <- Sys.time()

      if (run_type == "sequential") {
        # not running in parallel
        progressr::with_progress(
          {
            pb <- progressr::progressor(steps = self$num_analyses())
            private$run_sequential(
              plans_index = 1:length(self$plans),
              schema = self$schema,
              upsert_at_end_of_each_plan = self$upsert_at_end_of_each_plan,
              insert_at_end_of_each_plan = self$insert_at_end_of_each_plan,
              pb = pb,
              cores = cores
            )
          },
          handlers = progressr_handler(),
          # handlers = progressr::handler_progress(
          #   format = ifelse(
          #     interactive(),
          #     "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta",
          #     "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta\n"
          #   ),
          #   interval = 10.0,
          #   clear = FALSE
          # ),
          # handlers = progressr::handler_rstudio(
          #   title = self$name
          # ),
          interval = 10,
          delay_stdout = FALSE,
          delay_conditions = ""
        )
      } else if (run_type == "parallel_plans") {
        # running in parallel

        message("Running plans 1 and ", length(self$plans), " sequentially, and 2:", length(self$plans) - 1, " in parallel\n")

        message("*****")
        message("*****")
        message("***** Running plan 1 sequentially at ", lubridate::now(), " *****")
        # pb <- progressr::progressor(steps = self$plans[[1]]$x_length())
        private$run_sequential(
          plans_index = 1,
          schema = self$schema,
          upsert_at_end_of_each_plan = self$upsert_at_end_of_each_plan,
          insert_at_end_of_each_plan = self$insert_at_end_of_each_plan,
          cores = cores
        )
        b0 <- Sys.time()
        message("\nPlan 1 ran in ", round(as.numeric(difftime(b0, a0, units = "mins")), 1), " mins\n")

        message("*****")
        message("*****")
        message("***** Running plans 2:", (length(self$plans) - 1), " in parallel at ", lubridate::now(), " *****")

        private$run_parallel_plans(
          plans_index = 2:(length(self$plans) - 1),
          schema = self$schema,
          upsert_at_end_of_each_plan = self$upsert_at_end_of_each_plan,
          insert_at_end_of_each_plan = self$insert_at_end_of_each_plan,
          cores = cores
        )

        message("\n*****")
        message("*****")
        message("***** Running plan ", length(self$plans), " sequentially at ", lubridate::now(), " *****")
        a1 <- Sys.time()
        private$run_sequential(
          plans_index = length(self$plans),
          schema = self$schema,
          upsert_at_end_of_each_plan = self$upsert_at_end_of_each_plan,
          insert_at_end_of_each_plan = self$insert_at_end_of_each_plan,
          cores = cores
        )
        b1 <- Sys.time()
        message("\nPlan ", length(self$plans), " ran in ", round(as.numeric(difftime(b1, a1, units = "mins")), 1), " mins")
      }

      b1 <- Sys.time()
      message("Task ran in ", round(as.numeric(difftime(b1, a0, units = "mins")), 1), " mins\n")

      future::plan(future::sequential)
      foreach::registerDoSEQ()
      data.table::setDTthreads()

      if (!is.null(self$action_after_fn)) {
        message("Running action_after_fn")
        self$action_after_fn()
      }

      update_config_last_updated(type = "task", tag = self$name)
      if (!is.null(self$permission)) self$permission$revoke_permission()
    }
  ),
  private = list(
    run_sequential = function(plans_index, schema, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, pb = NULL, cores) {
      for (s in schema) s$connect()
      for (i in seq_along(self$plans[plans_index])) {
        if (!is.null(pb)) self$plans[plans_index][[i]]$set_progressor(pb)
        config$plan_attempt_index <- 1

        if (length(plans_index) == 1 & is.null(pb)) {
          verbose <- TRUE
        } else {
          verbose <- FALSE
        }

        # self$plans[plans_index][[i]]$set_verbose(FALSE)
        data <- self$plans[plans_index][[i]]$get_data()
        hashes <- data$hash
        last_run_hashes <- get_last_run_data_hash_split_into_plnr_format(task = self$name, index_plan = i, expected_element_tags = names(data$hash$current_elements))
        data$hash$last_run <- last_run_hashes$last_run
        data$hash$last_run_elements <- last_run_hashes$last_run_elements

        retval <- self$plans[plans_index][[i]]$run_all_with_data(data = data, schema = schema)

        if(upsert_at_end_of_each_plan){
          retval <- splutil::unnest_dfs_within_list_of_fully_named_lists(retval, returned_name_when_dfs_are_not_nested = "output", use.names = T, fill = T)
          for(df_name in names(retval)){
            schema[[df_name]]$upsert_data(retval[[df_name]], verbose = verbose)
          }
        }
        if (insert_at_end_of_each_plan) {
          retval <- splutil::unnest_dfs_within_list_of_fully_named_lists(retval, returned_name_when_dfs_are_not_nested = "output", use.names = T, fill = T)
          for(df_name in names(retval)){
            schema[[df_name]]$insert_data(retval[[df_name]], verbose = verbose)
          }
        }

        rm("retval")

        update_config_data_hash_for_each_plan(
          task = self$name,
          index_plan = i,
          element_tag = names(hashes$current_elements),
          element_hash = unlist(hashes$current_elements),
          all_hash = hashes$current
        )
        rm("data")
      }
      for (s in schema) s$disconnect()
    },
    run_parallel_plans = function(plans_index, schema, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, cores) {
      y <- pbmcapply::pbmclapply(
        self$plans[plans_index],
        function(x, schema, upsert_at_end_of_each_plan, insert_at_end_of_each_plan) {
          config$in_parallel <- TRUE # this will stop TABLOCK from being used in in/upserts
          data.table::setDTthreads(1)
          x$set_verbose(FALSE)
          message(".")

          for (tries in 1:5) {
            config$plan_attempt_index <- tries

            catch_result <- tryCatch(
              {
                for (s in schema) s$connect()
                data <- x$get_data()
                hashes <- data$hash
                last_run_hashes <- get_last_run_data_hash_split_into_plnr_format(task = self$name, index_plan = x$get_argset(1)$index_plan, expected_element_tags = names(data$hash$current_elements))
                data$hash$last_run <- last_run_hashes$last_run
                data$hash$last_run_elements <- last_run_hashes$last_run_elements

                retval <- x$run_all_with_data(data = data, schema = schema)

                if(upsert_at_end_of_each_plan){
                  retval <- splutil::unnest_dfs_within_list_of_fully_named_lists(retval, returned_name_when_dfs_are_not_nested = "output", use.names = T, fill = T)
                  for(df_name in names(retval)){
                    schema[[df_name]]$upsert_data(retval[[df_name]], verbose = F)
                  }
                }
                if (insert_at_end_of_each_plan) {
                  retval <- splutil::unnest_dfs_within_list_of_fully_named_lists(retval, returned_name_when_dfs_are_not_nested = "output", use.names = T, fill = T)
                  for(df_name in names(retval)){
                    schema[[df_name]]$insert_data(retval[[df_name]], verbose = F)
                  }
                }

                rm("retval")

                # this might break things!!!!!!
                update_config_data_hash_for_each_plan(
                  task = self$name,
                  index_plan = x$get_argset(1)$index_plan,
                  element_tag = names(hashes$current_elements),
                  element_hash = unlist(hashes$current_elements),
                  all_hash = hashes$current
                )
                rm("data")

                return(list(
                  error = FALSE,
                  msg = "success"
                ))
              },
              error = function(e) {
                return(list(
                  error = TRUE,
                  msg = paste0("Error in index ", x$get_argset(1)$index, ".\n********\n", e$message, "\n********\n")
                ))
              }
            )
            for (s in schema) s$disconnect()

            # if the plan executed without any errors
            # then break the loop
            # otherwise sleep for 5 seconds and try again
            if (!catch_result$error) {
              break()
            } else {
              Sys.sleep(5)
            }
          }
          if (catch_result$error) stop(catch_result$msg)

          # ***************************** #
          # NEVER DELETE gc()             #
          # IT CAUSES 2x SPEEDUP          #
          # AND 10x MEMORY EFFICIENCY     #
          gc() #
          # ***************************** #
          1
        },
        schema = schema,
        upsert_at_end_of_each_plan = upsert_at_end_of_each_plan,
        insert_at_end_of_each_plan = insert_at_end_of_each_plan,
        ignore.interactive = TRUE,
        mc.cores = cores,
        mc.style = "ETA",
        mc.substyle = 2
      )
      config$in_parallel <- FALSE # this will allow TABLOCK in in/upserts

      try_error_index <- unlist(lapply(y, function(x) inherits(x, "try-error")))
      if (sum(try_error_index) > 0) {
        stop("Error running in parallel: ", y[try_error_index][1][[1]][1])
      }
      # print(y)
    },
    # run_parallel = function(plans_index, schema, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, pb){
    #   y <- foreach(x = self$plans[plans_index]) %dopar% {
    #     data.table::setDTthreads(1)
    #
    #     for (s in schema) s$connect()
    #     x$set_progressor(pb)
    #     retval <- x$run_all(schema = schema)
    #
    #     if (upsert_at_end_of_each_plan) {
    #       retval <- rbindlist(retval)
    #       schema$output$upsert_data(retval, verbose = F)
    #     }
    #
    #     if (insert_at_end_of_each_plan) {
    #       retval <- rbindlist(retval)
    #       schema$output$insert_data(retval, verbose = F)
    #     }
    #     rm("retval")
    #     for (s in schema) s$db_disconnect()
    #
    #     # ***************************** #
    #     # NEVER DELETE gc()             #
    #     # IT CAUSES 2x SPEEDUP          #
    #     # AND 10x MEMORY EFFICIENCY     #
    #     gc() #
    #     # ***************************** #
    #     1
    #   }
    # },
    run_parallel = function(plans_index, schema, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, pb) {
      # y <- future.apply::future_lapply(
      #   self$plans[plans_index],
      #   function(x, schema, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, pb){
      #     data.table::setDTthreads(1)
      #
      #     for (s in schema) s$connect()
      #     x$set_progressor(pb)
      #     retval <- x$run_all(schema = schema)
      #
      #     if (upsert_at_end_of_each_plan) {
      #       retval <- rbindlist(retval)
      #       schema$output$upsert_data(retval, verbose = F)
      #     }
      #
      #     if (insert_at_end_of_each_plan) {
      #       retval <- rbindlist(retval)
      #       schema$output$insert_data(retval, verbose = F)
      #     }
      #     rm("retval")
      #     for (s in schema) s$db_disconnect()
      #
      #     # ***************************** #
      #     # NEVER DELETE gc()             #
      #     # IT CAUSES 2x SPEEDUP          #
      #     # AND 10x MEMORY EFFICIENCY     #
      #     gc() #
      #     # ***************************** #
      #     1
      #   },
      #   schema = schema,
      #   upsert_at_end_of_each_plan = upsert_at_end_of_each_plan,
      #   insert_at_end_of_each_plan = insert_at_end_of_each_plan,
      #   pb = pb
      # )
      y <- pbmcapply::pbmcmapply(
        function(x, schema, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, pb) {
          data.table::setDTthreads(1)

          for (s in schema) s$connect()
          x$set_progressor(pb)
          retval <- x$run_all(schema = schema)

          if (upsert_at_end_of_each_plan) {
            retval <- rbindlist(retval)
            schema$output$upsert_data(retval, verbose = F)
          }

          if (insert_at_end_of_each_plan) {
            retval <- rbindlist(retval)
            schema$output$insert_data(retval, verbose = F)
          }
          rm("retval")
          for (s in schema) s$db_disconnect()

          # ***************************** #
          # NEVER DELETE gc()             #
          # IT CAUSES 2x SPEEDUP          #
          # AND 10x MEMORY EFFICIENCY     #
          gc() #
          # ***************************** #
          1
        },
        self$plans[plans_index],
        MoreArgs = list(
          schema, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, pb
        ),
        ignore.interactive = TRUE,
        mc.cores = 2
      )
      # y <- foreach(x = self$plans[plans_index]) %dopar% {
      #   data.table::setDTthreads(1)
      #
      #   for (s in schema) s$connect()
      #   x$set_progressor(pb)
      #   retval <- x$run_all(schema = schema)
      #
      #   if (upsert_at_end_of_each_plan) {
      #     retval <- rbindlist(retval)
      #     schema$output$upsert_data(retval, verbose = F)
      #   }
      #
      #   if (insert_at_end_of_each_plan) {
      #     retval <- rbindlist(retval)
      #     schema$output$insert_data(retval, verbose = F)
      #   }
      #   rm("retval")
      #   for (s in schema) s$db_disconnect()
      #
      #   # ***************************** #
      #   # NEVER DELETE gc()             #
      #   # IT CAUSES 2x SPEEDUP          #
      #   # AND 10x MEMORY EFFICIENCY     #
      #   gc() #
      #   # ***************************** #
      #   1
      # }
    }
  )
)

data_function_factory <- function(table_name, filter) {
  force(table_name)
  force(filter)
  function() {
    if (is.na(filter)) {
      d <- tbl(table_name) %>%
        dplyr::collect() %>%
        latin1_to_utf8()
    } else {
      d <- tbl(table_name) %>%
        dplyr::filter(!!!rlang::parse_exprs(filter)) %>%
        dplyr::collect() %>%
        latin1_to_utf8()
    }
  }
}

get_filters <- function(for_each, table_name, filter = "") {
  retval <- list()
  for (t in names(for_each)) {
    message(glue::glue("{Sys.time()} Starting pulling plan data for {t} from {table_name}"))
    if (for_each[t] == "all") {
      table <- tbl(table_name)
      if (filter != "") {
        table <- table %>% dplyr::filter(!!!rlang::parse_exprs(filter))
      }
      options <- table %>%
        dplyr::distinct(!!as.symbol(t)) %>%
        dplyr::collect() %>%
        dplyr::pull(!!as.symbol(t))
    } else {
      options <- for_each[[t]]
    }
    retval[[t]] <- options
    message(glue::glue("{Sys.time()} Finished pulling plan data for {t} from {table_name}"))
  }
  return(retval)
}
