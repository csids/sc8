# sc 2023.7.23

- Deprecated in favor of sc9.

# sc 8.0.2

- Allows for multiple databases to be used for different access levels.
- copy_into_new_table_where now also copies indexes.
- V8 schemas now have a nice print function.
- V8 redirects now have a nice print function.
- copy_into_new_table_where uses tablock.
- upsert_at_end_of_each_plan and insert_at_end_of_each_plan can now take named lists as the return value from the action_fn.
- Custom progressr handler.

# sc 8.0.1

- When using `sc8::add_task_from_config_v8` the schema list is checked to make sure they are actually schemas. This will solve the issue where people incorrectly add non-existent schemas to the task.
- insert_data, upsert_data, drop_all_rows_and_then_insert_data are now the recommended ways of inserting data
- addin_load_production
- schemas now use load_folder_fn, which should dynamically check if a user has permission to write to a folder, solving permissions errors
- Including tm_get_schema_names
- Both granularity_time AND granularity_geo are now included in db censors
- Requires R >= 4.1.0
- sc8::config\u0026plan_attempt_index now exists. When running plans in parallel, if a plan fails it is retried five times. This lets a user track which attempt they are on. This is mostly useful so that emails and smses are only sent when  sc8::config$plan_attempt_index==1
- (Disabled) TABLOCK is disabled right now due to issues where data would not be uploaded.
- (Disabled) Data is sorted before sending it to bcp to speed up in/upserts.

# sc 8.0.0

- Release of schema redirects that allow for restricted and anonymous datasets to be seamlessly used by people with different access rights
- Consistent naming of task_from_config_v8 and add_schema_v8

# sc 7.1.4

- db_insert_data, db_upsert_data, db_drop_all_rows_and_then_upsert_data are now the recommended ways of inserting data

# sc 7.1.3

- update_config_datetime and get_config_datetime now automatically record database table updates as well

# sc 7.1.2

- Updating default db schemas to be more explicit with the useage of isotime.

# sc 7.1.1

- `qsenc_save` and `qsenc_read` to save/read to/from encrypted files.

# sc 7.1.0

- `task_from_config_v3` sets a new direction for creation of tasks and management of tasks
- `describe_tasks` and `describe_schemas` help with automatic documentation

# sc 7.0.8

- `task_inline_v1` allows for easy inline task creation
- Corresponding RStudio addin for inline tasks that copy from one db table to another

# sc 7.0.7

- `copy_into_new_table_where` allows for the creation of a new table from an old table
- Including `task_from_config_v2` 
- First RStudio addin

# sc 7.0.6

- write_data_infile now checks for Infinite/NaN values and sets them to NA

# sc 7.0.5

- `Task` now includes `action_before_fn` and `action_after_fn`

# sc 7.0.4

- `validator_field_contents_sykdomspulsen` now allows `baregion` as a valid `granularity_geo`

# sc 7.0.3

- `tm_get_plans_argsets_as_dt` provides an overview of the plans and argsets within a task

# sc 7.0.2

- `keep_rows_where` now also retains the PK constraints
