library(data.table)
library(magrittr)
system("/bin/authenticate.sh")


# anon_GROUPING_VARIANT ----
sc::add_schema_v8(
  name_access = c("anon"),
  name_grouping = "test",
  name_variant = NULL,
  db_configs = sc::config$db_configs,
  field_types =  c(
    "uuid" = "INTEGER",
    "n" = "INTEGER"
  ),
  keys = c(
    "uuid"
  ),
  censors = list(
    anon = list(
    )
  ),
  info = "This db table is used for..."
)

d = data.table(uuid = 1:10000)
d$n = 1

sc::config$schemas$anon_test$tbl()
sc::tbl("anon_test")
# setkey(d, uuid)
#setorder(d, -uuid)
a <- Sys.time()
sc::config$schemas$anon_test$drop_all_rows_and_then_insert_data(d)
b <- Sys.time()

b - a

sc::config$schemas$anon_test$tbl()
sc::tbl("anon_test")
sc::print_tables()

sc::config$schemas$anon_test$tbl() %>% dplyr::summarize(n()) %>% dplyr::collect()



sql <- glue::glue("SELECT * INTO sykdomspulsen_interactive_restr.dbo.test FROM sykdomspulsen_interactive_anon.dbo.anon_test")
DBI::dbExecute(pools$no_db, sql)

sql <- glue::glue("SELECT TOP 1 * FROM [sykdomspulsen_interactive_anon].[dbo].[anon_test]")
DBI::dbGetQuery(pool, sql)


sql <- glue::glue("SELECT TOP 1 * FROM [sykdomspulsen_interactive_anon].[dbo].[anon_test]")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)


sql <- glue::glue("SELECT TOP 1 * FROM [anon_test]")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)

sql <- glue::glue("SELECT TOP 1 * FROM [sykdomspulsen_interactive_anon].[].[anon_test]")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)



try(DBI::dbRemoveTable(conn, name = table_to), TRUE)

sql <- glue::glue("EXEC sp_rename '{temp_name}', '{table_to}'")
DBI::dbExecute(conn, sql)
t1 <- Sys.time()
dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
if(config$verbose) message(glue::glue("Copied rows in {dif} seconds from {table_from} to {table_to}"))


DBI::dbListTables(pools$`dm-prod/sykdomspulsen_interactive_anon`)



sql <- glue::glue("
CREATE TABLE dbo.anon_persons (
    PersonID int,
    LastName varchar(255),
    FirstName varchar(255),
    Address varchar(255),
    City varchar(255)
);")
DBI::dbExecute(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)
sc::tbl("anon_persons")
sc::print_tables()

sql <- glue::glue("
CREATE TABLE [FHI\\AK_Sykdomspulsen].anon_persons (
    PersonID int,
    LastName varchar(255),
    FirstName varchar(255),
    Address varchar(255),
    City varchar(255)
);")
DBI::dbExecute(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)
sc::tbl("anon_persons")
sc::print_tables()


sql <- glue::glue("SELECT SUSER_NAME() ")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)

sql <- glue::glue("SELECT SCHEMA_NAME() ")
DBI::dbGetQuery(pools$`[sykdomspulsen_interactive_anon].[dbo]`, sql)

sql <- glue::glue("SELECT SCHEMA_NAME() ")
DBI::dbGetQuery(pools$`[sykdomspulsen_interactive_restr].[dbo]`, sql)

sql <- glue::glue("SELECT name, has_dbaccess(name)
FROM sys.databases")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)
