# misc_scripts/build_duckdb.R

# 1) load packages
library(DBI); library(duckdb)

# 2) file paths
data_dir <- "data"
rds_fedcon <- file.path(data_dir, "smallcon.rds")
rds_acs <- file.path(data_dir, "acs_summary.rds")
duckdb_file <- file.path(data_dir, "fedcon.duckdb")

# 3) read RDS
fedcon_tbl <- readRDS(rds_fedcon)
acs_tbl    <- readRDS(rds_acs)

# 4) coerce
fedcon_df <- as.data.frame(fedcon_tbl)
acs_df    <- as.data.frame(acs_tbl)

# 5) connect & write
con <- dbConnect(duckdb(), dbdir = duckdb_file)
dbWriteTable(con, "fedcon", fedcon_df, overwrite=TRUE)
dbWriteTable(con, "acs_summary", acs_df, overwrite=TRUE)

# 6) optional indexes
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_fedcon_year ON fedcon(fiscal_year)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_acs_year    ON acs_summary(year)")
# 7) close
dbDisconnect(con, shutdown=TRUE)
message("✅ DuckDB built at ", duckdb_file)