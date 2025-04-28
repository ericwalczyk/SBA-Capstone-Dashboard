# 1) Install & load
if (!requireNamespace("DBI", quietly = TRUE))    install.packages("DBI")
if (!requireNamespace("duckdb", quietly = TRUE)) install.packages("duckdb")
library(DBI)
library(duckdb)

# 2) File paths
data_dir    <- "data"
rds_fedcon  <- file.path(data_dir, "smallcon.rds")
rds_acs     <- file.path(data_dir, "acs_summary.rds")
duckdb_file <- file.path(data_dir, "fedcon.duckdb")

# 3) Read in your big RDS files
fedcon_tbl <- readRDS(rds_fedcon)
acs_tbl    <- readRDS(rds_acs)

# 4) Coerce to plain data.frames
fedcon_df <- as.data.frame(fedcon_tbl)
acs_df    <- as.data.frame(acs_tbl)

# 5) Connect (creates or opens the DuckDB file)
con <- dbConnect(duckdb(), dbdir = duckdb_file)

# 6) Write (overwrite) your tables into DuckDB
dbWriteTable(con, "fedcon",      fedcon_df, overwrite = TRUE)
dbWriteTable(con, "acs_summary", acs_df,    overwrite = TRUE)

# 7) (Optional) add indexes for faster queries
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_fedcon_year    ON fedcon(fiscal_year)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_acs_variable   ON acs_summary(variable)")

# 8) Disconnect & finish
dbDisconnect(con, shutdown = TRUE)
message("✅ DuckDB built at ", duckdb_file, " with tables fedcon & acs_summary")