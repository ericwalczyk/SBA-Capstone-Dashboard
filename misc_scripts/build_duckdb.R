# misc_scripts/build_duckdb.R

# 1) load the packages
if (!requireNamespace("duckdb", quietly = TRUE)) install.packages("duckdb")
if (!requireNamespace("DBI",    quietly = TRUE)) install.packages("DBI")
library(DBI)
library(duckdb)

# 2) define your input & output paths
rds_in_fedcon  <- "data/smallcon.rds"
rds_in_acs     <- "data/acs_summary.rds"
duckdb_out     <- "data/fedcon.duckdb"

# 3) read your RDS files
fedcon_tbl     <- readRDS(rds_in_fedcon)
acs_tbl        <- readRDS(rds_in_acs)

# 4) open (or create) the DuckDB file
con <- dbConnect(duckdb(), dbdir = duckdb_out)

# 5) write both tables into it
dbWriteTable(con, "fedcon",       fedcon_tbl, overwrite = TRUE)
dbWriteTable(con, "acs_summary",  acs_tbl,    overwrite = TRUE)

# 6) clean up
dbDisconnect(con, shutdown = TRUE)

message("🎉 Built ", duckdb_out, " with tables: fedcon, acs_summary")