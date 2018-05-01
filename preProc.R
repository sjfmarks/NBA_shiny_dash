library(RSQLite)
library(data.table)


csvpath = "/Users/ICE-9/Desktop/NBA_shiny_dash/NBA_xPlayer_P36M.csv" # "path/to/csv"
dbname = "/Users/ICE-9/Desktop/NBA_shiny_dash/NBA_xPlayer_P36.sqlite"  # "./flights.sqlite"
tblname = "NBA_xPlayer_P36M"
## read csv
data <- fread(input = csvpath,
              sep = ",",
              header = TRUE)
## connect to database

source("helpers.R")

conn <- dbConnect(drv = SQLite(), 
                  dbname = dbname)
## write table
dbWriteTable(conn = conn,
             name = tblname,
             value = data)
## list tables
dbListTables(conn)
## disconnect
dbDisconnect(conn)
