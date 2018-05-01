
dbConnector <- function(session, dbname) {
  require(RSQLite)
  ## setup connection to database
  conn <- dbConnect(drv = SQLite(), 
                    dbname = dbname)
  ## disconnect database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
  ## return connection
  conn
}

dbGetData <- function(conn, tblname) {
  query <- paste("SELECT * FROM", tblname)
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

