library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname = 'fia', user = 'paciorek', password = 'test')

tree = dbGetQuery(db, "select * from fiaout.tree")
plot = dbGetQuery(db, "select * from fiaout.plot")
survey = dbGetQuery(db, "select * from fiaout.survey")

library(RSQLite)
drv2 <- dbDriver("SQLite")
db2 <- dbConnect(drv2, dbname = 'fia.sqlite3')

dbWriteTable(db2, "tree", tree)
dbWriteTable(db2, "survey", survey)
dbWriteTable(db2, "plot", plot)
