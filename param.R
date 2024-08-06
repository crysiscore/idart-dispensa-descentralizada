
library(RPostgreSQL)
library(dplyr)
library(writexl)



# Change here when deploying on server
#set working dir
wd <- '/Users/asamuel/Projects/idart-central'

#Local tests
# wd <-getwd()

# PostgreSQL connection parameters
db_host <- "127.0.0.1"
db_port <- 5404
db_name <- "central"
db_user <- "sidmat"
db_password <- "3De38@_qs"


# Miscellaneous Functions
#source('misc.R')

# Initialize a connection object outside the tryCatch block
con <- NULL


# Use a tryCatch block to handle potential connection errors
# tryCatch({

# Create a connection to the PostgreSQL database

message(Sys.time(), "  [sidmat] - Conecting to postgresql server...")

con <- dbConnect(
  PostgreSQL(),
  host = db_host,
  port = db_port,
  dbname = db_name,
  user = db_user,
  password = db_password
)

message(Sys.time(), "  [sidmat] - acquired connection to postgres server")


# Read data from the database
query_clinics <- paste("SELECT * FROM ", "public.clinic")
query_patients <- paste("SELECT * FROM ", "public.sync_temp_patients")
query_dispense <- paste("SELECT * FROM ", "public.sync_temp_dispense")
query_episode <- paste("SELECT * FROM ", "public.sync_temp_episode")

#df_patients <- dbGetQuery(con,query_patients )
#df_dispense <- dbGetQuery(con,query_dispense )
#df_episode <- dbGetQuery(con,query_episode )



load("~/Projects/idart-central/data/all_patients.RData")