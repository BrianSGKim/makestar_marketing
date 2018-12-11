library(tidyverse)
library(RMySQL)

mydb <- dbConnect(MySQL(),
                  user=Sys.getenv("DB_USER"),
                  password=Sys.getenv("DB_PWD"),
                  dbname=Sys.getenv("DB_NAME"),
                  host=Sys.getenv("DB_HOST"),
                  port=as.integer(Sys.getenv("DB_PORT")),
                  encoding = "UTF-8")

users <- dbGetQuery(mydb,
                    "SELECT *
                    FROM user")
