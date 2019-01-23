library(shiny)
library(shinymaterial)
library(jsonlite)
library(RMySQL)
library(pool)
library(dplyr)

dbsetup <- jsonlite::read_json("~/.dbsetup.json")

pool <- dbPool(
  drv = RMySQL::MySQL(),
  host = dbsetup$kasse$host,
  dbname = dbsetup$kasse$db,
  username = dbsetup$kasse$user,
  password = dbsetup$kasse$pass
)
onStop(function() {
  poolClose(pool)
})

function(input, output, session) {
  output$tbl <- renderTable({
    pool %>% tbl("verkauf") %>% arrange(desc(rechnungs_nr)) %>% head(10) %>% collect()
  })
}
