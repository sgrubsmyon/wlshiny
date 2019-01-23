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
  
  output$tageseinnahmen1 <- renderUI({
    df <- pool %>% tbl("abrechnung_tag") %>%
      filter(zeitpunkt > !!(Sys.Date())) %>%
      summarise(brutto = sum(mwst_netto) + sum(mwst_betrag)) %>% collect()
    paste(df$brutto, "€")
  })
  
  output$tageseinnahmen2 <- renderUI({
    df <- pool %>% tbl("abrechnung_tag") %>%
      filter(zeitpunkt > !!(Sys.Date() - 1) & zeitpunkt < !!(Sys.Date() - 0)) %>%
      summarise(brutto = sum(mwst_netto) + sum(mwst_betrag)) %>% collect()
    paste(df$brutto, "€")
  })
  
  output$tageseinnahmen3 <- renderUI({
    df <- pool %>% tbl("abrechnung_tag") %>%
      filter(zeitpunkt > !!(Sys.Date() - 2) & zeitpunkt < !!(Sys.Date() - 1)) %>%
      summarise(brutto = sum(mwst_netto) + sum(mwst_betrag)) %>% collect()
    paste(df$brutto, "€")
  })
  
  output$tageseinnahmen3 <- renderUI({
    df <- pool %>% tbl("abrechnung_tag") %>%
      filter(zeitpunkt > !!(Sys.Date() - 3) & zeitpunkt < !!(Sys.Date() - 2)) %>%
      summarise(brutto = sum(mwst_netto) + sum(mwst_betrag)) %>% collect()
    paste(df$brutto, "€")
  })
}
