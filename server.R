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

geldformat <- function(betrag) {
  sprintf("%s €", format(betrag, nsmall = 2, big.mark = ".", decimal.mark = ","))
}

function(input, output, session) {
  heute <- Sys.Date()
  
  output$tbl <- renderTable({
    pool %>% tbl("verkauf") %>% arrange(desc(rechnungs_nr)) %>% head(10) %>% collect()
  })
  
  tagesfilter <- function(pool, anzahl_tage) {
    filter(pool, zeitpunkt > !!(heute - anzahl_tage) &
             zeitpunkt < !!(heute - (anzahl_tage - 1)))
  }
  
  heute_filter <- function(pool) { tagesfilter(pool, 0) }
  gestern_filter <- function(pool) { tagesfilter(pool, 1) }
  vorgestern_filter <- function(pool) { tagesfilter(pool, 2) }
  vorvorgestern_filter <- function(pool) { tagesfilter(pool, 3) }
  heute_vor_einer_woche_filter <- function(pool) { tagesfilter(pool, 7) }
  gestern_vor_einer_woche_filter <- function(pool) { tagesfilter(pool, 8) }
  vorgestern_vor_einer_woche_filter <- function(pool) { tagesfilter(pool, 9) }
  vorvorgestern_vor_einer_woche_filter <- function(pool) { tagesfilter(pool, 10) }
  
  parse_betrag <- function(betrag) {
    if (is.na(betrag)) betrag <- 0
    betrag
  }
  
  tageseinnahmen <- function(pool, filter_function) {
    df <- pool %>% tbl("abrechnung_tag") %>%
      filter_function() %>%
      summarise(brutto = sum(mwst_netto) + sum(mwst_betrag)) %>% collect()
    parse_betrag(df$brutto)
  }
  
  tageseinnahmen_live <- function(pool) {
    # SELECT SUM(ges_preis) FROM kasse.verkauf_details INNER JOIN kasse.verkauf USING (rechnungs_nr) WHERE DATE(verkaufsdatum) = '2019-01-21';
    df <- pool %>% tbl("verkauf_details") %>%
      inner_join(tbl(pool, "verkauf"), by = "rechnungs_nr") %>%
      filter(date(verkaufsdatum) == heute) %>%
      summarise(brutto = sum(ges_preis)) %>% collect()
    parse_betrag(df$brutto)
  }
  
  tageseinnahmen_vergleich <- function(dir, vgl) {
    aenderung <- round((dir - vgl) / vgl * 100)
    if (vgl == 0) {
      if (dir == 0) aenderung <- 0 # zero makes sense
      else return(NULL) # no comparison possible
    }
    if (aenderung < 0) {
      color <- "red"
      vorzeichen <- "-"
      icn <- icon("caret-down")
    } else {
      color <- "green"
      if (aenderung == 0) {
        vorzeichen <- "±"
        icn <- ""
      } else {
        vorzeichen <- "+"
        icn <- icon("caret-up")
      }
    }
    HTML(sprintf(
      "<span style='color: %s;'>%s%s%% %s</span></span> <span style='font-size:14px'>ggü. Vorwoche (%s €)</span>",
      color, vorzeichen, abs(aenderung), icn, vgl
    ))
  }
  
  output$tageseinnahmen_direkt1 <- renderUI({
    te <- tageseinnahmen(pool, heute_filter)
    if (te == 0) {
      te <- tageseinnahmen_live(pool)
    }
    geldformat(te)
  })
  output$tageseinnahmen_direkt2 <- renderUI({ geldformat(tageseinnahmen(pool, gestern_filter)) })
  output$tageseinnahmen_direkt3 <- renderUI({ geldformat(tageseinnahmen(pool, vorgestern_filter)) })
  output$tageseinnahmen_direkt4 <- renderUI({ geldformat(tageseinnahmen(pool, vorvorgestern_filter)) })
  output$tageseinnahmen_vergleich1 <- renderUI({
    dir <- tageseinnahmen(pool, heute_filter)
    if (dir == 0) {
      dir <- tageseinnahmen_live(pool)
    }
    vgl <- tageseinnahmen(pool, heute_vor_einer_woche_filter)
    tageseinnahmen_vergleich(dir, vgl)
  })
  output$tageseinnahmen_vergleich2 <- renderUI({
    tageseinnahmen_vergleich(tageseinnahmen(pool, gestern_filter),
                             tageseinnahmen(pool, gestern_vor_einer_woche_filter))
  })
  output$tageseinnahmen_vergleich3 <- renderUI({
    tageseinnahmen_vergleich(tageseinnahmen(pool, vorgestern_filter),
                             tageseinnahmen(pool, vorgestern_vor_einer_woche_filter))
  })
  output$tageseinnahmen_vergleich4 <- renderUI({
    tageseinnahmen_vergleich(tageseinnahmen(pool, vorvorgestern_filter),
                             tageseinnahmen(pool, vorvorgestern_vor_einer_woche_filter))
  })
}
