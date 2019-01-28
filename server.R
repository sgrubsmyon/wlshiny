library(shiny)
library(shinymaterial)
library(jsonlite)
library(RMySQL)
library(pool)
library(dplyr)
library(lubridate)

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

parse_betrag <- function(betrag) {
  if (is.na(betrag)) betrag <- 0
  betrag
}

einnahmen_vergleich <- function(dir, vgl, vgl_name = "Vorwoche") {
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
    "<span style='color: %s;'>%s%s%% %s</span></span> <span style='font-size:14px'>ggü. %s (%s)</span>",
    color, vorzeichen, abs(aenderung), icn, vgl_name, geldformat(vgl)
  ))
}

function(input, output, session) {
  heute <- Sys.Date()
  heute_monatsanfang <- as.Date(paste0(substr(as.character(heute), 1, 8), "01"))
  
  output$tbl <- renderTable({
    pool %>% tbl("verkauf") %>% arrange(desc(rechnungs_nr)) %>% head(10) %>% collect()
  })
  
  
  ## Tageseinnahmen:
  tageseinnahmen <- function(pool, anzahl_tage) {
    df <- pool %>% tbl("abrechnung_tag") %>%
      filter(date(zeitpunkt) == !!(heute - anzahl_tage)) %>%
      summarise(brutto = sum(mwst_netto, na.rm = TRUE) + sum(mwst_betrag, na.rm = TRUE)) %>% collect()
    parse_betrag(df$brutto)
  }
  
  tageseinnahmen_live <- function(pool) {
    # SELECT SUM(ges_preis) FROM kasse.verkauf_details INNER JOIN kasse.verkauf USING (rechnungs_nr) WHERE DATE(verkaufsdatum) = '2019-01-21';
    max_zp <- (pool %>% tbl("abrechnung_tag") %>%
      summarise(zp = max(zeitpunkt_real, na.rm = TRUE)) %>% collect())$zp
    df <- pool %>% tbl("verkauf_details") %>%
      inner_join(tbl(pool, "verkauf"), by = "rechnungs_nr") %>%
      filter(verkaufsdatum > max_zp & date(verkaufsdatum) <= heute) %>%
      summarise(brutto = sum(ges_preis, na.rm = TRUE)) %>% collect()
    parse_betrag(df$brutto)
  }
  
  tageseinnahmen_vergleich <- function(dir, vgl) {
    einnahmen_vergleich(dir, vgl, vgl_name = "Vorwoche")
  }
  
  output$tageseinnahmen_direkt1 <- renderUI({ geldformat(tageseinnahmen(pool, 0) + tageseinnahmen_live(pool)) })
  output$tageseinnahmen_direkt2 <- renderUI({ geldformat(tageseinnahmen(pool, 1)) })
  output$tageseinnahmen_direkt3 <- renderUI({ geldformat(tageseinnahmen(pool, 2)) })
  output$tageseinnahmen_direkt4 <- renderUI({ geldformat(tageseinnahmen(pool, 3)) })
  output$tageseinnahmen_vergleich1 <- renderUI({
    tageseinnahmen_vergleich(tageseinnahmen(pool, 0) + tageseinnahmen_live(pool),
                             tageseinnahmen(pool, 7))
  })
  output$tageseinnahmen_vergleich2 <- renderUI({
    tageseinnahmen_vergleich(tageseinnahmen(pool, 1),
                             tageseinnahmen(pool, 8))
  })
  output$tageseinnahmen_vergleich3 <- renderUI({
    tageseinnahmen_vergleich(tageseinnahmen(pool, 2),
                             tageseinnahmen(pool, 9))
  })
  output$tageseinnahmen_vergleich4 <- renderUI({
    tageseinnahmen_vergleich(tageseinnahmen(pool, 3),
                             tageseinnahmen(pool, 10))
  })
  
  
  ## Monatseinnahmen:
  monatseinnahmen <- function(pool, anzahl_monate) {
    df <- pool %>% tbl("abrechnung_monat") %>%
      filter(monat == !!(heute_monatsanfang - months(anzahl_monate))) %>%
      summarise(brutto = sum(mwst_netto, na.rm = TRUE) + sum(mwst_betrag, na.rm = TRUE)) %>% collect()
    parse_betrag(df$brutto)
  }
  
  monatseinnahmen_live <- function(pool) {
    # SELECT SUM(mwst_netto) + SUM(mwst_betrag) FROM kasse.abrechnung_tag WHERE YEAR(zeitpunkt) = 2019 AND MONTH(zeitpunkt) = 1;
    df <- pool %>% tbl("abrechnung_tag") %>%
      filter(zeitpunkt > heute_monatsanfang & zeitpunkt < !!(heute + 1)) %>%
      summarise(brutto = sum(mwst_netto, na.rm = TRUE) + sum(mwst_betrag, na.rm = TRUE)) %>% collect()
    parse_betrag(df$brutto) + tageseinnahmen_live(pool)
  }
  
  monatseinnahmen_vorjahr_live <- function(pool) {
    df <- pool %>% tbl("abrechnung_tag") %>%
      filter(zeitpunkt > !!(heute_monatsanfang - years(1)) &
               zeitpunkt < !!(heute - years(1) + 1)) %>%
      summarise(brutto = sum(mwst_netto, na.rm = TRUE) + sum(mwst_betrag, na.rm = TRUE)) %>% collect()
    parse_betrag(df$brutto)
  }
  
  monatseinnahmen_vergleich <- function(dir, vgl) {
    einnahmen_vergleich(dir, vgl, vgl_name = "Vorjahresmonat")
  }
  
  output$monatseinnahmen_direkt1 <- renderUI({
    HTML(paste0(
      geldformat(monatseinnahmen_live(pool)),
      " <span style='font-size:14px'>bis zum ",
      format(heute, "%d.%m."),
      "</span>"
    ))
  })
  output$monatseinnahmen_direkt2 <- renderUI({ geldformat(monatseinnahmen(pool, 1)) })
  output$monatseinnahmen_direkt3 <- renderUI({ geldformat(monatseinnahmen(pool, 2)) })
  output$monatseinnahmen_direkt4 <- renderUI({ geldformat(monatseinnahmen(pool, 3)) })
  output$monatseinnahmen_vergleich1 <- renderUI({
    einnahmen_vergleich(
      monatseinnahmen_live(pool), monatseinnahmen_vorjahr_live(pool),
      paste0("Vorjahr bis zum ", format(heute, "%d.%m."))
    )
  })
  output$monatseinnahmen_vergleich2 <- renderUI({
    monatseinnahmen_vergleich(monatseinnahmen(pool, 1), monatseinnahmen(pool, 13))
  })
  output$monatseinnahmen_vergleich3 <- renderUI({
    monatseinnahmen_vergleich(monatseinnahmen(pool, 2), monatseinnahmen(pool, 14))
  })
  output$monatseinnahmen_vergleich4 <- renderUI({
    monatseinnahmen_vergleich(monatseinnahmen(pool, 3), monatseinnahmen(pool, 15))
  })
  
  
  ## Jahreseinnahmen:
  jahreseinnahmen <- function(pool, anzahl_jahre) {
    df <- pool %>% tbl("abrechnung_jahr") %>%
      filter(jahr == (year(heute) - anzahl_jahre)) %>%
      summarise(brutto = sum(mwst_netto, na.rm = TRUE) + sum(mwst_betrag, na.rm = TRUE)) %>% collect()
    parse_betrag(df$brutto)
  }
  
  jahreseinnahmen_live <- function(pool) {
    df <- pool %>% tbl("abrechnung_monat") %>%
      filter(year(monat) == year(heute) & monat < heute_monatsanfang) %>%
      summarise(brutto = sum(mwst_netto, na.rm = TRUE) + sum(mwst_betrag, na.rm = TRUE)) %>% collect()
    parse_betrag(df$brutto) + monatseinnahmen_live(pool)
  }
  
  jahreseinnahmen_vorjahr_live <- function(pool) {
    df <- pool %>% tbl("abrechnung_monat") %>%
      filter(year(monat) == (year(heute) - 1) & monat < !!(heute_monatsanfang - years(1))) %>%
      summarise(brutto = sum(mwst_netto, na.rm = TRUE) + sum(mwst_betrag, na.rm = TRUE)) %>% collect()
    parse_betrag(df$brutto) + monatseinnahmen_vorjahr_live(pool)
  }
  
  jahreseinnahmen_vergleich <- function(dir, vgl) {
    einnahmen_vergleich(dir, vgl, vgl_name = "Vorjahr")
  }
  
  output$jahreseinnahmen_direkt1 <- renderUI({
    HTML(paste0(
      geldformat(jahreseinnahmen_live(pool)),
      " <span style='font-size:14px'>bis zum ",
      format(heute, "%d.%m."),
      "</span>"
    ))
  })
  output$jahreseinnahmen_direkt2 <- renderUI({ geldformat(jahreseinnahmen(pool, 1)) })
  output$jahreseinnahmen_direkt3 <- renderUI({ geldformat(jahreseinnahmen(pool, 2)) })
  output$jahreseinnahmen_direkt4 <- renderUI({ geldformat(jahreseinnahmen(pool, 3)) })
  output$jahreseinnahmen_vergleich1 <- renderUI({
    einnahmen_vergleich(
      jahreseinnahmen_live(pool), jahreseinnahmen_vorjahr_live(pool),
      paste0("Vorjahr bis zum ", format(heute, "%d.%m."))
    )
  })
  output$jahreseinnahmen_vergleich2 <- renderUI({
    jahreseinnahmen_vergleich(jahreseinnahmen(pool, 1), jahreseinnahmen(pool, 2))
  })
  output$jahreseinnahmen_vergleich3 <- renderUI({
    jahreseinnahmen_vergleich(jahreseinnahmen(pool, 2), jahreseinnahmen(pool, 3))
  })
  output$jahreseinnahmen_vergleich4 <- renderUI({
    jahreseinnahmen_vergleich(jahreseinnahmen(pool, 3), jahreseinnahmen(pool, 4))
  })
}
