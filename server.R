library(shiny)
library(shinymaterial)
library(jsonlite)
# library(RMySQL)
library(RMariaDB)
library(pool)
library(dplyr)
library(lubridate)
library(readr)
library(DT)

dbsetup <- jsonlite::read_json("~/.dbsetup.json")

pool <- dbPool(
  # drv = RMySQL::MySQL(),
  drv = RMariaDB::MariaDB(),
  host = dbsetup$kasse$host,
  dbname = dbsetup$kasse$db,
  username = dbsetup$kasse$user,
  password = dbsetup$kasse$pass
)

# Needed only for MySQL, not MariaDB:
# conn <- poolCheckout(pool)
# dbSendQuery(conn, "SET NAMES utf8")
# poolReturn(conn)

onStop(function() {
  poolClose(pool)
})

produktgruppen <- {
  p <- pool %>% tbl("produktgruppe") %>% filter(!is.na(toplevel_id) && aktiv) %>%
    select(produktgruppen_id, toplevel_id, sub_id, subsub_id, produktgruppen_name) %>% collect()
  sub_fill <- sapply(p$sub_id, function(sid) if (!is.na(sid)) " " else "")
  subsub_fill <- sapply(p$subsub_id, function(sid) if (!is.na(sid)) " " else "")
  ps <- as.list(p$produktgruppen_id)
  names(ps) <- paste0(sub_fill, subsub_fill, p$produktgruppen_name)
  ps
}

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
  
  output$produktgruppen_div <- renderUI({
    material_row(
      # Material design list dropdown not so nice, because one cannot search for items:
      # material_dropdown(
      #   "prod_group",
      #   "Wähle eine Produktgruppe",
      #   # produktgruppen,
      #   choices = produktgruppen,
      #   selected = NULL,
      #   multiple = FALSE
      # ),
      material_column(
        width = 4,
        selectInput(
          "prod_group",
          "Wähle eine Produktgruppe",
          # produktgruppen,
          choices = produktgruppen,
          selected = "Kunsthandwerk",
          multiple = FALSE,
          width = "100%"
        )
      ),
      material_column(
        width = 4,
        material_switch("include_subgroups", "Untergruppen einschließen?",
                        off_label = "Nein", on_label = "Ja", initial_value = TRUE)
      ),
      {
        min_zp <- (pool %>% tbl("verkauf") %>%
                     summarise(zp = date(min(verkaufsdatum, na.rm = TRUE))) %>% collect())$zp  
        material_column(
          width = 4,
          # material_date_picker("start", "Wähle einen Zeitraum"),
          # " - ",
          # material_date_picker("end", "")
          dateRangeInput("timerange", label = "Wähle einen Zeitraum",
                         start = Sys.Date() - 14, end = Sys.Date(),
                         min = min_zp, max = Sys.Date(),
                         separator = " - ", format = "dd.mm.yyyy",
                         language = "de", weekstart = 1,
                         width = "100%")
        )
      }
    )
  })
  
  selected_prod_group_ids <- reactive({
    prod_ids <- as.integer(input$prod_group)
    if (!is.null(input$include_subgroups) && input$include_subgroups) {
      ids <- pool %>% tbl("produktgruppe") %>%
        filter(produktgruppen_id == prod_ids) %>%
        select(toplevel_id, sub_id, subsub_id) %>% collect()
      if (is.na(ids$sub_id)) {
        prod_ids <- c(
          prod_ids,
          (pool %>% tbl("produktgruppe") %>%
             filter(toplevel_id == ids$toplevel_id) %>%
             select(produktgruppen_id) %>% collect())$produktgruppen_id
        )
      } else if (is.na(ids$subsub_id)) {
        prod_ids <- c(
          prod_ids,
          (pool %>% tbl("produktgruppe") %>%
             filter(toplevel_id == ids$toplevel_id & sub_id == ids$sub_id) %>%
             select(produktgruppen_id) %>% collect())$produktgruppen_id
        )
      }
    }
    return(unique(as.integer(prod_ids)))
  })
  
  output$verkaufstabelle <- DT::renderDataTable({
    req(input$timerange[1])
    df <- pool %>% tbl("verkauf_details") %>%
      inner_join(tbl(pool, "verkauf"), by = "rechnungs_nr") %>%
      inner_join(tbl(pool, "artikel"), by = "artikel_id") %>%
      inner_join(tbl(pool, "lieferant"), by = "lieferant_id") %>%
      filter(verkaufsdatum >= input$timerange[1] &
               verkaufsdatum <= input$timerange[2] &
               produktgruppen_id %in% !!selected_prod_group_ids()) %>% # c(18, 19, 20)
      group_by(lieferant_id, lieferant_name, artikel_nr) %>%
      summarise(umsatz_stueck = sum(stueckzahl, na.rm = TRUE),
                umsatz_geld = sum(ges_preis, na.rm = TRUE)) %>%
      arrange(desc(umsatz_stueck)) %>% collect()
    df$artikel_name <- sapply(seq_len(nrow(df)), function(i)
      (pool %>% tbl("artikel") %>%
         filter(lieferant_id == df$lieferant_id[i] && artikel_nr == df$artikel_nr[i]) %>%
         arrange(desc(artikel_id)) %>% select(artikel_name) %>% head(1) %>% collect())$artikel_name)
    # Select and rename the relevant rows:
    df <- select(
      df,
      Lieferant = lieferant_name, `Art.-Nr.` = artikel_nr,
      Bezeichnung = artikel_name,
      `Umsatz (Stückzahl)` = umsatz_stueck, `Umsatz (Euro)` = umsatz_geld
    )
    df$lieferant_id <- NULL
    
    DT::datatable(df, rownames = FALSE)
  })
}
