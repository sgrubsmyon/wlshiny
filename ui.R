#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(lubridate)

tagesnamen <- c("Heute", "Gestern", "Vorgestern", "Vorvorgestern")
heute <- Sys.Date()
heute_monatsanfang <- as.Date(paste0(substr(as.character(heute), 1, 8), "01"))

# Define UI for application that draws a histogram
# Wrap shinymaterial apps in material_page
material_page(
  title = "Weltladen Bonn Kassenauswertung",
  nav_bar_fixed = TRUE,
  include_fonts = TRUE,
  nav_bar_color = "teal lighten-1",
  material_side_nav(
    fixed = TRUE,
    image_source = "Weltladen_Logo.png",
    span(style = "display: none;", icon("caret-up")), # load the fontawesome icons dependencies
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Übersicht Einnahmen" = "einnahmen",
        "Verkaufsstatistik" = "verkauf"
      ),
      icons = c("dashboard", "dehaze")
    )
    # material_row(
    #   material_column(
    #     width = 7,
    #     material_radio_button(
    #       "adjType",
    #       label = "",
    #       choices = c("Percent Change" = "close_p", "Price in $USD" = "close")
    #     )
    #   ),
    #   material_column(
    #     width = 5,
    #     material_radio_button(
    #       "day_back",
    #       label = "",
    #       choices = c("1 Week" = 7, "1 Month" = 30, "1 Year" = 365)
    #     )
    #   )
    # ),
    # tags$br(),
    # material_row(
    #   material_column(
    #     offset = 1,
    #     width = 5,
    #     material_switch("BTC", 
    #                     label = "Bitcoin",
    #                     initial_value = TRUE,
    #                     color = "#9c27b0")
    #   ),
    #   material_column(
    #     width = 5,
    #     material_switch("BCH",
    #                     label = "Bitcoin Cash", 
    #                     initial_value = TRUE,
    #                     color = "#9c27b0")
    #   )
    # ),
    # material_row(
    #   material_column(
    #     offset = 1,
    #     width = 5,
    #     material_switch("ETH",
    #                     label = "Ethereum", 
    #                     initial_value = TRUE,
    #                     color = "#9c27b0")
    #   ),
    #   material_column(
    #     width = 5,
    #     material_switch("LTC", 
    #                     label = "Litecoin",
    #                     initial_value = TRUE,
    #                     color = "#9c27b0")
    #   )
    # )
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "einnahmen",
    material_tabs(
      tabs = c(
        "Jahreseinnahmen" = "jahreseinnahmen",
        "Monatseinnahmen" = "monatseinnahmen",
        "Tageseinnahmen" = "tageseinnahmen",
        "Trend" = "trend"
      )
    ),
    material_tab_content(
      tab_id = "jahreseinnahmen",
      h4("Jahreseinnahmen"),
      material_row(
        tagList(
          lapply(1:4, function(i) {
            material_column(
              width = 3,
              material_card(
                title = HTML(paste0(
                  "<span style='font-weight:bold; color:", "#9c27b0", "'>",
                  year(heute) + 1 - i, "</span>"
                )),
                depth = 3,
                HTML(paste0(
                  "<div class='text-right'><span style='font-size:28px'>",
                  uiOutput(paste0("jahreseinnahmen_direkt", i)),
                  "</span></div>",
                  "<div class='text-right'><span style='font-size:28px'>",
                  uiOutput(paste0("jahreseinnahmen_vergleich", i)),
                  "</div>"
                ))
              )
            )
          })
        )        
      )
    ),
    material_tab_content(
      tab_id = "monatseinnahmen",
      h4("Monatseinnahmen"),
      material_row(
        tagList(
          lapply(1:4, function(i) {
            material_column(
              width = 3,
              material_card(
                title = HTML(paste0(
                  "<span style='font-weight:bold; color:", "#9c27b0", "'>",
                  format(heute_monatsanfang - months(i - 1), "%B %Y"), "</span>"
                )),
                depth = 3,
                HTML(paste0(
                  "<div class='text-right'><span style='font-size:28px'>",
                  uiOutput(paste0("monatseinnahmen_direkt", i)),
                  "</span></div>",
                  "<div class='text-right'><span style='font-size:28px'>",
                  uiOutput(paste0("monatseinnahmen_vergleich", i)),
                  "</div>"
                ))
              )
            )
          })
        )        
      )
    ),
    material_tab_content(
      tab_id = "tageseinnahmen",
      h4("Tageseinnahmen"),
      material_row(
        tagList(
          lapply(1:4, function(i) {
            material_column(
              width = 3,
              material_card(
                title = HTML(paste0(
                  "<span style='font-weight:bold; color:", "#9c27b0", "'>",
                  tagesnamen[i], " (", format(heute + 1 - i, "%A"), ")</span>",
                  "<br>",
                  "<span style='font-size:14px'>", format(heute + 1 - i, "%x"), "</span>"
                )),
                depth = 3,
                HTML(paste0(
                  "<div class='text-right'><span style='font-size:28px'>",
                  uiOutput(paste0("tageseinnahmen_direkt", i)),
                  "</span></div>",
                  "<div class='text-right'><span style='font-size:28px'>",
                  uiOutput(paste0("tageseinnahmen_vergleich", i)),
                  "</div>"
                ))
              )
            )
          })
        )
      )
    ),
    material_tab_content(
      tab_id = "trend",
      h4("Trend"),
      material_card(
        depth = 3,
        tableOutput("tbl"),
        uiOutput('hide_gear')
      )
    )
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "verkauf",
    uiOutput("produktgruppen_div")
  )
)
