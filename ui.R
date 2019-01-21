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
    material_row(
      material_column(
        width = 7,
        material_radio_button(
          "adjType",
          label = "",
          choices = c("Percent Change" = "close_p", "Price in $USD" = "close")
        )
      ),
      material_column(
        width = 5,
        material_radio_button(
          "day_back",
          label = "",
          choices = c("1 Week" = 7, "1 Month" = 30, "1 Year" = 365)
        )
      )
    ),
    tags$br(),
    material_row(
      material_column(
        offset = 1,
        width = 5,
        material_switch("BTC", 
                        label = "Bitcoin",
                        initial_value = TRUE,
                        color = "#9c27b0")
      ),
      material_column(
        width = 5,
        material_switch("BCH",
                        label = "Bitcoin Cash", 
                        initial_value = TRUE,
                        color = "#9c27b0")
      )
    ),
    material_row(
      material_column(
        offset = 1,
        width = 5,
        material_switch("ETH",
                        label = "Ethereum", 
                        initial_value = TRUE,
                        color = "#9c27b0")
      ),
      material_column(
        width = 5,
        material_switch("LTC", 
                        label = "Litecoin",
                        initial_value = TRUE,
                        color = "#9c27b0")
      )
    )
  ),
  tags$br(),
  material_row(
    tagList(
      lapply(1:4, function(i) {
        material_column(
          width = 3,
          material_card(
            title = HTML(
              paste0(
                "<span style='font-weight:bold; color:", "#9c27b0", "'>", "Interessante Zahl", "</span>&nbsp;&nbsp;",
                "<span style='font-size:14px'>", "Hier könnte ein Hinweis stehen", "</span>"
              )
            ),
            depth = 3,
            HTML(
              paste0(
                "<div class='text-right'><span style='font-size:28px'>",
                "+",
                42,
                "%"
              ),
              "</span></div>"
            )
          )
        )                 
      })
    )
  ),
  material_row(
    material_column(
      width = 12,
      material_card(
        depth = 3,
        plotOutput("crypto"),
        uiOutput('hide_gear')
      )
    )
  )
)