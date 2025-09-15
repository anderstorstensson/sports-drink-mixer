#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Sports Drink Ingredient Calculator"),

  sidebarLayout(
    sidebarPanel(
      numericInput("servings", "Number of servings:", value = 20, min = 1),
      numericInput("serving_size", "Serving size (g):", value = 80, min = 1),
      numericInput("ratio_malto", "Maltodextrin ratio part:", value = 1, min = 0, step = 0.1),
      numericInput("ratio_fruct", "Fructose ratio part:", value = 0.8, min = 0, step = 0.1),
      numericInput("salt", "Electrolytes/salt per serving (g):", value = 1, min = 0, step = 0.1)
    ),

    mainPanel(
      h3("Ingredients Needed"),
      tableOutput("ingredient_table")
    )
  ),
  # Footer with GitHub link
  tags$footer(
    tags$hr(),
    "Source code available on ",
    tags$a(href = "https://github.com/anderstorstensson/sports-drink-mixer", "GitHub"),
    align = "center",
    style = "
      position:fixed;
      bottom:0;
      width:100%;
      padding:10px;
      background:#f8f9fa;
      color:#555;
    "
  )
)

server <- function(input, output) {

  output$ingredient_table <- renderTable({

    # total available for carbs after salt
    carb_total <- input$serving_size - input$salt

    # normalize ratio
    ratio_sum <- input$ratio_malto + input$ratio_fruct
    malto_per_serv <- carb_total * (input$ratio_malto / ratio_sum)
    fruct_per_serv <- carb_total * (input$ratio_fruct / ratio_sum)

    # kcal per gram
    kcal_malto <- 3.8
    kcal_fruct <- 4.0

    # kcal per serving
    kcal_per_serv <- malto_per_serv * kcal_malto + fruct_per_serv * kcal_fruct

    tibble(
      Ingredient = c("Maltodextrin", "Fructose", "Electrolytes/salt", "Total kcal"),
      `Per Serving` = c(
        round(malto_per_serv, 1),
        round(fruct_per_serv, 1),
        round(input$salt, 1),
        round(kcal_per_serv, 1)
      ),
      `Total (all servings)` = c(
        round(malto_per_serv * input$servings, 1),
        round(fruct_per_serv * input$servings, 1),
        round(input$salt * input$servings, 1),
        round(kcal_per_serv * input$servings, 1)
      ),
      Unit = c("g", "g", "g", "kcal")
    )

  }, digits = 1)
}

# Run the application
shinyApp(ui = ui, server = server)
