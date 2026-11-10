# R Shiny Lab — STEP 4: Exploring US Murder Rates v2
# Based on the user's uploaded 19_Shiny_Lab_part_1(1).R
# Install required packages in the RStudio Console before running this app.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(dslabs)

data(murders)

murders_r <- murders %>% mutate(rate = total / population * 1e5)

ui <- page_sidebar(
  title = "Exploring US Murder Rates v2",
  sidebar = sidebar(
    selectInput("selected_region", "Region",
                choices = unique(murders_r$region)),
    sliderInput("pop_range", "Population range",
                min = 0, max = max(murders_r$population),
                value = range(murders_r$population), sep = ","),
    radioButtons("y_var", "y-axis variable",
                 choices = c("Number of murders" = "total",
                             "Murders per 100,000" = "rate")),
    checkboxInput("show_lm", "Show regression line", value = FALSE)
  ),
  textOutput("summary_text"),
  layout_columns(
    col_widths = c(7, 5),
    card(plotOutput("murder_plot")),
    card(tableOutput("result"))
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    murders_r %>%
      filter(region == input$selected_region,
             population >= input$pop_range[1],
             population <= input$pop_range[2])
  })

  output$summary_text <- renderText({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "No states selected."))
    paste0(nrow(d), " states selected in the ", input$selected_region, " region; ",
           "the average murder rate is ", round(mean(d$rate), 2), " per 100,000.")
  })

  output$murder_plot <- renderPlot({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "No states match these conditions. Try a wider population range."))

    # .data[[string]] : use an input value (a string) as a variable name
    p <- ggplot(d, aes(population, .data[[input$y_var]])) +
      geom_point(color = "#B57A23", size = 3) +
      geom_text(aes(label = abb), vjust = -1, size = 3) +
      labs(x = "Population", y = input$y_var) +
      theme_minimal()

    if (input$show_lm) {
      p <- p + geom_smooth(method = "lm", formula = y ~ x)
    }
    p
  })

  output$result <- renderTable({
    filtered_data() %>% select(state, population, total, rate)
  })
}

shinyApp(ui, server)
