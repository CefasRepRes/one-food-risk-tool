library(shiny)
library(ggplot2)
# Load packages
library(tidyverse)
library(viridis)
library(readxl)
library(here)
library(data.table)


ui <- fluidPage(
  titlePanel("One Food risk tool"),
  selectInput(inputId = "sector",
              label = "Sector",
              choices = c("Aquaculture",
                          "Crops",
                          "Fisheries",
                          "Livestock")),
  plotOutput("bar")
)

server <- function(input, output){
  # Import synthetic data
  hazard_scores <- read_xlsx(here::here("data",
                                        "data_structure.xlsx")) %>% data.table()
  updated_data <- reactive({hazard_scores[Sector == input$sector]})
  # Get colour palette
  sector_cols <- c("#56B4E9",
                            "#009E73",
                            "#0072B2",
                            "#F0E442")
                            
  names(sector_cols) <- c("Aquaculture",
                          "Crops",
                          "Fisheries",
                          "Livestock")
  # Plot
  output$bar <- renderPlot({
    ggplot(data = updated_data(), aes(x = Category, y = Score, fill = Sector)) +
      geom_col() +
      scale_fill_manual(values = sector_cols) +
      theme_light() +
      theme(text = element_text(size = 20),
            legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)

