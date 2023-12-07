library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Fast Food Influence on Obesity"),
  
  sidebarLayout(
    sidebarPanel(
      # Add filter inputs for users
      sliderInput("obesityFilter", "Filter by Obesity Rate:", min = 0, max = 50, value = c(0, 50)),
      selectInput("gdpFilter", "Filter by GDP Category:", choices = unique(df$GDP_Category)),
    ),
    
    mainPanel(
      plotlyOutput("bubbleChart")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive data filtering based on user inputs
  filtered_df <- reactive({
    df_filtered <- df[df$Obesity >= input$obesityFilter[1] & df$Obesity <= input$obesityFilter[2] &
                        df$GDP_Category == input$gdpFilter, ]
    return(df_filtered)
  })
  
  # Create Bubble Chart
  output$bubbleChart <- renderPlotly({
    # Use the filtered data
    df_filtered <- filtered_df()
    
    plot_ly(df_filtered, x = ~McDonald.s, y = ~Obesity,
            text = ~paste("State: ", NAME, "<br> GDP Category: ", GDP_Category, "<br> Population: ", Population.2022),
            marker = list(size = ~Population.2022, color = ~GDP_Category, colorscale = "Viridis", 
                          colorbar = list(title = "GDP Category")),
            mode = "markers"
    ) %>%
      layout(xaxis = list(title = "Fast Food Chain Density"),
             yaxis = list(title = "Obesity Rate"),
             title = "Fast Food Chain Density vs Obesity Rate")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
