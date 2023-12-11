library(shiny)
library(plotly)
library(dplyr)

# Load your dataset
df <- read.csv("Group_Proj_df.csv") 

ui <- fluidPage(
  titlePanel("Fast Food Influence on Obesity"),
  
  sidebarLayout(
    sidebarPanel(
      # Add slider input for X-axis range selection
      sliderInput("xRange", "Select X-axis Range:",
                  min = min(df$X2023.Q1), max = max(df$X2023.Q1),
                  value = c(min(df$X2023.Q1), max(df$X2023.Q1)), step = 0.1)
    ),
    
    mainPanel(
      plotlyOutput("bubbleChart"),
      div(style = "margin-top: 30px;"),  # Add space between graph and text
      HTML('<p>The link between the density of fast food chains and the rates of obesity in the 
                      different states included in the dataset is shown in this visualization. The density 
                      of fast food chains is represented by the X-axis, which shows the total count as well 
                      as the individual counts of various fast food franchises. The chart shows the reported 
                      obesity rates for each state on the Y-axis. The bubbles provide a visual depiction of 
                      their significance or scale within the dataset; their size and color correlate to the 
                      GDP or population size of the state. The chart\'s bubbles, each representing a state, 
                      enable quick comprehension of the relationship between the presence of fast food and 
                      obesity rates while taking into account the demographic or economic context denoted by 
                      the bubbles\' size and color.</p>')
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Create reactive subset of data based on selected X-axis range
  filtered_df <- reactive({
    df %>%
      filter(X2023.Q1 >= input$xRange[1] & X2023.Q1 <= input$xRange[2])
  })
  
  # Create Bubble Chart
  output$bubbleChart <- renderPlotly({
    plot_ly(filtered_df(), x = ~X2023.Q1, y = ~Obesity, color = ~GDP_Category,
            text = ~paste("State: ", NAME, "<br>GDP Category: ", GDP_Category, "<br>Population: ", Population.2022),
            size = ~Population.2022, sizes = c(5, 100)) %>%
      layout(xaxis = list(title = "Fast Food Chain Density"),
             yaxis = list(title = "Obesity Rate"),
             title = "Fast Food Chain Density vs Obesity Rate")
  })
}

# Run the application
shinyApp(ui = ui, server = server)