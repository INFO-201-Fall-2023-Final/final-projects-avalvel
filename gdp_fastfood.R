# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("Group_Proj_df.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Economic Impact: GDP and Fast Food Chains"),
  tags$img(src = "mcdonalds-money.jpeg", width = 200, height = 200),
  sidebarLayout(
    sidebarPanel(
      selectInput("fastfood", "Select a Fast Food Chain", choices = names(data)[10:27])
    ),
    mainPanel(
      plotOutput("plot")
    )
  ),
  # Description of visualization
  fluidRow(
    column(
      width = 12,
      p("This visualizes the interplay between economic growth, measured by GDP 
        categories, and the presence or impact of various fast food chains within 
        different regions or territories of the US. By selecting a specific 
        fast-food chain from the dropdown menu, users can explore how the prevalence 
        or sales of that chain correlates with different levels of economic development 
        across regions. The visualization aims to illustrate potential relationships or 
        dependencies between the economic landscape, as represented by GDP categories, 
        and the market presence or popularity of individual fast-food brands within those areas.")
    ),
    column(
      width = 12,
      p("The app essentially offers a lens into how economic prosperity or shifts 
        in GDP categories might influence consumer behavior or the expansion of fast-food chains. 
        It allows users to explore whether regions experiencing higher GDP categories tend to attract 
        or sustain a broader presence of certain fast-food brands, shedding light on the potential 
        socioeconomic factors influencing the growth or saturation of these chains in different parts of the US.")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(data, aes_string(x = "GDP_Category", y = input$fastfood)) +
      geom_boxplot(fill = "lightblue") +
      labs(x = "GDP Category", y = input$fastfood)
  })
}

# Run the application
shinyApp(ui = ui, server = server)