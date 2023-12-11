library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(DT)  

df <- read.csv("Group_Proj_df.csv")

state_mapping <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
            "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
            "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
            "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", 
           "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
           "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

df <- left_join(df, state_mapping, by = c("NAME" = "state"))

ui <- fluidPage(
  titlePanel("Fast Food & Socioeconomic Status"),
  br(),
  
  tabsetPanel(
    tabPanel("Home",
             h3("Introduction"),
             p("Welcome to our platform, where we embark on a journey to uncover the intricate relationship 
               between fast food consumption and socioeconomic status. Our project seeks to illuminate a 
               concerning correlation that ties the frequency of indulging in fast or junk food to one's 
               economic well-being. While the allure of quick and affordable meals may be universal, the 
               implications of relying on such options reveal disparities in economic privilege. For some, fast 
               food remains a choice, while for others, it becomes a necessity dictated by financial constraints 
               and time limitations.The spatial targeting of fast-food establishments in economically 
               disadvantaged areas draws attention to the systemic inequalities perpetuated by the food industry. 
               This endeavor delves into the societal dilemma of how income levels influence 
               dietary habits, potentially compromising long-term health outcomes. By unraveling the underlying 
               factors driving fast food consumption, we aim to contribute to the discourse on creating a more 
               equitable and accessible food environment for all."),
             br(),
             p("In our exploration, we delve into the multifaceted relationship between fast food, economic status, 
               and regional patterns across the United States. The visualizations presented include a map showcasing 
               regional obesity rates and fast-food density, revealing a correlation between the concentration of 
               fast-food establishments and higher obesity rates. Another visualization employs a chart, mapping the 
               density of fast-food chains against obesity rates in different states, considering demographic and 
               economic factors through bubble size and color. Additionally, a Shiny app allows users to explore how 
               the presence of specific fast-food chains correlates with varying levels of economic development across 
               regions. This lens into the interplay between economic prosperity and fast-food market dynamics sheds 
               light on potential socioeconomic influences, contributing to a broader discourse on creating a more 
               equitable and accessible food environment nationwide."),
             hr(),
             h5("For more information vist..."),
             a("National Library of Medicine", href = "https://pubmed.ncbi.nlm.nih.gov/28472714/"),
             br(),
             a("BMC Public Health", href = "https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-022-13826-1"),
             br(),
             a("NY Times", href = "https://www.nytimes.com/2023/02/28/opinion/processed-food-social-class-america.html"),
             br(),
             a("Science Direct", href = "https://www.sciencedirect.com/science/article/abs/pii/S1570677X16300363")
    ),
    
    tabPanel("US Regional Analysis",
             h3("US Regional Analysis"),
             p("The displayed map reveals distinct regional patterns in obesity rates and fast-food restaurant density 
    across the United States. Notably, the Southeast stands out with the highest obesity rates and a considerable 
    concentration of fast-food restaurants per capita. Following this, the Midwest, Southwest, Northeast, and 
    West exhibit varying degrees of obesity and fast-food prevalence, with a gradual decline observed from the 
    Southeast to the West. The map underscores a notable correlation between the density of fast-food 
    establishments and higher obesity rates, suggesting a potential link between dietary habits and health 
    outcomes. This observation aligns with the general understanding that regions with a higher concentration 
    of fast-food options tend to experience elevated obesity rates, highlighting the complex interplay between 
    lifestyle factors and health outcomes on a regional scale."),
             br(),
             h4("Iteractive Map"),
             br(),
             
             plotlyOutput("map"),
             br(),
             
             h4("Summary Table"),
             br(),
             
             DTOutput("summaryTable")
    ),
    
    tabPanel("Fast Food Influence on Obesity",
             h3("Fast Food Influence on Obesity"),
             sidebarLayout(
               sidebarPanel(
                 # Add slider input for X-axis range selection
                 sliderInput("xRange", "Fast Food Chain Density:",
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
    ),
    
    tabPanel("Economic Impact",
             fluidPage(
               titlePanel("Economic Impact: GDP and Fast Food Chains"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("fastfood", "Select a Fast Food Chain", choices = names(df)[10:27])
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
    )
    
    )
  )
  

# Server
server <- function(input, output) {
  # Map
  output$map <- renderPlotly({
    df %>%
      plot_ly(
        type = 'choropleth',
        locationmode = 'USA-states',
        locations = ~code,  
        z = ~Obesity,
        text = ~paste(NAME, "<br>Obesity Rate: ", Obesity, "%<br>Fast Food Count: ", Sum, 
                      "<br>Fast Food Per 100k: ", round((Sum / Population.2022) * 100000)),
        colorscale = 'YlOrRd_r',  # Reverse the color scale
        colorbar = list(title = 'Obesity')
      ) %>%
      layout(
        geo = list(
          scope = 'usa',
          showlakes = TRUE,
          lakecolor = toRGB('white')
        )
      )
  })
  
  # Summary Table
  output$summaryTable <- renderDT({
    summary_data <- df %>%
      group_by(`US.Territory.Region`) %>%
      summarize(
        Average_Obesity_Rate = round(mean(Obesity)),
        Average_Fast_Food_Per_State = round(mean(Sum)),
        Average_Fast_Food_Per_100k_People = round(median((Sum / Population.2022) * 100000))
      )
    
    datatable(summary_data, options = list(pageLength = 5))
  })
  
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
  
  output$plot <- renderPlot({
    ggplot(df, aes_string(x = "GDP_Category", y = input$fastfood)) +
      geom_boxplot(fill = "lightblue") +
      labs(x = "GDP Category", y = input$fastfood)
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)