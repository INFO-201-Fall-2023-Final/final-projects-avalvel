library(dplyr)
library(stringr)
library(ggplot2)

nat_ob_df <- read.csv("National_Obesity_By_State.csv") 
GDP_df <- read.csv("SQGDP1__ALL_AREAS_2005_2023.csv") 
FF_df <- read.csv("fast_food.csv")

df1 <- merge(nat_ob_df, GDP_df, by.x = "NAME", by.y = "GeoName")
df <- merge(df1, FF_df, by.x = "NAME", by.y = "State")
df <- subset(df, select = c(NAME, Obesity, Region, LineCode, Description, X2023.Q1, Population.2022, McDonald.s, 
                            Starbucks, Chick.fill.A, Taco.Bell, Wendy.s, Dunkin.Donuts, Burger.King, Subway, 
                            Domino.s, Chipotle, Sonic..Drive.In, Pizza.Hut, Panda.Express, KFC, Dairy.Queen, Arby.s, 
                            Buffalo.Wild.Wings, Whataburger, Five.Guys, In.N.Out.Burger))
df <- subset(df, LineCode != "1")
df <- subset(df, LineCode != "2")

#Adding GDP category for states
df <- df[order(df$X2023.Q1, decreasing = TRUE), ]
df$GDP_Category <- case_when(
  df$X2023.Q1 > 500000 ~ "High GDP",
  df$X2023.Q1 > 200000 ~ "Medium GDP",
  df$X2023.Q1 > 0 ~ "Low GDP"
)

#Adding Region
state_to_region <- function(state_name) {
  regions <- list(
    "West" = c("Washington", "Oregon", "Idaho", "Alaska", "California", "Colorado",
               "Nevada","Hawaii","Utah", "Montana", "Wyoming"),
    "Southwest" = c("Arizona", "Texas", "New Mexico", "Oklahoma"),
    "Midwest" = c("Ohio", "Michigan", "Illinois", "Indiana", "Wisconsin", "Iowa", "Kansas",
                  "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"),
    "Northeast" = c("New York", "Pennsylvania", "Massachusetts", "Connecticut", "Maine", 
                    "New Hampshire", "Rhode Island", "Vermont", "New Jersey"),
    "Southeast" = c("Florida", "Georgia", "Alabama", "Mississippi", "Arkansas", "North Carolina", 
                    "South Carolina", "Virginia", "West Virgina", "Kentucky", "Tennessee",
                    "Delaware","Maryland", "Louisiana")
  )
  
  for (region in names(regions)) {
    if (state_name %in% regions[[region]]) {
      return(region)
    }
  }
  return(NA)
}
df<- df %>%
  mutate(US.Territory.Region = sapply(NAME, state_to_region)) %>%
  select(NAME, US.Territory.Region, everything())

#Summarization
summary_df <- aggregate(X2023.Q1 ~ GDP_Category, data = df, FUN = mean)
colnames(summary_df) <- c("GDP_Category", "Average_GDP")

#Sum of all fastfood resturants per state
df[, 9:28] <- lapply(df[, 9:28], as.numeric)
df$Sum <- rowSums(df[, 9:28], na.rm = TRUE)


#Save df as CSV
write.csv(df, "Group_Proj_df.csv")

