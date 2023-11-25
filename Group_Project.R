library(dplyr)
library(stringr)
library(ggplot2)

nat_ob_df <- read.csv("National_Obesity_By_State.csv") 
GDP_df <- read.csv("SQGDP1__ALL_AREAS_2005_2023.csv") 
FF_df <- read.csv("fast_food.csv")
df1 <- merge(nat_ob_df, GDP_df, by.x = "NAME", by.y = "GeoName")
df <- merge(df1, FF_df, by.x = "NAME", by.y = "State")
df <- subset(df, select = c(NAME, Obesity, Region, Description, X2023.Q1, Population.2022, McDonald.s, Starbucks, Chick.fill.A, Taco.Bell, Wendy.s, Dunkin.Donuts, Burger.King, Subway, Domino.s, Chipotle, Sonic..Drive.In, Pizza.Hut, Panda.Express, KFC, Dairy.Queen, Arby.s, Buffalo.Wild.Wings, Whataburger, Five.Guys, In.N.Out.Burger))
df <- subset(df, LineCode != "1")
df <- subset(df, LineCode != "2")


