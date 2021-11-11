#local directory - advait
setwd("C:/Users/advai/Documents/Georgetown/7 - 2021 Fall/DataViz/Project 2")

library(tidyverse)
library(maps)
library(mapproj)
library(lubridate) #access lubridate to modify dates and times
library(biscale)
library(sf) # spatial data handling
library(sp)
library(cowplot)
library(ggthemes)

original_data <- read_csv("USA-BEA.csv",col_names = F)

names_data_frame <- original_data[1:3,]
original_data <- original_data[-c(1,2,3),]
original_data <- original_data[1:78,]
names(original_data)[1] <- "Country"
original_data <- original_data[-c(3, 26, 27, 28, 36, 37, 43, 44, 52, 53, 54, 58, 59, 63, 64, 78),]

naming_info <- NULL
for(i in 1:ncol(names_data_frame)){
  naming_info[i] <- paste(" :",names_data_frame[3,i],
                          ":",names_data_frame[1,i],
                          ":",names_data_frame[2,i])
}
naming_info

names(original_data) <- naming_info

gathered_data <- gather(original_data, key, value, -1)

prep <- mutate(gathered_data, 
               Year = str_split_fixed(key, " : ", 4)[,2],
               Industry = str_split_fixed(key, " : ", 4)[,3],
               Subcategory = str_split_fixed(key, " : ", 4)[,4]
)

names(prep)[1] <- "Country"

usa <- data.frame(Country = prep$Country, Year = prep$Year, Industry = prep$Industry, Subcategory = prep$Subcategory, Investment = prep$value)

usa$Investment <- as.numeric(usa$Investment)
usa$Industry <- as.factor(usa$Industry)
usa$Subcategory <- as.factor(usa$Subcategory)
usa$Year <- as.factor(usa$Year)

usa[is.na(usa)] <- 0

world_map <- map_data("world")
map_names <- levels(as.factor(world_map$region))

usa_names <- levels(as.factor(usa$Country))

usa_names[which( !(usa_names %in% map_names))]

usa <- mutate(usa, Country=fct_recode(Country,
                                      "Palestine" = "West Bank and Gaza",
                                      "Syria" = "Syrian Arab Republic",
                                                          "Antigua" = "Antigua and Barbuda",
                                                          "Bosnia and Herzegovina" = "Bosnia",
                                                          "Republic of Congo" = "Congo",
                                                          "Trinidad" = "Trinidad and Tobago",
                                                          "Syria" = "Syrian Arab Republic",
                                                          "Kyrgyzstan" = "Kyrgyz Republic",
                                                          "United Arab Emirates" = "UAE",
                                                          "Ivory Coast" = "Côte d'Ivoire",
                                                          "Macedonia" = "North Macedonia", 
                                                          "UK" = "United Kingdom"))



usa_condensed <- summarize(group_by(usa, Country, Industry, Subcategory),
                           investment = sum(Investment))

usa_total <- filter(usa_condensed, Industry == "All Industries Total")
usa_total <- rename(usa_total, usa_quantity = "investment")

alex_data <- read_csv("data_adj.csv")

fulldata <- full_join(alex_data, usa_total, by = "Country")

fulldata <- mutate(fulldata, usa_quantity = usa_quantity*1000000)
fulldata <- mutate(fulldata, usa_quantity_per_million = (usa_quantity/Population)*1000000)


write.csv(fulldata, "fulldata.csv")
