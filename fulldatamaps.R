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

data <- read_csv("fulldata.csv")
data <- select(data, -X1)
data$Region <- as.factor(data$Region)
data$Industry <- as.factor(data$Industry)
data$Subcategory <- as.factor(data$Subcategory)

data$quantity[is.na(data$quantity)] <- 0
data$total_doses_delivered[is.na(data$total_doses_delivered)] <- 0
data$doses_per_million[is.na(data$doses_per_million)] <- 0
data$quantity_per_million[is.na(data$quantity_per_million)] <- 0
data$usa_quantity[is.na(data$usa_quantity)] <- 0
data$usa_quantity_per_million[is.na(data$usa_quantity_per_million)] <- 0

bidata <- bi_class(data, x = quantity, y = total_doses_delivered, style = "jenks", dim = 3)
bidata <- rename(bidata, Investment_bi_Vaccine = bi_class)

bidata <- bi_class(bidata, x = usa_quantity, y = total_doses_delivered, style = "jenks", dim = 3)
bidata <- rename(bidata, USInvestment_bi_Vaccine = bi_class)

bidata <- bi_class(bidata, x = quantity, y = usa_quantity, style = "jenks", dim = 3)
bidata <- rename(bidata, Investment_bi_USInvestment = bi_class)

#MAP PREP
world_map <- map_data("world")
biworld <- full_join(bidata, world_map, by=c("Country"="region"))

#biworlddata <- filter(biworlddata, Country != "Côte d'Ivoire")
#biworlddata <- filter(biworlddata, Country != "North Macedonia")

#bivariate map
biworldmap1 <- ggplot() + geom_polygon(data=biworld, aes(x=long, y=lat, group=group, fill=Investment_bi_Vaccine), color="grey50", show.legend = FALSE, size = 0.1) +
  coord_map("albers", parameters=c(0,0)) +
  bi_scale_fill(pal = "DkViolet", dim = 3) + bi_theme() +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  labs(title = "Do Chinese Vaccines follow Chinese Investment?", subtitle = "Dark Blue (DkViolet) Palette")

legend1 <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "Higher Investment ",
                    ylab = "Higher Vaccine Disbursement",
                    size = 8)

finalPlot1 <- ggdraw() +
  draw_plot(biworldmap1, 0, 0, 1, 1) +
  draw_plot(legend1, 0, .0, 0.4, 0.4, scale = 1)

finalPlot1

#bivariabe map 2
sseasia <- filter(biworld, Region == "SE Asia" | Region == "South Asia")

biworldmap2 <- ggplot() + geom_polygon(data=filter(sseasia), aes(x=long, y=lat, group=group, fill=Investment_bi_Vaccine), color="grey50", show.legend = FALSE, size = 0.1) +
  coord_map("albers", parameters=c(0,0)) +
  bi_scale_fill(pal = "DkViolet", dim = 3) + bi_theme() +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  labs(title = "Do Chinese Vaccines follow Chinese Investment?", subtitle = "Dark Blue (DkViolet) Palette")

legend2 <- bi_legend(pal = "DkViolet",
                     dim = 3,
                     xlab = "Higher Investment ",
                     ylab = "Higher Vaccine Disbursement",
                     size = 8)

finalPlot2 <- ggdraw() +
  draw_plot(biworldmap2, 0, 0, 1, 1) +
  draw_plot(legend2, 0, .0, 0.4, 0.4, scale = 1)

finalPlot2

#bivariabe map 3
lac <- filter(biworld, Region == "Latin America")

biworldmap3 <- ggplot() + geom_polygon(data=filter(lac), aes(x=long, y=lat, group=group, fill=Investment_bi_Vaccine), color="grey50", show.legend = FALSE, size = 0.1) +
  coord_map("albers", parameters=c(0,0)) +
  bi_scale_fill(pal = "DkViolet", dim = 3) + bi_theme() +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  labs(title = "Do Chinese Vaccines follow Chinese Investment?", subtitle = "Dark Blue (DkViolet) Palette")

legend3 <- bi_legend(pal = "DkViolet",
                     dim = 3,
                     xlab = "Higher Investment ",
                     ylab = "Higher Vaccine Disbursement",
                     size = 8)

finalPlot3 <- ggdraw() +
  draw_plot(biworldmap3, 0, 0, 1, 1) +
  draw_plot(legend3, 0, .0, 0.4, 0.4, scale = 1)

finalPlot3



##################################################

#univariate maps 
worlddata <- full_join(data_condensed, world_map, by=c("Country"="region"))

worlddata <- filter(worlddata, Country != "Côte d'Ivoire")
worlddata <- filter(worlddata, Country != "North Macedonia")
#worlddata[is.na(worlddata)] <- 0

worldplot_investment <- ggplot() + geom_polygon(data=worlddata, aes(x=long, y=lat, group=group, fill=quantity), color="grey50") +
  coord_map("albers", parameters=c(0,0)) +
  scale_fill_gradient(name="Investment",low="whitesmoke",high="gold") +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme_void() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank())
worldplot_investment

worldplot_vaccines <- ggplot() + geom_polygon(data=worlddata, aes(x=long, y=lat, group=group, fill=total_doses_delivered), color="grey50") +
  coord_map("albers", parameters=c(0,0)) +
  scale_fill_gradient(name="Vaccines",low="whitesmoke",high="darkred") +
  labs(x="", y="", title="Chinese Vaccine Distribution") +
  theme_void() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank())
worldplot_vaccines
