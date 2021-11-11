library(tidyverse)
library(readxl)
library(maps)
library(mapproj)
library(lubridate) #access lubridate to modify dates and times
library(biscale)

library(sf) # spatial data handling
library(sp)
library(cowplot)
library(ggthemes)
#library(raster) # raster handling (needed for relief)
#library(viridis) # viridis color scale
#library(cowplot) # stack ggplots

#reading data
vaccines <- read_csv("Vaccines.csv")
investments <- read_csv("Chinese Investments.csv")
world_map <- map_data("world")

#filling in the missing values for vaccines
vaccines <- fill(vaccines, Region, Destination, `Income Level`)
vaccines[77,10] <- "Lower Middle"
vaccines[78,10] <- "Lower Middle"
vaccines[128, 10] <- "Lower Middle"
vaccines <- mutate(vaccines, `Total doses delivered (millions)` = `Total doses delivered (millions)`*1000000)
vaccines <- rename(vaccines, `Total doses delivered` = `Total doses delivered (millions)`)

#cutting out old investments data
cut_off_year = 2016
investments_new <- filter(investments, Year >= cut_off_year)
investments_new$`Quantity in Millions` <- gsub("\\$", "", investments_new$`Quantity in Millions`)
investments_new$`Quantity in Millions` <- gsub("\\,", "", investments_new$`Quantity in Millions`)
investments_new$`Quantity in Millions` <- as.numeric(investments_new$`Quantity in Millions`)

#groupby data
vaccines_condensed <- summarize(group_by(vaccines, Destination, `Income Level`),
                                Manufacturer = paste(Manufacturer, collapse = ", "),
                                doses_purchased = sum(`Doses purchased (millions)`),
                                doses_donated = sum(`Doses donated (millions)`),
                                purchased_delivered = sum(`Purchased doses delivered (millions)`),
                                donated_delivered = sum(`Donated doses delivered (millions)`),
                                total_doses_delivered = sum(`Total doses delivered`))

vaccines_condensed <- vaccines_condensed[-101,]

investments_condensed <- summarize(group_by(investments_new, Country, Region, BRI), 
                                   quantity = sum(`Quantity in Millions`),
                                   sector = paste(Sector, collapse = ", "),
                                   subsector = paste(Subsector, collapse = ", "),
                                   investor = paste(Investor, collapse = ", "))

investments_condensed <- mutate(investments_condensed, quantity = quantity*1000000)

#making sure the country names match up
vaccines_names <- levels(as.factor(vaccines_condensed$Destination))
investments_names <- levels(as.factor(investments_condensed$Country))
map_names <- levels(as.factor(world_map$region))

vaccines_condensed <- rename(vaccines_condensed, Country = Destination)
vaccines_condensed <- mutate(vaccines_condensed, Country=fct_recode(Country,
                                                                    "Venezuela" = "R. B. de Venezuela",
                                                                    "Brunei" = "Brunei Darussalam",
                                                                    "Egypt" = "Arab Republic of Egypt",
                                                                    "Bosnia" = "Bosnia and Herzegovina",
                                                                    "Democratic Republic of the Congo" = "Democratic Republic of Congo",
                                                                    "Iran" = "Islamic Republic of Iran",
                                                                    "Laos" = "Lao People's Democratic Republic",
                                                                    "UAE" = "United Arab Emirates"))


vaccines_names[which( !(vaccines_names %in% investments_names))]
investments_names[which( !(investments_names %in% vaccines_names))]

data <- full_join(vaccines_condensed, investments_condensed, by = "Country")
data$total_doses_delivered[is.na(data$total_doses_delivered)] <- 0
data_condensed <- dplyr::select(data, Country, total_doses_delivered, quantity)
data_condensed[is.na(data_condensed)] <- 0

data_condensed_names <- levels(as.factor(data_condensed$Country))


data_condensed_names[which( !(data_condensed_names %in% map_names))]


data_condensed <- mutate(data_condensed, Country=fct_recode(Country,
                                                            "UK" = "Britain",
                                                            "Palestine" = "West Bank and Gaza",
                                                            "Russia" = "Russian Federation",
                                                            "Antigua" = "Antigua and Barbuda",
                                                            "Bosnia and Herzegovina" = "Bosnia",
                                                            "Republic of Congo" = "Congo",
                                                            "Trinidad" = "Trinidad and Tobago",
                                                            "Syria" = "Syrian Arab Republic",
                                                            "Kyrgyzstan" = "Kyrgyz Republic",
                                                            "United Arab Emirates" = "UAE",
                                                            "Ivory Coast" = "Côte d'Ivoire"))


#bivariate map prep
bi_data_condensed <- bi_class(data_condensed, x = quantity, y = total_doses_delivered, style = "jenks", dim = 3)
biworlddata <- full_join(bi_data_condensed, world_map, by=c("Country"="region"))
biworlddata <- filter(biworlddata, Country != "Côte d'Ivoire")
biworlddata <- filter(biworlddata, Country != "North Macedonia")

#bivariate map
biworldmap <- ggplot() + geom_polygon(data=biworlddata, aes(x=long, y=lat, group=group, fill=bi_class), color="grey50", show.legend = FALSE, size = 0.1) +
  coord_map("albers", parameters=c(0,0)) +
  bi_scale_fill(pal = "DkViolet", dim = 3) + bi_theme() +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  labs(title = "Do Chinese Vaccines follow Chinese Investment?", subtitle = "Dark Blue (DkViolet) Palette")

legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "Higher Investment ",
                    ylab = "Higher Vaccine Disbursement",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(biworldmap, 0, 0, 1, 1) +
  draw_plot(legend, 0, .0, 0.4, 0.4, scale = 1)

finalPlot

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

#scatterplot
scatter <- ggplot(data=data) + 
  geom_point(aes(x=quantity, y=total_doses_delivered, color = `Income Level`))
scatter

scatter2 <- ggplot(subset(data, !is.na(`Income Level`))) +
  geom_text(aes(x=quantity, y=total_doses_delivered, label = Country, color = `Income Level`)) +
  xlim(0, 30000000000)+ylim(0,450000000)
scatter2

scatter2 + xlim(0, 10000000000) + ylim(0,150000000)

scatter2 + xlim(0, 3000000000) + ylim(0,25000000)

scatter2 + xlim(0,500000000) + ylim(0, 3000000)


#--------------------------------------------------------------
#--------------------------------------------------------------

#now let's adjust the data w/ world population

population <- read_excel("World Populations (1).xlsx")

#Just making sure country names match up, then joining
data_condensed_names <- levels(as.factor(data_condensed$Country))
population_names <- levels(as.factor(population$`Country Name`))

data_condensed_names[which( !(data_condensed_names %in% population_names))]

population <- mutate(population, Country=fct_recode(`Country Name`,
                                                    "Antigua" = "Antigua and Barbuda",
                                                    "UK" = "United Kingdom",
                                                    "Brunei" = "Brunei Darussalam",
                                                    "Republic of Congo" = "Congo, Rep.",
                                                    "Ivory Coast" = "Cote d'Ivoire",
                                                    "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
                                                    "Egypt" = "Egypt, Arab Rep.",
                                                    "Iran" = "Iran, Islamic Rep.",
                                                    "Kyrgyzstan" = "Kyrgyz Republic",
                                                    "Laos" = "Lao PDR",
                                                    "Russia" = "Russian Federation",
                                                    "South Korea" = "Korea, Rep.",
                                                    "Syria" = "Syrian Arab Republic",
                                                    "Trinidad" = "Trinidad and Tobago",
                                                    "USA" = "United States",
                                                    "Venezuela" = "Venezuela, RB"))

population <- population[,-1]
                                                    

data_adj <- full_join(data_condensed, population, by="Country")

data_adj[112,4] <- 4803000 #palestine pop
data_adj[151,4] <- 23570000 #taiwan pop

#adjusting vaccine and investment values

data_adj$doses_per_million <- data_adj$total_doses_delivered / data_adj$Population * 1000000
data_adj$quantity_per_million <- data_adj$quantity / data_adj$Population * 1000000

data_adj <- filter(data_adj, !(is.na(doses_per_million)))

#bivariate visualization

bi_data <- bi_class(data_adj, x = quantity_per_million, y = doses_per_million, style = "jenks", dim = 3)
biworlddata <- full_join(bi_data, world_map, by="Country")
biworlddata <- filter(biworlddata, Country != "Côte d'Ivoire")
biworlddata <- filter(biworlddata, Country != "North Macedonia")

#bivariate map
biworldmap <- ggplot() + geom_polygon(data=biworlddata, aes(x=long, y=lat, group=group, fill=bi_class), color="grey50", show.legend = FALSE, size = 0.1) +
  coord_map("albers", parameters=c(0,0)) +
  bi_scale_fill(pal = "DkViolet", dim = 3) + bi_theme() +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  labs(title = "Do Chinese Vaccines follow Chinese Investment?", subtitle = "Dark Blue (DkViolet) Palette")

legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "Higher Investment ",
                    ylab = "Higher Vaccine Disbursement",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(biworldmap, 0, 0, 1, 1) +
  draw_plot(legend, 0, .0, 0.4, 0.4, scale = 1)

finalPlot