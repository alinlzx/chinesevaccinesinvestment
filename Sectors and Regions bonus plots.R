library(RColorBrewer)

investments <- read_csv("Chinese Investments.csv")

#breaking down the investment sectors

cut_off_year = 2016
investments_new <- filter(investments, Year >= cut_off_year)
investments_new$`Quantity in Millions` <- gsub("\\$", "", investments_new$`Quantity in Millions`)
investments_new$`Quantity in Millions` <- gsub("\\,", "", investments_new$`Quantity in Millions`)
investments_new$`Quantity in Millions` <- as.numeric(investments_new$`Quantity in Millions`)

investments_new <- mutate(investments_new, Country=fct_recode(Country,
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
                                                                                            
investments_new <- select(investments_new, -Region)

investments_by_region <- left_join(investments_new, data_adj, by = "Country")

investments_by_region$Region[investments_by_region$Region=="Arab Middle East and North Africa"] <- "MENA"

nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols)

ggplot(data = investments_by_region) + geom_bar(aes(x = Region, y = `Quantity in Millions`, fill = Sector), stat = "identity", position = "fill") + 
  scale_fill_manual(values = mycolors)

#looking at just BRI countries or by region

investments_with_bri <- summarize(group_by(investments_by_region, Country, BRI, doses_per_million, quantity_per_million, Population), 
                                            quantity = sum(`Quantity in Millions`))

ggplot(investments_with_bri) + geom_point(aes(x = doses_per_million, y = quantity_per_million, 
                                              color = as.factor(BRI), size = Population, alpha = 0.3))

ggplot(investments_with_bri) + geom_boxplot(aes(x = as.factor(BRI), y = doses_per_million))

ggplot(investments_by_region) + geom_boxplot(aes(x = Region, y = doses_per_million))