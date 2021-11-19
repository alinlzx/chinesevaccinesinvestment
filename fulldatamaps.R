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
library(gridExtra)
library(mltools)

options(scipen=100)

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

#NEW BICLASS
custom_bi_class <- function (.data, x, y, style = "quantile", dim = 3, keep_factors = FALSE, include_lowest = TRUE) 
{
  bi_x = bi_y = NULL
  if (missing(.data)) {
    stop("An object containing data must be specified for the '.data' argument.")
  }
  if (missing(x)) {
    stop("A variable must be given for the 'x' argument.")
  }
  if (missing(y)) {
    stop("A variable must be given for the 'y' argument.")
  }
  if (style %in% c("quantile", "equal", "fisher", 
                   "jenks", "headtails") == FALSE) {
    stop("The allowed styles are 'equal', 'fisher', 'jenks', 'headtails', or 'quantile'.")
  }
  if (is.numeric(dim) == FALSE) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (dim != 2 & dim != 3 & dim != 4 & dim != 5) {
    stop("The 'dim' argument only accepts the numeric values '2' throguh '5'.")
  }
  if (is.logical(keep_factors) == FALSE) {
    stop("A logical scalar must be supplied for 'keep_factors'. Please provide either 'TRUE' or 'FALSE'.")
  }
  paramList <- as.list(match.call())
  if (!is.character(paramList$x)) {
    xQ <- rlang::enquo(x)
  }
  else if (is.character(paramList$x)) {
    xQ <- rlang::quo(!!rlang::sym(x))
  }
  xQN <- rlang::quo_name(rlang::enquo(x))
  if (!is.character(paramList$y)) {
    yQ <- rlang::enquo(y)
  }
  else if (is.character(paramList$y)) {
    yQ <- rlang::quo(!!rlang::sym(y))
  }
  yQN <- rlang::quo_name(rlang::enquo(y))
  if (xQN %in% names(.data) == FALSE) {
    stop(glue::glue("The given 'x' variable '{var}' is not found in the given data set.", 
                    var = xQN))
  }
  if (yQN %in% names(.data) == FALSE) {
    stop(glue::glue("The given 'y' variable '{var}' is not found in the given data set.", 
                    var = yQN))
  }
  bins_x <- dplyr::pull(.data, !!xQ)
  bins_y <- dplyr::pull(.data, !!yQ)
  if (style == "quantile") {
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "quantile")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "quantile")
  }
  else if (style == "equal") {
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "equal")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "equal")
  }
  else if (style == "fisher") {
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "fisher")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "fisher")
  }
  else if (style == "jenks") {
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "jenks")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "jenks")
  }
  else if (style == "headtails") {
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "headtails")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "headtails")
  }
  bins_x <- bins_x$brks
  bins_y <- bins_y$brks
  
  if (include_lowest == TRUE) {
    out <- dplyr::mutate(.data, bi_x = cut(!!xQ, breaks = unique(bins_x), 
                                           include.lowest = TRUE))
    out <- dplyr::mutate(out, bi_y = cut(!!yQ, breaks = unique(bins_y), 
                                         include.lowest = TRUE))
  }
  else if (include_lowest == FALSE) {
    out <- dplyr::mutate(.data, bi_x = cut(!!xQ, breaks = unique(bins_x), 
                                           include.lowest = FALSE))
    out <- dplyr::mutate(out, bi_y = cut(!!yQ, breaks = unique(bins_y), 
                                         include.lowest = FALSE))
  }
  
  out <- dplyr::mutate(out, bi_class = paste0(as.numeric(bi_x), 
                                              "-", as.numeric(bi_y)))
  if (keep_factors == FALSE) {
    out <- dplyr::select(out, -c(bi_x, bi_y))
  }
  return(out)
}


bidata <- custom_bi_class(data, x = quantity, y = total_doses_delivered, style = "quantile", dim = 4, include_lowest = TRUE)
bidata <- rename(bidata, Investment_bi_Vaccine = bi_class)


#bidata <- bi_class(data, x = quantity, y = total_doses_delivered, style = "jenks", dim = 3)
#bidata <- rename(bidata, Investment_bi_Vaccine = bi_class)

#bidata <- bi_class(bidata, x = usa_quantity, y = total_doses_delivered, style = "jenks", dim = 3)
#bidata <- rename(bidata, USInvestment_bi_Vaccine = bi_class)

#bidata <- bi_class(bidata, x = quantity, y = usa_quantity, style = "jenks", dim = 3)
#bidata <- rename(bidata, Investment_bi_USInvestment = bi_class)

#MAP PREP
world_map <- map_data("world")
biworld <- full_join(bidata, world_map, by=c("Country"="region"))

#biworlddata <- filter(biworlddata, Country != "Côte d'Ivoire")
#biworlddata <- filter(biworlddata, Country != "North Macedonia")

#bivariate map

#PALETTE
bi_pal_manual_custom <- function (val_1_1, val_1_2, val_1_3, val_1_4,
                                  val_2_1, val_2_2, val_2_3, val_2_4,
                                  val_3_1, val_3_2, val_3_3, val_3_4,
                                  val_4_1, val_4_2, val_4_3, val_4_4,
                                  preview = FALSE) 
{
  paramList <- as.list(match.call())
  if ("preview" %in% names(paramList) == TRUE) {
    input <- length(match.call()) - 2
  }
  else {
    input <- length(match.call()) - 1
  }
  #  if (input != 4 & input != 9) {
  #    stop("Incorrect number of values specified. For two-by-two palettes, there should be four values.\n         For three-by-three palettes, there should be nine.")
  #  }
  #  if (input == 4 & (missing(val_1_1) == TRUE | missing(val_1_2) == 
  #                    TRUE | missing(val_2_1) == TRUE | missing(val_2_2) == 
  #                    TRUE)) {
  #    stop("For two-by-two palettes, the 'val_1_1', 'val_1_2', 'val_2_1', and 'val_2_2' parameters are required.")
  #  }
  if (grepl(pattern = "^#", x = val_1_1) == FALSE | nchar(x = val_1_1) != 
      7) {
    stop("Hex value for 'val_1_1' not formatted appropriately.")
  }
  if (grepl(pattern = "^#", x = val_1_2) == FALSE | nchar(x = val_1_2) != 
      7) {
    stop("Hex value for 'val_1_2' not formatted appropriately.")
  }
  if (grepl(pattern = "^#", x = val_2_1) == FALSE | nchar(x = val_2_1) != 
      7) {
    stop("Hex value for 'val_2_1' not formatted appropriately.")
  }
  if (grepl(pattern = "^#", x = val_2_2) == FALSE | nchar(x = val_2_2) != 
      7) {
    stop("Hex value for 'val_2_2' not formatted appropriately.")
  }
  if (input == 9) {
    if (grepl(pattern = "^#", x = val_1_3) == FALSE | 
        nchar(x = val_1_3) != 7) {
      stop("Hex value for 'val_1_3' not formatted appropriately.")
    }
    if (grepl(pattern = "^#", x = val_2_3) == FALSE | 
        nchar(x = val_2_3) != 7) {
      stop("Hex value for 'val_2_3' not formatted appropriately.")
    }
    if (grepl(pattern = "^#", x = val_3_3) == FALSE | 
        nchar(x = val_3_3) != 7) {
      stop("Hex value for 'val_3_3' not formatted appropriately.")
    }
    if (grepl(pattern = "^#", x = val_3_1) == FALSE | 
        nchar(x = val_3_1) != 7) {
      stop("Hex value for 'val_3_1' not formatted appropriately.")
    }
    if (grepl(pattern = "^#", x = val_3_2) == FALSE | 
        nchar(x = val_3_2) != 7) {
      stop("Hex value for 'val_3_2' not formatted appropriately.")
    }
  }
  if (input == 4) {
    out <- c(`2-2` = val_2_2, `1-2` = val_1_2, 
             `2-1` = val_2_1, `1-1` = val_1_1)
    dim <- 2
  }
  else if (input == 9) {
    out <- c(`3-3` = val_3_3, `2-3` = val_2_3, 
             `1-3` = val_1_3, `3-2` = val_3_2, `2-2` = val_2_2, 
             `1-2` = val_1_2, `3-1` = val_3_1, `2-1` = val_2_1, 
             `1-1` = val_1_1)
    dim <- 3
  }
  else if (input == 16) {
    out <- c(`4-4` = val_4_4, `3-4` = val_3_4, `2-4` = val_2_4, `1-4` = val_1_4,
             `4-3` = val_4_3, `3-3` = val_3_3, `2-3` = val_2_3, `1-3` = val_1_3,
             `4-2` = val_4_2, `3-2` = val_3_2, `2-2` = val_2_2, `1-2` = val_1_2,
             `4-1` = val_4_1, `3-1` = val_3_1, `2-1` = val_2_1, `1-1` = val_1_1)
    dim <- 4
  }
  else if (input == 25) {
    out <- c(`5-5` = val_5_5, `4-5` = val_4_5, `3-5` = val_3_5, `2-5` = val_2_5, `1-5` = val_1_5,
             `5-4` = val_5_4, `4-4` = val_4_4, `3-4` = val_3_4, `2-4` = val_2_4, `1-4` = val_1_4,
             `5-3` = val_5_3, `4-3` = val_4_3, `3-3` = val_3_3, `2-3` = val_2_3, `1-3` = val_1_3,
             `5-2` = val_5_2, `4-2` = val_4_2, `3-2` = val_3_2, `2-2` = val_2_2, `1-2` = val_1_2,
             `5-1` = val_5_1, `4-1` = val_4_1, `3-1` = val_3_1, `2-1` = val_2_1, `1-1` = val_1_1)
    dim <- 5
  }
  class(out) <- append(class(out), "bi_pal_custom")
  if (preview == TRUE) {
    out <- bi_legend(pal = out, dim = dim, size = 16)
  }
  return(out)
  
}

custom_palette <- bi_pal_manual_custom("#d3d3d3", "#d9b1b1", "#e08b8b", "#e85d5d", "#cec192", "#d4a17a", "#da7f61", "#e25540", "#c9ae4c", "#cf9140", "#d57332", "#dd4c22", "#c39900", "#c98000", "#ce6500", "#d64300", preview = FALSE)


#MANUAL PALETTE
bi_scale_color_custom <- function (pal, dim = 3, ...) {
  if (missing(pal) == TRUE) {
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', or 'GrPink'\n         or supply a custom palette created with 'bi_pal_custom()'.")
  }
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    if (dim == 2 & length(pal) != 4) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    else if (dim == 3 & length(pal) != 9) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    else if (dim == 4 & length(pal) != 16) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    else if (dim == 5 & length(pal) != 25) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
  }
  else if ("bi_pal_custom" %in% class(pal) == FALSE) {
    if (pal %in% c("Brown", "DkBlue", "DkCyan", 
                   "DkViolet", "GrPink") == FALSE) {
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', or 'GrPink'.")
    }
  }
  if (is.numeric(dim) == FALSE) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (dim != 2 & dim != 3 & dim != 4 & dim != 5) {
    stop("The 'dim' argument only accepts the numeric values '2' through '5'.")
  }
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    x <- pal
  }
  else if ("bi_pal_custom" %in% class(pal) == FALSE) {
    if (pal == "DkViolet") {
      x <- pal_dkviolet(n = dim)
    }
    else if (pal == "GrPink") {
      x <- pal_grpink(n = dim)
    }
    else if (pal == "DkBlue") {
      x <- pal_dkblue(n = dim)
    }
    else if (pal == "DkCyan") {
      x <- pal_dkcyan(n = dim)
    }
    else if (pal == "Brown") {
      x <- pal_brown(n = dim)
    }
  }
  ggplot2::scale_color_manual(values = x, ...)
}

bi_scale_fill_custom <- function (pal, dim = 3, ...) {
  
  if (missing(pal) == TRUE) {
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', or 'GrPink'\n         or supply a custom palette created with 'bi_pal_custom()'.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    if (dim == 4 & length(pal) != 16) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
  } 
  else if ("bi_pal_custom" %in% class(pal) == FALSE) {
    if (pal %in% c("Brown", "DkBlue", "DkCyan", 
                   "DkViolet", "GrPink") == FALSE) {
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', or 'GrPink'.")
    }
  }
  if (is.numeric(dim) == FALSE) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (dim != 2 & dim != 3 & dim != 4 & dim != 5) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    x <- pal
  }
  else if ("bi_pal_custom" %in% class(pal) == FALSE) {
    if (pal == "DkViolet") {
      x <- pal_dkviolet(n = dim)
    }
    else if (pal == "GrPink") {
      x <- pal_grpink(n = dim)
    }
    else if (pal == "DkBlue") {
      x <- pal_dkblue(n = dim)
    }
    else if (pal == "DkCyan") {
      x <- pal_dkcyan(n = dim)
    }
    else if (pal == "Brown") {
      x <- pal_brown(n = dim)
    }
  }
  ggplot2::scale_fill_manual(values = x, ...)
}

#MANUAL LEGEND
bi_legend_custom <- function (pal, dim = 3, xlab, ylab, size = 10) {
  bi_class = bi_fill = x = y = NULL
  if (missing(pal) == TRUE) {
    stop("A palette must be specified for the 'pal' argument.")
  }
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    if (dim == 2 & length(pal) != 4) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    else if (dim == 3 & length(pal) != 9) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    else if (dim == 4 & length(pal) != 16) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    if (dim == 5 & length(pal) != 25) {
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
  }
  else if ("bi_pal_custom" %in% class(pal) == FALSE) {
    if (pal %in% c("Brown", "DkBlue", "DkCyan", 
                   "DkViolet", "GrPink") == FALSE) {
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', or 'GrPink'.")
    }
  }
  if (is.numeric(dim) == FALSE) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (dim != 2 & dim != 3 & dim !=4 & dim !=5) {
    stop("The 'dim' argument only accepts the numeric values '2' through '5'.")
  }
  if (missing(xlab) == TRUE) {
    xlab <- "x var "
  }
  if (is.character(xlab) == FALSE) {
    stop("The 'xlab' argument must be a character string.")
  }
  if (missing(ylab) == TRUE) {
    ylab <- "y var "
  }
  if (is.character(ylab) == FALSE) {
    stop("The 'ylab' argument must be a character string.")
  }
  if (is.numeric(size) == FALSE) {
    stop("The 'size' argument must be a numeric value.")
  }
  xQN <- rlang::quo_name(rlang::enquo(xlab))
  yQN <- rlang::quo_name(rlang::enquo(ylab))
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    x <- pal
  }
  else if ("bi_pal_custom" %in% class(pal) == FALSE) {
    if (pal == "DkViolet") {
      x <- pal_dkviolet(n = dim)
    }
    else if (pal == "GrPink") {
      x <- pal_grpink(n = dim)
    }
    else if (pal == "DkBlue") {
      x <- pal_dkblue(n = dim)
    }
    else if (pal == "DkCyan") {
      x <- pal_dkcyan(n = dim)
    }
    else if (pal == "Brown") {
      x <- pal_brown(n = dim)
    }
  }
  x <- dplyr::tibble(bi_class = names(x), bi_fill = x)
  leg <- tidyr::separate(x, bi_class, into = c("x", "y"), 
                         sep = "-")
  leg <- dplyr::mutate(leg, x = as.integer(x), y = as.integer(y))
  legend <- ggplot2::ggplot() + ggplot2::geom_tile(data = leg, 
                                                   mapping = ggplot2::aes(x = x, y = y, fill = bi_fill)) + 
    ggplot2::scale_fill_identity() + ggplot2::labs(x = substitute(paste(xQN, 
                                                                        "" %->% "")), y = substitute(paste(yQN, "" %->% 
                                                                                                             ""))) + bi_theme() + ggplot2::theme(axis.title = ggplot2::element_text(size = size)) + 
    ggplot2::coord_fixed()
  return(legend)
}


biworldmap1 <- ggplot() + geom_polygon(data=biworld, aes(x=long, y=lat, group=group, fill=Investment_bi_Vaccine), color="grey50", show.legend = FALSE, size = 0.1) +
  coord_map("albers", parameters=c(0,0)) +
  bi_scale_fill_custom(pal = custom_palette, dim = 4) + bi_theme() +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  labs(title = "Do Chinese Vaccines follow Chinese Investment?", subtitle = "Custom Palette")

legend1 <- bi_legend_custom(pal = custom_palette,
                    dim = 4,
                    xlab = "Higher Investment ",
                    ylab = "Higher Vaccine Disbursement",
                    size = 9)

finalPlot1 <- ggdraw() +
  draw_plot(biworldmap1, 0, 0, 1, 1) +
  draw_plot(legend1, 0, .0, 0.4, 0.4, scale = 1)

finalPlot1

#bivariabe map 2
sseasia <- filter(biworld, Region == "SE Asia" | Region == "South Asia")

biworldmap2 <- ggplot() + geom_polygon(data=sseasia, aes(x=long, y=lat, group=group, fill=Investment_bi_Vaccine), color="grey50", show.legend = FALSE, size = 0.1) +
  coord_map("albers", parameters=c(0,0)) +
  bi_scale_fill_custom(pal = custom_palette, dim = 4) + bi_theme() +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  labs(title = "Do Chinese Vaccines follow Chinese Investment?", subtitle = "Custom Palette")

legend2 <- bi_legend_custom(pal = custom_palette,
                     dim = 4,
                     xlab = "Higher Investment ",
                     ylab = "Higher Vaccine Disbursement",
                     size = 9)

finalPlot2 <- ggdraw() +
  draw_plot(biworldmap2, 0, 0, 1, 1) +
  draw_plot(legend2, 0, .0, 0.4, 0.4, scale = 1)

finalPlot2

#bivariabe map 3
lac <- filter(biworld, Region == "Latin America")

biworldmap3 <- ggplot() + geom_polygon(data=lac, aes(x=long, y=lat, group=group, fill=Investment_bi_Vaccine), color="grey50", show.legend = FALSE, size = 0.1) +
  coord_map("albers", parameters=c(0,0)) +
  bi_scale_fill_custom(pal = custom_palette, dim = 4) + bi_theme() +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  labs(title = "Do Chinese Vaccines follow Chinese Investment?", subtitle = "Dark Blue (DkViolet) Palette")

legend3 <- bi_legend_custom(pal = custom_palette,
                     dim = 4,
                     xlab = "Higher Investment ",
                     ylab = "Higher Vaccine Disbursement",
                     size = 8)

finalPlot3 <- ggdraw() +
  draw_plot(biworldmap3, 0, 0, 1, 1) +
  draw_plot(legend3, 0, .0, 0.4, 0.4, scale = 1)

finalPlot3



##################################################

#univariate maps 
world_investment <- ggplot() + 
  geom_polygon(data=sseasia, aes(x=long, y=lat, group=group, fill=quantity), color="grey50") +
  coord_map("albers", parameters=c(0,0)) +
  scale_fill_gradient(name="Investment",low="whitesmoke",high="gold") +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme_void() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank())
world_investment

world_usa_investment <- ggplot() + 
  geom_polygon(data=sseasia, aes(x=long, y=lat, group=group, fill=usa_quantity_per_million), color="grey50") +
  coord_map("albers", parameters=c(0,0)) +
  scale_fill_gradient(name="Investment",low="whitesmoke",high="blue") +
  labs(x="", y="", title="US Investment since 2015") +
  theme_void() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank())
world_usa_investment


world_vaccines <- ggplot() + 
  geom_polygon(data=sseasia, aes(x=long, y=lat, group=group, fill=total_doses_delivered), color="grey50") +
  coord_map("albers", parameters=c(0,0)) +
  scale_fill_gradient(name="Vaccines",low="whitesmoke",high="darkred") +
  labs(x="", y="", title="Chinese Vaccine Distribution") +
  theme_void() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank())
world_vaccines

lac_investment <- ggplot() + 
  geom_polygon(data=lac, aes(x=long, y=lat, group=group, fill=quantity), color="grey50") +
  coord_map("albers", parameters=c(0,0)) +
  scale_fill_gradient(name="Investment",low="whitesmoke",high="gold") +
  labs(x="", y="", title="Chinese Investment since 2016") +
  theme_void() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank())

lac_vaccines <- ggplot() + 
  geom_polygon(data=lac, aes(x=long, y=lat, group=group, fill=total_doses_delivered), color="grey50") +
  coord_map("albers", parameters=c(0,0)) +
  scale_fill_gradient(name="Vaccines",low="whitesmoke",high="darkred") +
  labs(x="", y="", title="Chinese Vaccine Distribution") +
  theme_void() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank())

grid.arrange(lac_investment, lac_vaccines)

grid.arrange(world_investment, world_vaccines)

ggplot() + geom_point(data = bidata, aes(x=quantity_per_million, y=doses_per_million, color = Region))
