
# Set working directory
#setwd("~/Documents/map_RO_auto/GitHub version")

# Housekeeping
rm(list = ls())

# Define libraries
library(dplyr)
library(ggplot2)
library(mapdata)
library(readxl)


# Load data
auto <- read_xlsx("Data.xlsx")


# Eliminate firms with no turnover & transform to millions
auto_clean <- auto[auto$TO_2018!=0,]
auto_clean$TO_2018 <- auto_clean$TO_2018/(10^6)


# define a "not in" function - the negation of the function "in"
`%notin%` <- Negate(`%in%`)


# Some statistics
summary(auto_clean[auto_clean$CUI %notin% c(160796,6488696),])
auto_clean[auto_clean$CUI %in% c(160796,6488696),]

# Extract (world) map from package
world <- map_data("world")


# Number of regions
length(unique(world$region))


# Extract Romania
RO <- world %>%
  filter(region == "Romania")


# Build first layer (Romania) & plot
g1 <- ggplot() + geom_polygon(data = RO, aes(x=long, y = lat, group = group),colour="black",
                                   fill = rgb(231,230,198,max=255),size=1,linetype="twodash")
plot(g1)


# Add second layer (density), titles, other information & plot
g2 <- g1 + 
           coord_fixed(1.3) +
           theme_void() +
           stat_density2d(
           aes(x = Coord_A, y= Coord_B, fill = ..level.., alpha = ..level..),
           size = 1, bins = 10, data = auto_clean, geom = "polygon") +
           scale_alpha(guide = FALSE) +
           guides(size=guide_legend(title="Turnover")) +
           theme(legend.position="right") +
           scale_fill_gradient(low=rgb(62,138,128,max=255), high=rgb(0,112,99,max=255), 
                               name="Density") +
           labs(title = "Geographical spread of automotive manufacturers in Romania, in 2018",
                subtitle = "Most important auto hubs are near București, Pitești, Craiova, Brașov and Timișoara",
                caption = "Note: Dacia & Ford are coloured distinctly\n Source: Ministry of Finance")
plot(g2)


# Add third layer (individual points)
g3 <- g2 +
           annotate("point", x = auto_clean$Coord_A[auto_clean$CUI == 160796][2], 
                             y = auto_clean$Coord_B[auto_clean$CUI == 160796][2], 
                              colour = rgb(194,193,222,max=255),size=36) +
           annotate("point", x = auto_clean$Coord_A[auto_clean$CUI == 6488696][1], 
                             y = auto_clean$Coord_B[auto_clean$CUI == 6488696][1], 
                              colour = rgb(194,193,222,max=255),size=15) +
           annotate("text", x = auto_clean$Coord_A[auto_clean$CUI == 160796][2]-0.7, 
                            y = auto_clean$Coord_B[auto_clean$CUI == 160796][2]-0.1, 
                            label = "Dacia", colour = "black",size=5) +
           annotate("text", x = auto_clean$Coord_A[auto_clean$CUI == 6488696][1]-0.4, 
                            y = auto_clean$Coord_B[auto_clean$CUI == 6488696][1]-0.05, 
                            label = "Ford", colour = "black",size=5) +
           geom_point(data = auto_clean[auto_clean$CUI %notin% c(160796,6488696),], 
                      mapping = aes(x = Coord_A, y = Coord_B,
                      size = TO_2018),color = rgb(171,42,37,max=255))

plot(g3)





