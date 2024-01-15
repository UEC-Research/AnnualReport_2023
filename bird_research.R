#Setup ####
#set packages needed
pkgs <- c("here", "tidyverse", "hrbrthemes", "lubridate", "cowplot", "sf",
          "sfhotspot", "plotly")
#install/update any packages needed
install.packages(pkgs, repos = 'http://cran.rstudio.com/')
#load packages
lapply(pkgs, require, character.only = TRUE)

#Data Import ####
birds <- read.csv("ebird_Jan2024.csv")

#Data Cleaning ####
e_2023 <- birds %>% 
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d"),
                Year = format(Date, "%Y")) %>% 
  dplyr::filter(County == "Milwaukee" &
                  (Location == "Menomonee Valley Passage (Urban Ecology Center)" |
                     Location == "Milwaukee River Greenway--North Ave. to Locust St." |
                     Location == "Riverside Park (Urban Ecology Center)" |
                     Location == "Three Bridges Park (Urban Ecology Center)" |
                     Location == "Washington Park (Urban Ecology Center)")) %>% 
  dplyr::mutate(Location = ifelse(Location == "Menomonee Valley Passage (Urban Ecology Center)" |
                                    Location == "Three Bridges Park (Urban Ecology Center)", "MV",
                                  ifelse(Location == "Milwaukee River Greenway--North Ave. to Locust St." |
                                           Location == "Riverside Park (Urban Ecology Center)", "RP", "WP"))) %>% 
  dplyr::mutate(Count = ifelse(Count == "X", 1, as.numeric(Count))) %>% 
  dplyr::mutate(Month = format(Date, "%m")) %>% 
  dplyr::filter(Count > 0)

#Species ####
sp <- e_2023 %>% 
        dplyr::select(Common.Name, Count, Location, Month, Year) %>% 
        dplyr::group_by(Common.Name, Location, Year) %>% 
        dplyr::summarise(Count = sum(Count))

sp_all <- sp %>% 
            dplyr::mutate(X = 1) %>% 
            dplyr::group_by(Location, Year) %>% 
            dplyr::summarise(X = sum(X)) %>% 
            dplyr::filter(Year != 2024)

p <- ggplot(sp_all %>% dplyr::filter(Location == "RP")) +
  geom_bar(mapping = aes(x = Year, y = X), stat = "identity", fill = "plum4") +
  labs(y = "Count\n", x = "\nYear",
       title = "Number of Species Reported at Riverside Park",
       caption = "Last updated 1-15-20243") +
  theme(plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(size = 7, face = "italic", color = "grey55"))

p <- ggplotly(p, tooltip = "X")
library(htmlwidgets)
saveWidget(p, "birds23_1.html", selfcontained = T, libdir = "lib")
