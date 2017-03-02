library(maptools)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(colorplaner)

# Shspefiles from https://chronicdata.cdc.gov/d/x7zy-2xmx?category=500-Cities
tracts <- readOGR("./shapes", "500Cities_Tracts_Clip")
chi_tracts <- subset(tracts, PlaceName == "Chicago")
tractgg <- fortify(chi_tracts, region = "plctract10")

# 500Cities data from https://chronicdata.cdc.gov/d/6vp6-wxuq?category=500-Cities
city500 <- read_csv("500_Cities__Local_Data_for_Better_Health.csv")
chi_500 <- filter(city500, CityName == "Chicago", 
                  GeographicLevel == "Census Tract")
chi_500 %>%
  filter(grepl("Current lack of health insurance|Obesity", Measure)) %>%
  select(UniqueID, Measure, Data_Value) %>%
  mutate(Measure = ifelse(grepl("insurance", Measure), "lack_insurance", Measure),
         Measure = ifelse(grepl("Obesity", Measure), "obesity", Measure),
         Data_Value = Data_Value / 100) %>%
  spread(Measure, Data_Value) %>% 
  ggplot(aes(fill = lack_insurance, fill2 = obesity, map_id = UniqueID)) +
  geom_map(map = tractgg) +
  scale_fill_colorplane(axis_title = "People Without\nHealth Insurance",
                        axis_title_y = "Obesity Prevalence",
                        labels = scales::percent,
                        labels_y = scales::percent,
                        na.color = "white") +
  expand_limits(x = range(tractgg$long), y = range(tractgg$lat)) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(color = "grey40")) +
  labs(x = "", y = "", title = "Chicago: Health Insurance versus Obesity",
       subtitle = "Data from the 500 Cities Project", 
       caption = "@DATATITIAN - made with the colorplaner R package") +
  guides(fill = guide_colorplane(
    title = "Color Key",
    title.theme = element_text(size = 13),
    label.theme = theme_gray() + theme(axis.text = element_text(size = 8)),
    label.y.theme = theme_gray())) 

