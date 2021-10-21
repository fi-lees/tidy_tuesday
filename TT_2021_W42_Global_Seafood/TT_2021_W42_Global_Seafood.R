# Tidy Tuesday - W42 - Global Fish and Seafood Supply ------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2021, Week 42). 
#
# Date: 2021-10-13
#
# Author: Fiona Lees
#
# Data Source: Our World in Data (https://ourworldindata.org/seafood-production)
# Hannah Ritchie and Max Roser (2019) - "Seafood Production". 
#
# Note: The data were originally sourced from:
# UN Food and Agriculture Organization database (https://www.fao.org/faostat/en/#data)
# UN Food and Agricultural Organization FishStat Database (https://www.fao.org/fishery/statistics/software/fishstatj/en). 
#
# Data Description: Our World in Data provide a collection of of charts and data files showing 
# seafood capture / production and consumption around the world (1961 - present).
# -----------------------------------------------------------------------------------------------------


### Load Packages
library(sf)
library(rnaturalearth)
library(RColorBrewer)
library(tidyverse)


### Import Data

# Import the fish and seafood supply data
supply <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv"
)

# Use the ne_countries() function from the rnaturalearth package to load world boundary data
# Make sure the returned object is of class sf
world <- ne_countries(returnclass = "sf")


### Wrangle Data

# Clean variable names
supply <- janitor::clean_names(supply) %>% 
  rename(kg_capita = fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020)

# Filter out entities that aren't countries (rows where `code` equals NA or is more than 3 digits long) 
supply_new <- supply %>% 
  filter(!is.na(code), nchar(code) == 3) %>% 
  arrange(entity, year)

# Pivot data into wider format so each row represents one entity 
supply_wide <- supply_new %>% 
  pivot_wider(names_from = year, values_from = kg_capita, names_prefix = "Y_",)

# Join boundary and supply data
world_supply_wide <- left_join(world, supply_wide, by = c("iso_a3" = "code"))

# Make data tidy again
world_supply_narrow <- world_supply_wide %>% 
  pivot_longer(cols = starts_with("Y_"), names_to = "year", values_to = "kg_capita") %>% 
  mutate(year = as.numeric(gsub("Y_", "", year)))

# Create discrete version of the kg_capita variable
world_supply_narrow  <- world_supply_narrow %>% 
  mutate(kg_capita_cut = cut(kg_capita, breaks = c(-0.1, 2.5, 5, 10, 20, 30, 40, 50, 75, 200)))


### Visualise Data

# Set theme for chart
theme_set(theme_minimal())
theme_update(
  text = element_text(colour = "grey40"),
  plot.title = element_text(colour = "black", size = 16, face = "bold", margin = margin(t = 5, b = 5)),
  plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
  legend.text = element_text(size = 11),
  legend.position = "right",
  plot.caption = element_text(hjust = 0),
  plot.margin = margin(rep(8, 4))
)

# Create final choropleth map
map_final_2017 <- world_supply_narrow %>%
  # Get 2017 data
  filter(year == 2017) %>%
  # Map fill colour to discrete variable kg_capita_cut
  ggplot(aes(fill = kg_capita_cut)) +
  # Plot map with thin white boundaries
  geom_sf(colour = "#ffffff", size = 0.1) +
  # Use ColorBrewer palette
  scale_fill_brewer(palette = "YlGnBu", 
                    na.value = "#dddddd",
                    name = "Per capita (kg)",
                    # Give the break points in the legend more meaningful labels 
                    labels = c("<= 2.5", "2.5 - 5", "5 - 10", "10 - 20", "20 - 30", 
                               "30 - 40", "40 - 50", "50 - 75", "> 75", "No data")) +
  # Use Robinson projection
  coord_sf(crs = "ESRI:54030") +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Average supply of fish and seafood across the population, measured in kilograms per person per year.",
       # Add some information at the bottom of the chart to give necessary context to chart
       caption = "Supply is defined as food available for human consumption. The values presented in this chart are based on per capita food supply at the consumer level; they do not account for food\nwaste at the consumer level and therefore overestimate the average amount of food actually consumed. Data are inclusive of all fish species and major seafood commodities, including\ncrustaceans, cephalopods and other mollusc species.\n  \nTidy Tuesday: Week 42, 2021 | Data source: United Nations Food and Agricultural Organization (via OurWorldInData.org/seafood-production) | Visualisation: @Fi_Lees"
       )

map_final_2017

# Save this visualisation.
ggsave("fish_supply_world_map_2017.png", map_final_2017 , width = 12, height = 7, units = "in", dpi = 300)
