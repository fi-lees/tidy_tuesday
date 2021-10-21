---
title: "Tidy Tuesday: 2021, Week 42, Global Fish and Seafood Supply"
author: "Fiona Lees"
date: 2021-10-13
output: 
  html_document: 
    keep_md: yes
---



## Global Fish and Seafood Supply
This week"s data were provided by [Our World in Data](https://ourworldindata.org/seafood-production) (Hannah Ritchie and Max Roser (2019) - "Seafood Production"). The data were originally sourced from the [UN Food and Agriculture Organization Database](https://www.fao.org/faostat/en/#data) and the [UN Food and Agricultural Organization FishStat Database](https://www.fao.org/fishery/statistics/software/fishstatj/en). Our World in Data provide a collection of of charts and data files showing seafood capture / production and supply around the world (1961 - present).
  
### Objectives
This is a dataset with lots of possibilities, but I'm going to focus on fish and seafood supply per capita. I'd like to know more about how supply varies around the world.  

### Definitions
The United Nations Food and Agricultural Organization define fish and seafood supply per capita as the average supply across the population, measured in kilograms per person per year.  

Food supply is defined as food available for human consumption. At country level; it is calculated as the food remaining for human use after deduction of all non-food utilisations (i.e. food = production + imports + stock withdrawals − exports − industrial use − animal feed – seed – wastage − additions to stock). Wastage includes losses of usable products occurring along distribution chains from farm gate (or port of import) up to the retail level. However, such values do not include consumption-level waste (i.e. retail, restaurant and household waste) and therefore overestimate the average amount of food actually consumed.
  
### Learning Points
This week I learned how to create choropleth maps. I haven't created any maps in R before (or any other tool for that matter), so in preparation:   
- I spent a few hours doing a course called [Mapping Geographic Data in R, with ggplot2](https://flowingdata.com/mapping-in-r-ggplot2/) on the Flowing Data website. It's a really good tutorial and I'd recommend it for anyone who wants to learn more about mapping in R. It covers a lot more than just choropleth maps.  
- I read through an informative blog piece on the Datawrapper website called [What to consider when creating choropleth maps](https://blog.datawrapper.de/choroplethmaps/).  

### Load Packages
Load the required packages.  


```r
library(sf)
library(rnaturalearth)
library(countrycode)
library(rcartocolor)
library(RColorBrewer)
library(patchwork)
library(tidyverse)
```
  
### Import Data
Import the fish and seafood supply data. Store it in a data frame named `supply`.


```r
supply <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv"
  )
```

As I'm going to be creating some maps this week, I'll load the required boundary data at this point too. I'm going to use the countries 1:110 scale boundary data from [Natural Earth](https://www.naturalearthdata.com) and use a function from the `rnaturalearth` package to do this. I'm going to name this data `world` and make sure it's stored as a sf (simple features) object.


```r
# Use the ne_countries() function from the rnaturalearth package to load the data
# Make sure the returned object is of class sf
world <- ne_countries(returnclass = "sf")

# What's in the world boundary data?
print(world, n=0)
```

```
## Simple feature collection with 177 features and 63 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: -180 ymin: -90 xmax: 180 ymax: 83.64513
## CRS:           +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
```

```r
# What does the boundary data look like?
plot(select(world, name))
```

<img src="README_files/figure-html/import_shapefile-1.png" width="100%" style="display: block; margin: auto;" />

### Explore Data
Set-up a basic theme for any charts before starting to explore the `supply` data.


```r
theme_set(theme_minimal())

theme_update(
  text = element_text(colour = "grey40"),
  plot.title = element_text(colour = "black", size = 16, face = "bold", margin = margin(t = 5, b = 5)),
  plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
  strip.text = element_text(size = 12),
  axis.text = element_text(size = 11),
  axis.title.x = element_text(margin = margin(t = 10, b = 10)),
  axis.title.y = element_text(margin = margin(r = 10)),
  legend.text = element_text(size = 11),
  legend.position = "top",
  legend.justification = "left",
  plot.caption = element_text(hjust = 0),
  plot.margin = margin(rep(8, 4))
  )
```

Have an initial look at the `supply` data.  


```r
glimpse(supply)
```

```
## Rows: 11,028
## Columns: 4
## $ Entity                                                           <chr> "Afgh~
## $ Code                                                             <chr> "AFG"~
## $ Year                                                             <dbl> 1961,~
## $ `Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)` <dbl> 0.03,~
```

```r
# Clean up the variable names to make them easier to work with
supply <- janitor::clean_names(supply) %>% 
  rename(kg_capita = fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020)

# Data summary
summary(supply)
```

```
##     entity              code                year        kg_capita     
##  Length:11028       Length:11028       Min.   :1961   Min.   :  0.00  
##  Class :character   Class :character   1st Qu.:1975   1st Qu.:  5.34  
##  Mode  :character   Mode  :character   Median :1990   Median : 11.99  
##                                        Mean   :1989   Mean   : 16.67  
##                                        3rd Qu.:2004   3rd Qu.: 22.51  
##                                        Max.   :2017   Max.   :191.75
```

Have a closer look at the `entity` variable. Not all entities are countries; some rows show aggregate data for regional / economic groupings (e.g. Eastern Africa, Low Income Food Deficit Countries), or for old geographic groupings (e.g. USSR, Ethiopia PDR). These are the rows where `code` equals NA or is more than 3 digits long. I'm going to drop the rows for these entities and just focus on the countries. I'm going to store this filtered dataset in `supply_new`.


```r
# List entities that do not have a code
supply %>% 
  filter(is.na(code)) %>% 
  summarise(unique(entity))
```

```
## # A tibble: 34 x 1
##    `unique(entity)`       
##    <chr>                  
##  1 Africa                 
##  2 Americas               
##  3 Asia                   
##  4 Asia, Central          
##  5 Australia & New Zealand
##  6 Belgium-Luxembourg     
##  7 Caribbean              
##  8 Central America        
##  9 Eastern Africa         
## 10 Eastern Asia           
## # ... with 24 more rows
```

```r
# List entities where the code is four or more characters long
supply %>% 
  filter(nchar(code) >= 4) %>% 
  summarise(unique(entity))
```

```
## # A tibble: 7 x 1
##   `unique(entity)`     
##   <chr>                
## 1 Czechoslovakia       
## 2 Melanesia            
## 3 Polynesia            
## 4 Serbia and Montenegro
## 5 USSR                 
## 6 World                
## 7 Yugoslavia
```

```r
# Filter out these entities and store results in a new data frame
supply_new <- supply %>% 
  filter(!is.na(code), nchar(code) == 3) %>% 
  arrange(entity, year)
```

Add a variable for world region via the `countrycode` package in case I want to group countries by region. 


```r
# Add a column for region (as defined in the World Bank Development Indicators)
supply_new$region <- countrycode(supply_new$entity, "country.name", "region")

# Timor hasn't been allocated to a region
supply_new %>% 
  filter(is.na(region)) %>% 
  summarise(unique(entity))
```

```
## # A tibble: 1 x 1
##   `unique(entity)`
##   <chr>           
## 1 Timor
```

```r
# Allocate Timor to a region
supply_new <- supply_new %>% 
  mutate(region = case_when(
    entity == "Timor" ~ "East Asia & Pacific",
    TRUE ~ as.character(region))
    )

# Region levels
region_levels = c("Middle East & North Africa", 
                  "Sub-Saharan Africa", 
                  "East Asia & Pacific",
                  "South Asia", 
                  "Europe & Central Asia",
                  "North America", 
                  "Latin America & Caribbean")

supply_new$region = factor(supply_new$region, levels = region_levels)
```


#### Distribution of fish and seafood supply per capita
Have a closer look at the fish and seafood supply per capita variable. `kg_capita` is skewed right. There are only a few instances where the value is above 75kg. Note that data for all years is bundled together in the charts below; we'll look at individual years later on.


```r
kg_capita_histogram <- supply_new %>% 
  ggplot(aes(x = kg_capita)) +
  geom_histogram(bins = 100) +
  labs(title = "Histogram: Fish and seafood supply per capita (kg)")

kg_capita_histogram +
    geom_vline(xintercept = 75, colour = "darkorange2", linetype = "dashed", size = 1)
```

<img src="README_files/figure-html/kg_capita_histogram-1.png" title="Histogram showing the distribution of the kg_capita variable. The distribution is skewed right. There are few instances where the value is above 75kg." alt="Histogram showing the distribution of the kg_capita variable. The distribution is skewed right. There are few instances where the value is above 75kg." width="100%" style="display: block; margin: auto;" />

Break this information down by world region. In all regions the distribution is skewed right.


```r
kg_capita_histogram +
  facet_wrap(~ region, ncol = 2) +
  theme(panel.border = element_rect(colour = "grey40", fill = FALSE)) +
  labs(title = "Histogram: Fish and seafood supply per capita (kg), by region")
```

<img src="README_files/figure-html/kg_capita_histogram_regional-1.png" title="Histogram showing the distribution of the kg_capita variable by region. In all regions the distribution is skewed right. There are few instances where the value is above 75kg." alt="Histogram showing the distribution of the kg_capita variable by region. In all regions the distribution is skewed right. There are few instances where the value is above 75kg." width="100%" style="display: block; margin: auto;" />

**Q:** Are the high values (> 75 kg) associated with certain countries?   
**A:** Yes, only three countries (Iceland, Kiribati and Maldives) have had an annual per capita supply value above 75kg. The only country with any values above 100kg is Maldives; an island nation with a relatively small population (just over half a million people).


```r
# List countries with an annual per capita supply value above 75kg
supply_new %>% 
  filter(kg_capita > 75) %>% 
  group_by(entity) %>% 
  summarise(n_years_above_75 = n(),
            first_year = min(year),
            last_year = max(year),
            max_kg_capita = max(kg_capita)) %>% 
  arrange(-max_kg_capita) %>% 
  ungroup()
```

```
## # A tibble: 3 x 5
##   entity   n_years_above_75 first_year last_year max_kg_capita
##   <chr>               <int>      <dbl>     <dbl>         <dbl>
## 1 Maldives               51       1967      2017         192. 
## 2 Iceland                46       1972      2017          92.3
## 3 Kiribati               12       1993      2017          76.9
```

To get a better idea of the distribution of supply per capita across all countries for each year, I'm going to create a strip plot.   

I'm going to highlight the three outlier countries mentioned above and add a line showing the median per capita value in each year. One thing to note is that each country has a different population size, but this isn't accounted for in the median value - each country has been given equal weighting in this calculation.

The strip plot clearly shows that Iceland, Kiribati and the Maldives are above the norm (Maldives is all over the place!). It also shows that the distribution of per capita supply is right-skewed in all years (the median value is skewed towards the bottom of the strip plot). It would appear there's quite a bit of variation amongst countries.  


```r
# Colour palette for outliers
outlier_colours = c("Iceland" = "cyan3", "Kiribati" = "gold2", "Maldives" = "firebrick1")

# Build basic strip plot
plot_by_year <- supply_new %>% 
  ggplot(aes(x = year, y = kg_capita)) +
  # Supply per year - a point for each country
  geom_point(alpha = 0.3) +
  # Highlight outlier countries
  geom_point(data = supply_new %>% filter(entity %in% c("Iceland", "Kiribati", "Maldives")), 
             aes(colour = entity), size = 1) +
  scale_colour_manual(values = outlier_colours) +
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, 5)) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 25)) +
  theme(axis.title.y = element_text(margin = margin(r = -20), angle = 0, vjust = 1.01),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Fish and seafood supply per capita, by year",
       subtitle = "Each point represents an individual country (outlier countries highlighted); Kg per capita (1961 - 2017)",
       x = "Year",
       y = "kg per capita",
       colour = ""
    )

# Add the median line
plot_by_year +
  # Show median line
  stat_summary(fun.y = median, geom = "line", size = 1, colour = "deepskyblue2") +
  # Label the median line
  geom_text(aes(x = 2019.5, y = 14.5, label = "Median", stat = "unique"), size = 4, colour = "grey40")
```

<img src="README_files/figure-html/plot_by_year-1.png" title="Strip plot showing the distribution of fish and seafood supply per capita (kg) across all countries in each year. Three outlier countries are highlighted (Iceland, Kiribati, Maldives). The median per capita value for each year is also shown and suggests that data are right-skewed." alt="Strip plot showing the distribution of fish and seafood supply per capita (kg) across all countries in each year. Three outlier countries are highlighted (Iceland, Kiribati, Maldives). The median per capita value for each year is also shown and suggests that data are right-skewed." width="100%" style="display: block; margin: auto;" />

Now let's break things down by world region. Within their own region, Iceland and the Maldives are still outliers. Kiribati is slightly higher than the other countries in East Asia and Pacific.


```r
# Build plot
plot_by_year_region <- plot_by_year +
  facet_wrap(~ region, ncol = 2) +
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50)) +
  theme(
    panel.border = element_rect(colour = "grey40", fill = FALSE),
    axis.title.y = element_text(margin = margin(r = -20), angle = 0, vjust = 1.02)
    ) +
  labs(title = "Fish and seafood supply per capita, by year and region",
       subtitle = "Each point represents an individual country (outlier countries highlighted); Kg per capita (1961 - 2017)",
       x = "Year",
       y = "kg per capita",
       colour = ""
    )

# Add the median line
plot_by_year_region +
  stat_summary(fun.y = median, geom = "line", size = 1, colour = "deepskyblue2")
```

<img src="README_files/figure-html/plot_by_year_region-1.png" title="Strip plot showing the distribution of fish and seafood supply per capita (kg) across all countries in each year, by world region. Three outlier countries are highlighted (Iceland, Kiribati, Maldives)." alt="Strip plot showing the distribution of fish and seafood supply per capita (kg) across all countries in each year, by world region. Three outlier countries are highlighted (Iceland, Kiribati, Maldives)." width="100%" style="display: block; margin: auto;" />

Show the above chart again, but this time free the y-axis scale for each region so that we can get a closer look at what's going on in each area of the world. There appears to be quite a bit of variation in per capita supply even within world regions.


```r
plot_by_year_region +
  stat_summary(fun.y = median, geom = "line", size = 1.2, colour = "deepskyblue2") +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  scale_y_continuous()
```

<img src="README_files/figure-html/plot_by_year_region_free_y-1.png" title="Strip plot showing the distribution of fish and seafood supply per capita (kg) across all countries in each year, by world region. The three outlier countries are highlighted (Iceland, Kiribati, Maldives). The y-axis scale varies by region." alt="Strip plot showing the distribution of fish and seafood supply per capita (kg) across all countries in each year, by world region. The three outlier countries are highlighted (Iceland, Kiribati, Maldives). The y-axis scale varies by region." width="100%" style="display: block; margin: auto;" />


### Wrangle Data
My plan is to plot fish and seafood supply per capita on a world map to create a choropleth. To be able to do this I need to get the data into a format that's easy to join to the `world` boundary data.   

First, pivot the `supply_new` data into a wider format so that each row represents one entity (country). 


```r
# Pivot wider
supply_wide <- supply_new %>% 
  pivot_wider(names_from = year, values_from = kg_capita, names_prefix = "Y_",)

# Check that there is now only one row per entity
count(supply_wide, entity) %>% 
  filter(n > 1)
```

```
## # A tibble: 0 x 2
## # ... with 2 variables: entity <chr>, n <int>
```

Now join the `world` data to the `supply_wide` data with a left_join(). 


```r
# Join the boundary data and the supply data
world_supply_wide <- left_join(world, supply_wide, by = c("iso_a3" = "code"))
```

Finally, make the `world_supply_wide` data tidy again, by pivoting longer, so that the data for each year is on a separate row.


```r
# Make the data tidy again by pivoting longer
world_supply_narrow <- world_supply_wide %>% 
  pivot_longer(cols = starts_with("Y_"), names_to = "year", values_to = "kg_capita") %>% 
  mutate(year = as.numeric(gsub("Y_", "", year)))
```


### Visualise Data
My first visualisation attempt is a basic world map showing fish and seafood supply per capita (kg) for 2017 (the most recent year for which data are available). I'm using the continuous `kg_capita` variable to determine the colour of each country. Because `kg_capita` is continuous, the colour scale will also be continuous. I'm going to try out a couple of different map projections.

One of the first things this chart makes obvious is that data isn't available for every country in every year (shaded grey on the map). The default continuous colour scale isn't great in this instance; it isn't easy to differentiate between countries of high / low supply and (perhaps counter-intuitively) countries with the lowest supply per capita are the darkest. For this analysis, I think I prefer the Robinson projection.


```r
# Build basic plot
map_2017_basic <- world_supply_narrow %>% 
  filter(year == 2017) %>%
  # Map fill colour to kg_capita
  ggplot(aes(fill = kg_capita)) +
  # Plot map with thin white boundaries
  geom_sf(color = "#ffffff", size = 0.1)

# Apply World Robinson projection
map_2017_basic_robinsons <- map_2017_basic +
  coord_sf(crs = "ESRI:54030") +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Robinson projection"
       )

# Apply World Mollweide projection
map_2017_basic_mollweide <- map_2017_basic +
  coord_sf(crs = "ESRI:54009") +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Mollweide projection"
       )

# The Patchwork package is loaded, so I can use it to display these charts side by side
map_2017_basic_robinsons + map_2017_basic_mollweide
```

<img src="README_files/figure-html/map_2017_basic_projections-1.png" title="Two choropleth maps showing country level fish and seafood supply per capita (kg) for 2017. The colour scale is continuous. One map uses the Robinson projection; the other map uses the Mollweide projection." alt="Two choropleth maps showing country level fish and seafood supply per capita (kg) for 2017. The colour scale is continuous. One map uses the Robinson projection; the other map uses the Mollweide projection." width="100%" style="display: block; margin: auto;" />

Now I'm going to try out a few alternative continuous colour scales (Viridis, CartoColor, ColorBrewer). Note that ColorBrewer is really designed to work with discrete scales, but I can make it work with a continuous one by using the scale_fill_gradientn() function.

I think all of these are an improvement on the default palette.


```r
# Viridis - option viridis
map_2017_basic_viridis_c_1 <- map_2017_basic_robinsons +
  scale_fill_viridis_c(direction = -1, 
                       option = "viridis", 
                       na.value = "#dddddd",
                       limits = c(0, 100)
                       ) +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Viridis palette (option viridis)"
       )

# Viridis - option inferno
map_2017_basic_viridis_c_2 <- map_2017_basic_robinsons +
  scale_fill_viridis_c(direction = -1, 
                       option = "inferno", 
                       na.value = "#dddddd",
                       limits = c(0, 100)
                       ) +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Viridis palette (option inferno)"
       )

# CARTO - palette BluYl
map_2017_basic_carto_c_1 <- map_2017_basic_robinsons +
  scale_fill_carto_c(direction = 1,
                     palette = "BluYl",
                     na.value = "#dddddd",
                     limits = c(0, 100)
                     ) +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "CARTO palette (palette BluYl)"
       )

# CARTO - palette BluYl
map_2017_basic_carto_c_2 <- map_2017_basic_robinsons +
  scale_fill_carto_c(direction = 1,
                     palette = "Teal",
                     na.value = "#dddddd",
                     limits = c(0, 100)
                     ) +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "CARTO palette (palette Teal)"
       )

# ColorBrewer - palette YlGnBu
map_2017_basic_brewer_1 <- map_2017_basic_robinsons +
  scale_fill_gradientn(colours=brewer.pal(9,"YlGnBu"), 
                       na.value = "#dddddd"
                       ) +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "ColorBrewer palette (palette YlGnBu)"
       )

# ColorBrewer - palette PuBu
map_2017_basic_brewer_2 <- map_2017_basic_robinsons +
  scale_fill_gradientn(colours=brewer.pal(9,"PuBu"), 
                       na.value = "#dddddd"
                       ) +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "ColorBrewer palette (palette PuBu)"
       )

# Patch the charts together
(map_2017_basic_viridis_c_1 + map_2017_basic_viridis_c_2) / 
  (map_2017_basic_carto_c_1 + map_2017_basic_carto_c_2) / 
    (map_2017_basic_brewer_1 + map_2017_basic_brewer_2)
```

<img src="README_files/figure-html/map_2017_basic_colours-1.png" title="Panel of choropleth maps showing country level fish and seafood supply per capita (kg) for 2017. Each map uses a different continuous colour palette." alt="Panel of choropleth maps showing country level fish and seafood supply per capita (kg) for 2017. Each map uses a different continuous colour palette." width="100%" style="display: block; margin: auto;" />

Although changing the default colour palette helps differentiate between countries with high / low supply, I still don't think the map is as clear as it could be. As highlighted above, the distribution of `kg_capita` is right skewed, so a large proportion of each colour scale is being hi-jacked by the very high supply values in Iceland and the Maldives. I going to try using a discrete scale to see if this helps.   

The break points in this discrete scale will strongly influence how the map looks. My natural instinct is to go for equally sized intervals, but because the data are skewed-right this might not be very helpful. I'm going to look at three different options.  


```r
world_supply_narrow  <- world_supply_narrow %>% 
  mutate(kg_capita_cut_1 = cut(kg_capita, breaks = c(-0.1, 10, 20, 30, 40, 50, 60, 70, 80, 200))) %>% 
  mutate(kg_capita_cut_2 = cut(kg_capita, breaks = c(-0.1, 5, 10, 15, 20, 30, 40, 50, 75, 200))) %>% 
  mutate(kg_capita_cut_3 = cut(kg_capita, breaks = c(-0.1, 2.5, 5, 10, 20, 30, 40, 50, 75, 200)))

p_cut_1 <- world_supply_narrow %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = kg_capita_cut_1)) +
  geom_bar()

p_cut_2 <- world_supply_narrow %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = kg_capita_cut_2)) +
  geom_bar()

p_cut_3 <- world_supply_narrow %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = kg_capita_cut_3)) +
  geom_bar()

# Patch the charts together
p_cut_1 / p_cut_2 / p_cut_3
```

<img src="README_files/figure-html/cut_distributions-1.png" title="Three bar charts showing the distribution of fish and seafood supply per capita (kg) values for 2017. Each version uses a different discrete scale to bin the values." alt="Three bar charts showing the distribution of fish and seafood supply per capita (kg) values for 2017. Each version uses a different discrete scale to bin the values." width="100%" style="display: block; margin: auto;" />



```r
map_2017_discrete_1 <- world_supply_narrow %>% 
  filter(year == 2017) %>%
  # Map fill colour to discrete variable kg_capita_cut_1
  ggplot(aes(fill = kg_capita_cut_1)) +
  geom_sf(color = "#ffffff", size = 0.1) +
  scale_fill_brewer(palette = "YlGnBu", na.value = "#dddddd") +
  coord_sf(crs = "ESRI:54030") +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Discrete scale option 1"
       )

map_2017_discrete_2 <- world_supply_narrow %>% 
  filter(year == 2017) %>%
  # Map fill colour to discrete variable kg_capita_cut_2
  ggplot(aes(fill = kg_capita_cut_2)) +
  geom_sf(color = "#ffffff", size = 0.1) +
  scale_fill_brewer(palette = "YlGnBu", na.value = "#dddddd") +
  coord_sf(crs = "ESRI:54030") +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Discrete scale option 2"
       )

map_2017_discrete_3 <- world_supply_narrow %>% 
  filter(year == 2017) %>%
  # Map fill colour to discrete variable kg_capita_cut_3
  ggplot(aes(fill = kg_capita_cut_3)) +
  geom_sf(color = "#ffffff", size = 0.1) +
  scale_fill_brewer(palette = "YlGnBu", na.value = "#dddddd") +
  coord_sf(crs = "ESRI:54030") +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Discrete scale option 3"
       )
  
map_2017_discrete_1 /
  map_2017_discrete_2 /
  map_2017_discrete_3
```

<img src="README_files/figure-html/map_2017_basic_discrete-1.png" title="Three choropleth maps showing country level fish and seafood supply per capita (kg) for 2017. Each version uses a different discrete scale to determine how colour is mapped to each country." alt="Three choropleth maps showing country level fish and seafood supply per capita (kg) for 2017. Each version uses a different discrete scale to determine how colour is mapped to each country." width="100%" style="display: block; margin: auto;" />

I think the third option (`kg_capita_cut_3`) makes it easier to see countries with very high / low supply, but it also makes it easier to see more subtle differences within continents like Africa. I'm going to use `kg_capita_cut_3` to build the final chart for the 2017 data.


```r
map_final_2017 <- world_supply_narrow %>% 
  filter(year == 2017) %>%
  # Map fill colour to discrete variable kg_capita_cut_3
  ggplot(aes(fill = kg_capita_cut_3)) +
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
  theme(legend.position = "right") +
  labs(title = "Fish and seafood supply per capita (kg), 2017",
       subtitle = "Average supply of fish and seafood across the population, measured in kilograms per person per year.",
       # Add some information at the bottom of the chart to give necessary context to chart
       caption = "Supply is defined as food available for human consumption. The values presented in this chart are based on per capita food supply at the consumer level; they do not account for food\nwaste at the consumer level and therefore overestimate the average amount of food actually consumed. Data are inclusive of all fish species and major seafood commodities, including\ncrustaceans, cephalopods and other mollusc species.\n  \nTidy Tuesday: Week 42, 2021 | Data source: United Nations Food and Agricultural Organization (via OurWorldInData.org/seafood-production) | Visualisation: @Fi_Lees"
       )

map_final_2017
```

<img src="README_files/figure-html/map_final_2017-1.png" title="Choropleth map showing country level fish and seafood supply per capita (kg) for 2017. There is wide variation across the world, ranging from less than 2.5 kg per capital in some African and Asian countries to more than 75 kg in Iceland." alt="Choropleth map showing country level fish and seafood supply per capita (kg) for 2017. There is wide variation across the world, ranging from less than 2.5 kg per capital in some African and Asian countries to more than 75 kg in Iceland." width="100%" style="display: block; margin: auto;" />

Save this visualisation.


```r
ggsave("fish_supply_world_map_2017.png", map_final_2017 , width = 12, height = 7, units = "in", dpi = 300)
```


Finally, lets have a look at a panel of choropleths showing country level fish and seafood supply per capita (kg) every decade between 1965 and 2015. Unfortunately, there's quite a bit of missing data for the earlier years in Africa and Asia, so it's not easy to see a clear trend.


```r
map_final_multi_year <- world_supply_narrow %>% 
  filter(year %in% c(1965, 1975, 1985, 1995, 2005, 2015)) %>%
  ggplot(aes(fill = kg_capita_cut_3)) +
  geom_sf(colour = "#ffffff", size = 0.1) +
  scale_fill_brewer(palette = "YlGnBu", 
                    na.value = "#dddddd",
                    name = "Per capita (kg)",
                    labels = c("< 2.5", "2.5 - 5", "5 - 10", "10 - 20", "20 - 30", 
                               "30 - 40", "40 - 50", "50 - 75", "> 75", "No data")) +
  coord_sf(crs = "ESRI:54030") +
  facet_wrap(~ year, ncol = 2) +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Fish and seafood supply per capita (kg), 1965 - 2015",
       subtitle = "Average supply of fish and seafood across the population, measured in kilograms per person per year.",
       caption = "Supply is defined as food available for human consumption. The values presented in this chart are based on per capita food supply at the consumer level; they do not account for food\nwaste at the consumer level and therefore overestimate the average amount of food actually consumed. Data are inclusive of all fish species and major seafood commodities, including\ncrustaceans, cephalopods and other mollusc species.\n\nTidy Tuesday: Week 42, 2021 | Data source: United Nations Food and Agricultural Organization (via OurWorldInData.org/seafood-production) | Visualisation: @Fi_Lees"
       )

map_final_multi_year
```

<img src="README_files/figure-html/map_multi_year-1.png" title="Choropleth map showing country level fish and seafood supply per capita (kg) every decade fom 1965 to 2015. There is wide variation across the world, ranging from less than 2.5 kg per capital in some African and Asian countries to more than 75 kg in Iceland." alt="Choropleth map showing country level fish and seafood supply per capita (kg) every decade fom 1965 to 2015. There is wide variation across the world, ranging from less than 2.5 kg per capital in some African and Asian countries to more than 75 kg in Iceland." width="100%" style="display: block; margin: auto;" />

### Final Notes
Well, I learned a lot about maps this week, but there's still so much to know...


### Session Information

```r
sessionInfo()
```

```
## R version 4.1.1 (2021-08-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19043)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United Kingdom.1252 
## [2] LC_CTYPE=English_United Kingdom.1252   
## [3] LC_MONETARY=English_United Kingdom.1252
## [4] LC_NUMERIC=C                           
## [5] LC_TIME=English_United Kingdom.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] forcats_0.5.1       stringr_1.4.0       dplyr_1.0.7        
##  [4] purrr_0.3.4         readr_2.0.2         tidyr_1.1.4        
##  [7] tibble_3.1.5        ggplot2_3.3.5       tidyverse_1.3.1    
## [10] patchwork_1.1.1     RColorBrewer_1.1-2  rcartocolor_2.0.0  
## [13] countrycode_1.3.0   rnaturalearth_0.1.0 sf_1.0-3           
## 
## loaded via a namespace (and not attached):
##  [1] fs_1.5.0           lubridate_1.8.0    bit64_4.0.5        httr_1.4.2        
##  [5] tools_4.1.1        backports_1.2.1    bslib_0.3.1        utf8_1.2.2        
##  [9] R6_2.5.1           KernSmooth_2.23-20 rgeos_0.5-8        DBI_1.1.1         
## [13] colorspace_2.0-2   withr_2.4.2        sp_1.4-5           tidyselect_1.1.1  
## [17] bit_4.0.4          curl_4.3.2         compiler_4.1.1     textshaping_0.3.5 
## [21] cli_3.0.1          rvest_1.0.1        xml2_1.3.2         labeling_0.4.2    
## [25] sass_0.4.0         scales_1.1.1       classInt_0.4-3     proxy_0.4-26      
## [29] systemfonts_1.0.2  digest_0.6.28      rmarkdown_2.11     pkgconfig_2.0.3   
## [33] htmltools_0.5.2    dbplyr_2.1.1       fastmap_1.1.0      highr_0.9         
## [37] rlang_0.4.11       readxl_1.3.1       rstudioapi_0.13    jquerylib_0.1.4   
## [41] generics_0.1.0     farver_2.1.0       jsonlite_1.7.2     vroom_1.5.5       
## [45] magrittr_2.0.1     Rcpp_1.0.7         munsell_0.5.0      fansi_0.5.0       
## [49] lifecycle_1.0.1    stringi_1.7.5      yaml_2.2.1         snakecase_0.11.0  
## [53] grid_4.1.1         parallel_4.1.1     crayon_1.4.1       lattice_0.20-45   
## [57] haven_2.4.3        hms_1.1.1          knitr_1.36         pillar_1.6.3      
## [61] reprex_2.0.1       glue_1.4.2         evaluate_0.14      modelr_0.1.8      
## [65] vctrs_0.3.8        tzdb_0.1.2         cellranger_1.1.0   gtable_0.3.0      
## [69] assertthat_0.2.1   xfun_0.26          janitor_2.1.0      broom_0.7.9       
## [73] e1071_1.7-9        ragg_1.1.3         class_7.3-19       viridisLite_0.4.0 
## [77] units_0.7-2        ellipsis_0.3.2
```


