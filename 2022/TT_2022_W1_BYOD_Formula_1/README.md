## Formula 1 Motor Racing
This week is "bring your own data week", so I'm going to revisit one of my favourite Tidy Tuesday datasets from 2021 (week 37), the [Formula 1 dataset](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-07/readme.md) from [Ergast API](https://ergast.com/mrd/).  

(To be honest, I didn't have time to do this during the official "bring your own data week", so I'm catching-up in March 2022. The new season of Drive to Survive has just landed on Netflix - it reminded me how much I liked working with the F1 dataset).    


### Objectives
When I first used this dataset, the 2021 F1 Season was still ongoing. Now that the season is complete, my aim is to answer the following questions: 

**1**: What where the final constructor standings at the end of the 2021 F1 Season?  

**2**: What where the final driver standings at the end of the 2021 F1 Season?  

**3**: How did each driver perform in comparison to his team mate?  


### Learning Points
The main thing I learned about this week was:  

- **{ggrepel}:** Up until now I've used geom_text(), or geom_label(), or annotate() to place annotation text on my charts. These methods work well, but sometimes it can take quite a bit of time / messing about to make sure text is placed correctly and doesn't overlap with other elements in the chart. The geom_label_repel() function works a lot like geom_label(), however the labels automatically repel away from each other and from the data points. This makes it easier to ensure that text / data points don't overlap.   


### Load Packages


```r
library(showtext)
library(ggrepel)
library(tidyverse)
```


### Import Data
To ensure I get the most up-to-date information, I'm going to download the data directly from the Ergast API. I'll store it on my local drive.  


```r
# Download data
#download.file("http://ergast.com/downloads/f1db_csv.zip", destfile = "Data/f1db.zip")

# Unzip data into a folder on my local drive called "Data"
#unzip("data/f1db.zip", overwrite = TRUE, exdir = "Data")
```

Next, I'll import the data and load the bits I'm interested in (there's lots of information in this dataset I'm not going to use today, e.g. lap times, pit stops).    


```r
# Pull all the csv files into a list, removing path and file extension info
f1_data <- map(fs::dir_ls("Data/", glob = "*.csv"), read_csv) %>%
  set_names(nm = (basename(names(.)) %>% tools::file_path_sans_ext()))

# Load the data of interest
races <- f1_data$races
results <- f1_data$results
driver_standings <- f1_data$driver_standings
drivers <- f1_data$drivers
constructor_standings <- f1_data$constructor_standings
constructors <- f1_data$constructors
```


### Wrangle Data
I'm going to pull out the various bits of (2021) data I'm interested in, then I'm going to join it all together in one tibble.  


```r
# Get race details for 2021
races_2021 <- races %>% 
  filter(year == 2021) %>% 
  rename("raceName" = "name") %>% 
  mutate(
    # Strip 'Grand Prix' out of race name
    raceName = gsub(" Grand Prix", "", raceName),
    # Create variable that combines race name and round
    raceNameNum = str_c(raceName, ": ", as.character(round)) 
    ) %>% 
  select(-date, -time, -url) %>% 
  arrange(round)   

# Get driver/constructor mapping for each 2021 race
driver_mapping_2021 <- results %>% 
  filter(raceId %in% races_2021$raceId) %>% 
  select(raceId:constructorId, positionOrder, points) %>% 
  rename(
    "racePosition" = "positionOrder",
    "racePoints" = "points"
    )
  
# Get driver standing info for 2021
driver_standings_2021 <- driver_standings %>% 
  filter(raceId %in% races_2021$raceId) %>% 
  # Add driver info
  left_join(
    select(drivers, c(driverId, code, surname, nationality)), 
    by = "driverId"
    ) %>% 
  rename(
    "driverPoints" = "points",
    "driverPosition" = "position",
    "driverWins" = "wins",
    "driverCode" = "code",
    "driverSurname" = "surname",
    "driverNationality" = "nationality"
    ) %>% 
  # Add driver/constructor mapping for each race
  left_join(driver_mapping_2021, by = c("driverId", "raceId")) %>% 
  select(driverId, driverCode:driverNationality, raceId:racePoints, -positionText) %>% 
  arrange(driverId, raceId)

# Kubica stood in for Raikkonen in a couple of races
# Assign a constructor (Alfa Romeo) to these drivers for the races they didn't drive in
driver_standings_2021 <- driver_standings_2021 %>% 
  mutate(constructorId = ifelse(
    is.na(constructorId) & driverCode %in% c("KUB", "RAI"), 
    51, 
    constructorId)
    )

# Get constructor standing info for 2021
constructor_standings_2021 <- constructor_standings %>% 
  filter(raceId %in% races_2021$raceId) %>% 
  # Add constructor info
  left_join(
    select(constructors, c(-url)), 
    by = "constructorId"
    ) %>% 
  rename(
    "constructorPoints" = "points",
    "constructorPosition" = "position",
    "constructorWins" = "wins",
    "constructorName" = "name",
    "constructorNationality" = "nationality"
    ) %>% 
  select(constructorId, constructorName, constructorNationality, raceId:constructorWins, -positionText) %>% 
  arrange(constructorId, raceId)

# Join all the 2021 data together in one tibble
race_driver_constructor_2021 <- races_2021 %>% 
  right_join(driver_standings_2021, by = "raceId") %>% 
  left_join(constructor_standings_2021, by = c("constructorId", "raceId")) %>% 
  select(raceId:driverNationality, racePosition, racePoints, driverPoints:constructorWins) %>% 
  arrange(round, racePosition)
```


### Visualise Data
Now I have all the 2021 data in one tibble, I'm ready to start building my visualisation.  

First, I'll load the font I want to use in my chart.  


```r
# Fonts
showtext_auto(enable = TRUE)
font_add_google("Alfa Slab One")
font_add_google("Titillium Web")
```

There are a few variables I want to reformat for my chart. I'll do this first and then pass the data to ggplot() and build the chart.   


```r
# Create data to be used in the chart
data_for_chart <- race_driver_constructor_2021 %>% 
  # Select data for the final race of the season
  slice(which(round == max(round))) %>% 
  mutate(
    # Set a flag to identify reserve drivers (Kubica)
    reserveDriver = ifelse(driverSurname == "Kubica", TRUE, FALSE),
    # Change driver names to upper case, adding minor tweaks as required
    driverSurname = toupper(ifelse(driverSurname == "Kubica", "(reserve driver)\nKubica", driverSurname)),
    # Make minor tweaks to constructor names
    constructorName = case_when(
      constructorName == "Alpine F1 Team" ~ "Alpine",
      constructorName == "Haas F1 Team" ~ "Haas",
      TRUE ~ constructorName),
    # Add constructor name and position together to create a new variable
    constructorStanding = toupper(factor(str_c(as.character(constructorPosition), ":  ", constructorName))),
    # Reorder this new variable according to final constructor standings
    constructorStanding = fct_reorder(constructorStanding, -constructorPosition)
    )
```

Just to note, I've decide to show the final **position** of each constructor / driver, rather than final number of **points** earned. Either option is valid, but using position means it's easy to give each constructor / driver their own space in the chart. Constructors / drivers have a unique **position** in the standings, but some have the same number of points.  


```r
# Build the chart
p_final <- data_for_chart %>% 
  ggplot(aes(x = driverPosition, y = constructorStanding)) +
  # Line to connect drivers in each team
  geom_line(aes(group = constructorId), colour = "grey80", size = 1.5) +
  # Dot for each driver (reserve drivers in grey)
  geom_point(aes(colour = reserveDriver), size = 4) +
  scale_colour_manual(values = c("#e10600", "grey80")) +
  # Driver name text labels
  geom_label_repel(
    aes(label = driverSurname),
    family = "Titillium Web",
    size = 3.2, 
    colour = "grey20",
    fill = "white",
    hjust = 0.5,
    vjust = 0.5,
    nudge_y = 0.25,
    label.padding = unit(0.2, "cm"),
    label.size = NA,
    lineheight = 1
    ) +
  # Scales
  scale_y_discrete(position = "right") +
  scale_x_reverse(limits = c(21, 1), breaks = seq(1, 21, 1)) +
  # Styling
  theme_minimal() +
  theme(
    text = element_text(family = "Titillium Web", colour = "grey20", size = 12),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.title = element_text(family = "Alfa Slab One", colour = "#e10600", size = 18, 
                              margin = margin(b = 5), hjust = 0),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0),
    plot.caption = element_text(size = 8, hjust = 0),
    plot.background = element_rect(colour = "white", fill = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    axis.title.y.right = element_text(vjust = 1.03, hjust = 0, angle = 0, margin = margin(l = -69, r = 0)),
    axis.title.x = element_text(hjust = 0.955, margin = margin(t = 10)),
    legend.position = "none"
    ) +
  # Titles
  labs(
    title = "FORMULA 1: 2021 STANDINGS",
    subtitle = "CONSTRUCTOR AND DRIVER FINISHING POSITIONS FOR THE 2021 SEASON",
    x = "DRIVER POSITION",
    y = "CONSTRUCTOR\nPOSITION",
    caption = "VISUALISATION: FIONA LEES (@FI_LEES) | SOURCE: ERGAST API | TIDY TUESDAY: WEEK 1, 2022"
    )

p_final 
```

<img src="README_files/figure-html/final_chart-1.png" title="Dumbell plot showing the final standing of each constructor and driver in the Formula 1 2021 Season. The top constructor was Mercedes, whose drivers finished second and third overall. However the top driver was Verstappen, who drives for Red Bull." alt="Dumbell plot showing the final standing of each constructor and driver in the Formula 1 2021 Season. The top constructor was Mercedes, whose drivers finished second and third overall. However the top driver was Verstappen, who drives for Red Bull." width="100%" style="display: block; margin: auto;" />



Save the plot as an image.  


```r
# Tell showtext to use DPI of 320 so text size is correct when chart is saved as an image
showtext_opts(dpi = 320)

# Save plot as image
ggsave("F1Standings_2021.png", p_final, width = 11, height = 8, units = "in", dpi = 320)

# Turn off showtext
showtext_auto(FALSE)
```


### Session Information

```r
sessionInfo()
```

```
## R version 4.1.3 (2022-03-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 22000)
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
##  [1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.8     purrr_0.3.4    
##  [5] readr_2.1.2     tidyr_1.2.0     tibble_3.1.6    tidyverse_1.3.1
##  [9] ggrepel_0.9.1   ggplot2_3.3.5   showtext_0.9-5  showtextdb_3.0 
## [13] sysfonts_0.8.8 
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.8.2      lubridate_1.8.0   assertthat_0.2.1  digest_0.6.29    
##  [5] utf8_1.2.2        R6_2.5.1          cellranger_1.1.0  backports_1.4.1  
##  [9] reprex_2.0.1      evaluate_0.15     highr_0.9         httr_1.4.2       
## [13] pillar_1.7.0      rlang_1.0.2       curl_4.3.2        readxl_1.3.1     
## [17] rstudioapi_0.13   jquerylib_0.1.4   rmarkdown_2.13    textshaping_0.3.6
## [21] bit_4.0.4         munsell_0.5.0     broom_0.7.12      compiler_4.1.3   
## [25] modelr_0.1.8      xfun_0.30         systemfonts_1.0.4 pkgconfig_2.0.3  
## [29] htmltools_0.5.2   tidyselect_1.1.2  fansi_1.0.2       crayon_1.5.0     
## [33] tzdb_0.2.0        dbplyr_2.1.1      withr_2.5.0       grid_4.1.3       
## [37] jsonlite_1.8.0    gtable_0.3.0      lifecycle_1.0.1   DBI_1.1.2        
## [41] magrittr_2.0.2    scales_1.1.1      cli_3.2.0         stringi_1.7.6    
## [45] vroom_1.5.7       farver_2.1.0      fs_1.5.2          xml2_1.3.3       
## [49] bslib_0.3.1       ragg_1.2.2        ellipsis_0.3.2    generics_0.1.2   
## [53] vctrs_0.3.8       tools_4.1.3       bit64_4.0.5       glue_1.6.2       
## [57] hms_1.1.1         parallel_4.1.3    fastmap_1.1.0     yaml_2.3.5       
## [61] colorspace_2.0-3  rvest_1.0.2       knitr_1.37        haven_2.4.3      
## [65] sass_0.4.0
```

