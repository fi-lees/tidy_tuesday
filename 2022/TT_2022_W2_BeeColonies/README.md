## Honey Bee Colonies
The [data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md#bee-colonies) this weeks comes from the [United States Department of Agriculture (USDA)](https://usda.library.cornell.edu/concern/publications/rn301137d?locale=en), and relates to honey bee colonies in the USA. The dataset contains quarterly information about the number of colonies; lost, added and renovated colonies; and colony health stressors.  


### Objectives
This week I'm going to produce a [cycle plot](https://policyviz.com/2021/02/09/the-cycle-plot/) to highlight seasonal variation in honey bee colony losses in the USA.  


### Learning Points
I learned about a number of things this week:  

- **Cycle Plots:** I've never used this type of chart before, but I saw one recently on the [PolicyViz website](https://policyviz.com/2021/02/09/the-cycle-plot/) and it made me think about how seasonal variation can best be visualised. 

- **Hexbin Maps:** The #TidyTuesday community went wild for hexbin maps this week because they look a bit like honeycombs. It was a perfect opportunity for me to try creating one too. I found a short tutorial on the [data-to-viz website](https://www.data-to-viz.com/graph/hexbinmap.html) that helped me.   

- **Purrr Package:** I'm still relatively new to R and so far I've focussed on getting to my intended end point without worrying too much about how efficient my code is. One of my objectives for 2022 is to start writing more efficient / elegant code. I sometimes break the "never copy and paste more than twice" rule of thumb (I've done it a few times in this file), but I used the `purrr::map()` and `purr::pwalk` functions this week to make some of my code more efficient. 


### Load Packages


```r
library(showtext)
library(geojsonio)
library(broom) 
library(mapproj)
library(png)
library(patchwork)
library(tidyverse)
```


### Import Data
I'm only going to use the `colony` data (I don't need to import the `stressor` data).  


```r
colony <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv"
  )
```

Let's have a quick look at the `colony` data.      


```r
view(colony)
Hmisc::describe(colony)
```

```
## colony 
## 
##  10  Variables      1222  Observations
## --------------------------------------------------------------------------------
## year 
##        n  missing distinct     Info     Mean      Gmd 
##     1222        0        7    0.978     2018    2.156 
## 
## lowest : 2015 2016 2017 2018 2019, highest: 2017 2018 2019 2020 2021
##                                                     
## Value       2015  2016  2017  2018  2019  2020  2021
## Frequency    188   188   188   188   188   188    94
## Proportion 0.154 0.154 0.154 0.154 0.154 0.154 0.077
## --------------------------------------------------------------------------------
## months 
##        n  missing distinct 
##     1222        0        4 
##                                                                               
## Value            April-June    January-March   July-September October-December
## Frequency               329              329              282              282
## Proportion            0.269            0.269            0.231            0.231
## --------------------------------------------------------------------------------
## state 
##        n  missing distinct 
##     1222        0       47 
## 
## lowest : Alabama       Arizona       Arkansas      California    Colorado     
## highest: Virginia      Washington    West Virginia Wisconsin     Wyoming      
## --------------------------------------------------------------------------------
## colony_n 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1175       47      284        1   123578   205973     3970     5500 
##      .25      .50      .75      .90      .95 
##     8000    17500    55500   147000   413000 
## 
## lowest :    1300    1600    1900    2000    2100
## highest: 3132880 3135340 3175330 3175960 3181180
## --------------------------------------------------------------------------------
## colony_max 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1150       72      278        1    79113   114569     5000     6500 
##      .25      .50      .75      .90      .95 
##     9000    21000    68750   159000   302750 
## 
## lowest :    1700    1900    2100    2500    2700
## highest: 1540000 1550000 1580000 1690000 1710000
## --------------------------------------------------------------------------------
## colony_lost 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1175       47      254        1    16551    28093      220      370 
##      .25      .50      .75      .90      .95 
##      950     2200     6500    21000    48600 
## 
## lowest :     20     30     40     50     60, highest: 444730 457100 484920 500020 502350
## --------------------------------------------------------------------------------
## colony_lost_pct 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1168       54       42    0.997    11.38    7.635        2        4 
##      .25      .50      .75      .90      .95 
##        6       10       15       21       25 
## 
## lowest :  1  2  3  4  5, highest: 40 41 45 48 52
## --------------------------------------------------------------------------------
## colony_added 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1139       83      256        1    17243    30137       50      100 
##      .25      .50      .75      .90      .95 
##      420     1800     6500    25000    61100 
## 
## lowest :     10     20     30     40     50, highest: 653470 661860 677690 725650 736920
## --------------------------------------------------------------------------------
## colony_reno 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1091      131      252        1    15279    27175       30       60 
##      .25      .50      .75      .90      .95 
##      260      960     4585    23000    64000 
## 
## lowest :     10     20     30     40     50, highest: 561160 632680 692850 715110 806170
## --------------------------------------------------------------------------------
## colony_reno_pct 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##      962      260       54    0.994    9.098    9.196        1        1 
##      .25      .50      .75      .90      .95 
##        2        6       12       20       28 
## 
## lowest :  1  2  3  4  5, highest: 57 62 65 68 77
## --------------------------------------------------------------------------------
```


### Wrangle Data
First, I'll tidy-up some variable types to make dealing with them easier later on. I'll also drop the variables I'm not going to use.   


```r
# Change `year` to numeric and `months` and `state` to factors
colony_new <- colony %>% 
  mutate(
    year = as.numeric(year),
    months = as_factor(months),
    state = as_factor(state)
  ) %>% 
  select(year:colony_lost_pct)
```

Next, I need to create a couple of summaries. The first summary will help me identify the states that have, on average, the most bee colonies. The second summary contains information for each state by quarter (across all years).


```r
# Create summary showing mean `colony_n`, 'colony_max` and 'colony_lost_pct` 
# for each state across all quarters and years  
colony_summary_overall <- colony_new %>% 
  group_by(state) %>% 
  summarise(
    n_quarters = n(),
    mean_colony_n = mean(colony_n, na.rm = TRUE),
    mean_colony_max = mean(colony_max, na.rm = TRUE),
    mean_colony_lost_pct = mean(colony_lost_pct, na.rm = TRUE)
    ) %>% 
  ungroup() 

# Create summary showing mean `colony_n`, 'colony_max` and 'colony_lost_pct` 
# for each state for each quarter across all years 
colony_summary_quarter <- colony_new %>% 
  group_by(months, state) %>% 
  summarise(
    n_years = n(),
    mean_colony_n = mean(colony_n, na.rm = TRUE),
    mean_colony_max = mean(colony_max, na.rm = TRUE),
    mean_colony_lost_pct = mean(colony_lost_pct, na.rm = TRUE),
    ) %>% 
  ungroup()
```

The next step is to pull out the data for the four states that have the most bee colonies. I also want data for the United States as a whole.


```r
# Get overall summary data for the four states with the 
# highest `mean_colony_max` + the USA as a whole
top_states <- colony_summary_overall %>% 
  mutate(colony_max_rank = min_rank(desc(mean_colony_max))) %>% 
  filter(colony_max_rank <= 4 | state == "United States") %>% 
  arrange(state)

# Get the colony data for the four states with the 
# highest `mean_colony_max` + the USA as a whole
colony_top_states <- colony_new %>%
  filter(state %in% top_states$state) %>% 
  group_by(months, state) %>% 
  # Add the min / max colony_lost_pct for each state / quarter
  mutate(
    min_colony_lost_pct = min(colony_lost_pct, na.rm = TRUE),
    max_colony_lost_pct = max(colony_lost_pct, na.rm = TRUE)
    ) %>% 
  ungroup() %>% 
  arrange(state, year, months)

# Get the quarterly summary data for the four states with the 
# highest `mean_colony_max` + the USA as a whole
colony_top_states_summary <- colony_summary_quarter %>%
  filter(state %in% top_states$state) %>% 
  arrange(state, months)
```

All I need now is the [hexbin map boundary data](https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map) required to draw a hexbin map for the USA.


```r
# Read in hexbin map boundary data for USA (spdf = Spatial Polygons Data Frame)
# source: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
spdf <- geojson_read("us_states_hexgrid.geojson", what = "sp")

# Remove the text "(United States)" from the name of each state (stored in `google_name`)
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Turn the spdf data into a tidy tibble for use within ggplot2
spdf_tidy <- tidy(spdf, region = "google_name")
```


### Visualise Data
I'm ready to start building my visualisation.  

First, I'll load the fonts I want to use.  


```r
# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Roboto Slab")
```

Next, I'll define a colour palette.  


```r
# Colours
honey_pale = "#f8d887"
honey_dark = "#c18d0b"
```

I'm going to create a hexbin map for the USA, plus one for each the four top states showing their location. I'll save these maps as images so that I can place them inside my cycle plot at a later stage.  


```r
# Base map showing all states in a pale honey colour
map_base <- ggplot() +
  geom_polygon(data = spdf_tidy, 
               aes(x = long, y = lat, group = group), 
               fill = honey_pale, color = "white") +
  theme_void() +
  coord_map()

# USA - show all states in a dark honey colour
map_usa <- map_base +
  geom_polygon(data = spdf_tidy, 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")
# California - show state in a dark honey colour
map_ca <- map_base +
  geom_polygon(data = filter(spdf_tidy, id == "California"), 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")
# Florida
map_fl <- map_base +
  geom_polygon(data = filter(spdf_tidy, id == "Florida"), 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")
# North Dakota
map_nd <- map_base +
  geom_polygon(data = filter(spdf_tidy, id == "North Dakota"), 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")
# Texas
map_tx <- map_base +
  geom_polygon(data = filter(spdf_tidy, id == "Texas"), 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")

# Create list containing all maps created above
map_list <- list(map_usa, map_ca, map_fl, map_nd, map_tx)

# Create a .png file name for each map
map_paths <- str_c(c("map_usa", "map_ca", "map_fl", "map_nd", "map_tx"), ".png")

# Save each map as a .png image using the file names created above
pwalk(list(map_paths, map_list), ggsave, dpi = 300)

# Create list containing all map .png files (read back in using png::readPNG) 
map_image_list <- map(map_paths, readPNG, native = TRUE, info = TRUE)

# Store name of each image in the list for ease of access later
names(map_image_list) <- str_sub(map_paths, 1, -5)
```

Now to create the cycle plot.  


```r
# Labels for facet strips
quarter_labels <- c("Jan - Mar", "Apr - Jun", "Jul - Sep", "Oct - Dec")
names(quarter_labels) <- levels(colony_new$months)

# Annotation text
annotation_text <- tibble(
  label = c("Average", "Range"),
  x = c(2015.4, 2018.7),
  y = c(7, 7),
  state = factor(c("United States", "United States")),
  months = factor(c("January-March", "January-March"))
)

# Annotation lines
annotation_line <- tibble(
  x = c(2015.6, 2018.9),
  xend = c(2015.6, 2018.9),
  y = c(7.7, 7.7),
  yend = c(15.28, 13),
  state = factor(c("United States", "United States")),
  months = factor(c("January-March", "January-March"))
)

# Initial cycle plot
p_initial <- colony_top_states %>%
  # Reorder states - USA first, followed by states in alphabetical order
  mutate(state = factor(state, levels = c("United States", "California", "Florida", "North Dakota", "Texas"))) %>% 
  ggplot(aes(x = year, y = colony_lost_pct)) +
  # Ribbon showing range of values for each quarter across all years
  geom_ribbon(aes(ymin = min_colony_lost_pct, ymax = max_colony_lost_pct), 
              fill = honey_pale, alpha = 0.5) +
  # Dashed horizontal line showing mean value for each quarter across all years
  geom_hline(data = colony_top_states_summary, 
             aes(yintercept = mean_colony_lost_pct), 
             colour = honey_dark, 
             linetype = "dashed"
             ) +
  # Line and points showing values for each year
  geom_line() +
  geom_point() +
  # Facet by state and quarter
  facet_grid(state ~ months, switch = "y", labeller = labeller(months = quarter_labels)) +
  # Annotation text
  geom_label(data = annotation_text, 
             aes(x = x, y = y, label = label),
             colour = "grey40",
             fill = "white",
             label.size = NA,
             label.padding = unit(0.0, "lines"),
             size = 3.5,
             hjust = 0.0,
             vjust = 0.7
             ) +
  # Annotation lines
  geom_segment(data = annotation_line,
               aes(x = x, xend = xend , y = y, yend = yend),
               colour = "grey40",
               size = 0.5
               ) +
  # Scale formatting
  scale_x_continuous(limits = c(2015, 2021), 
                     breaks = c(2015, 2021), 
                     position = "top"
                     ) +
  scale_y_continuous(limits = c(0, 22), 
                     breaks = seq(0, 20, 5),
                     position = "right",
                     labels = scales::label_percent(accuracy = 1, scale = 1)
                     ) +
  # Styling
  theme_minimal() +
  theme(text = element_text(family = "Lato", colour = "grey40"),
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(colour = "grey40", size = 11, face = "bold"),
        strip.text.y.left = element_text(angle = 0, vjust = 0.92, hjust = 0, margin = margin(r = 60)),
        strip.text.x.top = element_text(vjust = 1, margin = margin(b = 5)),
        axis.text = element_text(colour = "grey40", size = 10, hjust = c(0.05, 0.95)),
        axis.title.y.right = element_text(colour = "grey40", size = 10, hjust = 0.05, margin = margin(l = 10))
        ) +
  # Labels
  labs(x = "",
       y = "Colonies lost (%)"
       )

p_initial
```

<img src="README_files/figure-html/initial_chart-1.png" title="Cycle plot showing the percentage of honey bee colonies lost each calendar quarter (2015 - 2021), across the USA as a whole and in the four states with the highest number of colonies (California, Florida, North Dakota and Texas). Seasonal variation in honey bee colony losses differs across the USA." alt="Cycle plot showing the percentage of honey bee colonies lost each calendar quarter (2015 - 2021), across the USA as a whole and in the four states with the highest number of colonies (California, Florida, North Dakota and Texas). Seasonal variation in honey bee colony losses differs across the USA." width="100%" style="display: block; margin: auto;" />

Add the hexbin map images to the cycle plot and add plot titles.  


```r
# Add the hexbin map images to the plot
p_final <- p_initial +
    patchwork::inset_element(
    p = map_image_list$map_usa,
    t = 0.90,
    r = 0.190,
    l = 0.000,
    b = 0.77,
    align_to = "plot"
  ) +
  patchwork::inset_element(
    p = map_image_list$map_ca,
    t = 0.71,
    r = 0.190,
    l = 0.000,
    b = 0.58,
    align_to = "plot"
  ) +
  patchwork::inset_element(
    p = map_image_list$map_fl,
    t = 0.52,
    r = 0.190,
    l = 0.000,
    b = 0.39,
    align_to = "plot"
  ) +
  patchwork::inset_element(
    p = map_image_list$map_nd,
    t = 0.33,
    r = 0.190,
    l = 0.000,
    b = 0.20,
    align_to = "plot"
  ) +
  patchwork::inset_element(
    p = map_image_list$map_tx,
    t = 0.14,
    r = 0.190,
    l = 0.000,
    b = 0.01,
    align_to = "plot"
  )
  
# Add plot titles
p_final <- p_final +
  plot_annotation(
    title = "Seasonal variation in honey bee colony losses differs across the USA",
    subtitle = "Percentage of honey bee colonies lost each calendar quarter (2015 - 2021), across the USA as a whole and in the four\nstates with the highest number of colonies. The average (mean) loss across all years is shown for each quarter, along\nwith the range (minimum / maximum losses).",
    caption = "Notes:\n1. A lost colony is a completely failed colony with loss of most worker bees, and possibly the queen. Lost colonies are no longer viable.\n2. No data are available for April - June 2019 (data collection was suspended by USDA).\n3. Data for July - December 2021 are not available yet.\n \nTidy Tuesday: 2022, Week 2 | Visualisation: Fiona Lees (@Fi_Lees) | Data source: United States Department of Agriculture (USDA)",
    theme = theme(
      plot.title = element_text(family = "Roboto Slab", colour = "black", size = 16, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Lato", colour = "grey40", size = 11),
      plot.caption = element_text(family = "Lato", colour = "grey40", size = 10, hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
      )
    )

p_final 
```

<img src="README_files/figure-html/final_chart-1.png" title="Cycle plot showing the percentage of honey bee colonies lost each calendar quarter (2015 - 2021), across the USA as a whole and in the four states with the highest number of colonies (California, Florida, North Dakota and Texas). Seasonal variation in honey bee colony losses differs across the USA." alt="Cycle plot showing the percentage of honey bee colonies lost each calendar quarter (2015 - 2021), across the USA as a whole and in the four states with the highest number of colonies (California, Florida, North Dakota and Texas). Seasonal variation in honey bee colony losses differs across the USA." width="100%" style="display: block; margin: auto;" />

Save the plot as an image.  


```r
# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save plot as image
ggsave("Bee_Colony_Loss.png", p_final, width = 9, height = 12, units = "in", dpi = 300)

# Turn off showtext
showtext_auto(FALSE)
```


### Session Information

```r
sessionInfo()
```

```
## R version 4.1.2 (2021-11-01)
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
##  [1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7     purrr_0.3.4    
##  [5] readr_2.1.1     tidyr_1.1.4     tibble_3.1.6    ggplot2_3.3.5  
##  [9] tidyverse_1.3.1 patchwork_1.1.1 png_0.1-7       mapproj_1.2.7  
## [13] maps_3.4.0      broom_0.7.11    geojsonio_0.9.4 showtext_0.9-4 
## [17] showtextdb_3.0  sysfonts_0.8.5 
## 
## loaded via a namespace (and not attached):
##   [1] colorspace_2.0-2    ellipsis_0.3.2      class_7.3-19       
##   [4] htmlTable_2.4.0     base64enc_0.1-3     fs_1.5.2           
##   [7] httpcode_0.3.0      rstudioapi_0.13     proxy_0.4-26       
##  [10] farver_2.1.0        bit64_4.0.5         fansi_1.0.0        
##  [13] lubridate_1.8.0     xml2_1.3.3          splines_4.1.2      
##  [16] knitr_1.37          Formula_1.2-4       jsonlite_1.7.2     
##  [19] cluster_2.1.2       dbplyr_2.1.1        rgeos_0.5-9        
##  [22] compiler_4.1.2      httr_1.4.2          backports_1.4.1    
##  [25] assertthat_0.2.1    Matrix_1.4-0        fastmap_1.1.0      
##  [28] lazyeval_0.2.2      cli_3.1.0           htmltools_0.5.2    
##  [31] tools_4.1.2         gtable_0.3.0        glue_1.6.0         
##  [34] geojson_0.3.4       V8_4.0.0            Rcpp_1.0.7         
##  [37] cellranger_1.1.0    jquerylib_0.1.4     vctrs_0.3.8        
##  [40] crul_1.2.0          xfun_0.29           rvest_1.0.2        
##  [43] lifecycle_1.0.1     jqr_1.2.2           scales_1.1.1       
##  [46] vroom_1.5.7         ragg_1.2.1          hms_1.1.1          
##  [49] parallel_4.1.2      RColorBrewer_1.1-2  yaml_2.2.1         
##  [52] curl_4.3.2          gridExtra_2.3       sass_0.4.0         
##  [55] rpart_4.1-15        latticeExtra_0.6-29 stringi_1.7.6      
##  [58] highr_0.9           maptools_1.1-2      e1071_1.7-9        
##  [61] checkmate_2.0.0     rlang_0.4.12        pkgconfig_2.0.3    
##  [64] systemfonts_1.0.3   evaluate_0.14       lattice_0.20-45    
##  [67] sf_1.0-5            labeling_0.4.2      htmlwidgets_1.5.4  
##  [70] bit_4.0.4           tidyselect_1.1.1    magrittr_2.0.1     
##  [73] geojsonsf_2.0.1     R6_2.5.1            generics_0.1.1     
##  [76] Hmisc_4.6-0         DBI_1.1.2           pillar_1.6.4       
##  [79] haven_2.4.3         foreign_0.8-81      withr_2.4.3        
##  [82] units_0.7-2         survival_3.2-13     sp_1.4-6           
##  [85] nnet_7.3-16         modelr_0.1.8        crayon_1.4.2       
##  [88] KernSmooth_2.23-20  utf8_1.2.2          tzdb_0.2.0         
##  [91] rmarkdown_2.11      jpeg_0.1-9          grid_4.1.2         
##  [94] readxl_1.3.1        data.table_1.14.2   reprex_2.0.1       
##  [97] digest_0.6.29       classInt_0.4-3      textshaping_0.3.6  
## [100] munsell_0.5.0       bslib_0.3.1
```

