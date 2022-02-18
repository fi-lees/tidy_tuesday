## W.E.B. Du Bois Challenge 2022
The [data](https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022/challenge02) this week was provided by Anthony Starks, as part of the [Du Bois Visualisation Challenge 2022](https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022#readme). The goal of the challenge is to celebrate the legacy of W.E.B. Du Bois, by using modern tools to recreate his visualisations from the 1900 Paris Exposition. Anthony wrote a [great article](https://nightingaledvs.com/the-dubois-challenge/) for Nightingale to accompany the challenge.


### Objectives
I'm going to attempt challenge number 2. I'll try to recreate [Plate 22 - Assessed Valuation of all Taxable Property Owned by Georgia Negroes](https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/2022/challenge02/original-plate-22.jpg). Please note that the title is taken directly from the 1900 original and uses an antiquated term to refer to populations of colour.


### Learning Points
The main things I learned about this week were:  

- **W.E.B. Du Bois:** I've seen some of Du Bois' work before, but I spent a lot of time **really** looking at his beautiful visualisations this week. The process of trying to recreate one of his charts led me to see details I hadn't noticed before and helped me appreciate some of his subtle design decisions.

- **ggforce package:** I used a couple of features within the ggforce package to draw my chart this week. This is a great package with lots of interesting tools to explore further when I have time.


### Load Packages


```r
library(showtext)
library(ggforce)
library(tidyverse)
```


### Import Data
I can get the data from Anthony's GitHub page. I'll do a little tidying as I import the data.  


```r
df_raw <- readr::read_csv(
  "https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge02/data.csv"
  ) %>% 
  # Clean names
  janitor::clean_names() %>% 
  # Remove duplicate data for year 1880
  distinct()

view(df_raw)
```


### Wrangle Data
I want to make the area of each year's circle proportional to the valuation in dollars for that year. To do this I need to calculate the radius for each circle.  


```r
df_plate22 = df_raw %>% 
  mutate(year = factor(year, levels = c("1899", "1895", "1890", "1885", "1880", "1875")),
         # Use valuation_dollars (desired area of each circle) to calculate radius
         radius = sqrt(valuation_dollars / pi) / 100
         )
```

Now I want to check that the circle sizes look similar to the Du Bois original. I can use `ggforce::geom_circle` to draw the circles, using the radius values calculated above.  

Unfortunately, when I draw my circles (see below), I can see they look quite different to the original. For example, the central circle for 1875 looks too big.


```r
ggplot(df_plate22) +
  geom_circle(aes(x0 = 0, y0 = 0, r = radius)) +
  coord_fixed()
```

<img src="README_files/figure-html/check_radius_values_1-1.png" width="100%" style="display: block; margin: auto;" />

I have a decision to make. Should I size the circles according to the valuation in dollars, or should I try to match the sizes to the Du Bois original? In this instance, because my goal is to try to recreate Du Bois' chart, I'm going to manually adjust the circle sizes so that they look more like the original.  


```r
df_plate22 = df_plate22 %>% 
  mutate(radius_adjusted = c(9, 9.75, 14, 20.50, 21.25, 22))
```

Let's see what the adjusted circle sizes look like (see below). I think these new circles look closer to the original.    


```r
ggplot(df_plate22) +
  geom_circle(aes(x0 = 0, y0 = 0, r = radius_adjusted)) +
  coord_fixed()
```

<img src="README_files/figure-html/check_radius_values_2-1.png" width="100%" style="display: block; margin: auto;" />

Before I start putting my final chart together, I need to work out how to add the "wedges" that point toward the centre of the circles. I can do this using another tool from the `ggforce` package called `geom_arc_bar`.  

I can adjust the size and placement of a wedge by changing the x0, y0, r0, r, start and end aesthetics. 

- x0 and y0 are used to offset the wedge from the centre of the circle (where the centre is x0=0, y0=0).

- r0 is used to change the length of the wedge.  

- r determines how much of the pointy bit of the wedge is shown. r=0 will show the whole point, r=5 will chop a good bit of it off.  

- start / end are used to determine the start and end points of the wedge (the width).  


```r
ggplot(df_plate22) +
  geom_circle(aes(x0 = 0, y0 = 0, r = radius_adjusted)) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 22, r = 0, start = 0, end = 0.5), colour = "blue") +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 17, r = 5, start = 1, end = 2), colour = "red") +
  geom_arc_bar(aes(x0 = -3, y0 = 3, r0 = 22, r = 2, start = 5.5, end = 6), colour = "cyan3") +
  coord_fixed()
```

<img src="README_files/figure-html/draw_wedge-1.png" width="100%" style="display: block; margin: auto;" />

The Du Bois original has some jagged-edges around the wedges, where the coloured circles kind of bleed into each other. I could layer wedges on top of each other to get this sort of effect, but I'm going to keep things simple and omit this detail.


### Visualise Data
Right, I'm ready to start recreating Plate 22.  

I'm going to take a fairly manual approach to placing the wedges and text labels on my chart. I could create a tibble of parameters to pass into ggplot, but I want the chart to have a hand-drawn quality where things don't quite align. I'm going to manually adjust the values by eye until I get the look I'm aiming for.  

First, I'll load the fonts I want to use.  The [Du Bois style guide](https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf) suggests using Public Sans (a Google font).  


```r
# Fonts
showtext_auto(enable = TRUE)
font_add_google("Public Sans")
```

Next, I'll define a colour palette. The Du Bois style guide suggests colours to use, but I want my chart to look a bit more antiquated, so I'm going to pick out colours from the original [Plate 22](https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/2022/challenge02/original-plate-22.jpg).


```r
# Colours
col_red <- "#c31f35"
col_light_grey <- "#d9c7af"
col_gold <- "#edaf07"
col_blue <- "#2b4788"
col_tan <- "#af8f7e"
col_off_black <- "#141514"
col_off_white <- "#eae0d9"
col_parchment <- "#e3d2bd"
year_palette <- c(col_red, col_light_grey, col_gold, col_blue, col_tan, col_off_black)
```

Now I'll put the chart together. 


```r
p_plate22 <- ggplot(df_plate22) +
  # Circles
  geom_circle(aes(x0 = 0, y0 = 0, r = radius_adjusted, fill = year), 
              colour = col_tan
              ) +
  scale_fill_manual(values = year_palette) +
  # Year labels - size, colour and nudge label for each year separately  
  geom_text(aes(x = 0, y = 0 - radius_adjusted, label = year), 
            size = c(2.9, 2.4, 2.9, 2.9, 2.4, 2.4), 
            colour = c(col_off_white, col_off_black, col_off_white, col_off_black, col_off_black, col_off_black),
            nudge_y = c(1, 0.35, 0.9, 0.8, 0.4, 0.4),
            ) +
  # Label for centre of 1875 circle
  geom_text(data = df_plate22 %>% filter(year == 1875), 
            aes(x = 0, y = 0, label = scales::dollar(valuation_dollars)), 
            colour = col_off_white, size = 3
            ) +
  # Wedges + wedge labels
  # 1880
  geom_arc_bar(aes(x0 = -3, y0 = -3, r0 = 5.5, r = 0.3, start = 3.8, end = 4.4), 
               fill = col_tan, colour = col_tan
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1880), 
            aes(x = -5.8, y = -5, label = scales::dollar(valuation_dollars), angle = 35), 
            colour = col_off_black, size = 2.4
            ) +
  # 1885
  geom_arc_bar(aes(x0 = 3, y0 = -3, r0 = 8, r = 0.3, start = 2.1, end = 2.7), 
               fill = col_blue, colour = col_blue
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1885), 
            aes(x = 6, y = -6.2, label = scales::dollar(valuation_dollars), angle = -45), 
            colour = col_off_white, size = 2.6
            ) +
  # 1890
  geom_arc_bar(aes(x0 = 5, y0 = 0, r0 = 14, r = 0.7, start = 1.4, end = 1.7), 
               fill = col_gold, colour = col_gold
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1890), 
            aes(x = 14.5, y = 0.1, label = scales::dollar(valuation_dollars), angle = 0), 
            colour = col_off_black, size = 3
            ) +
  # 1895
  geom_arc_bar(aes(x0 = 3, y0 = 3, r0 = 16.75, r = 1, start = 0.55, end = 0.8), 
               fill = col_light_grey, colour = col_light_grey
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1895), 
            aes(x = 11.2, y = 13.5, label = scales::dollar(valuation_dollars), angle = 51), 
            colour = col_off_black, size = 3
            ) +
  # 1899
  geom_arc_bar(aes(x0 = -3, y0 = 3, r0 = 17.5, r = 1, start = 5.5, end = 5.75), 
               fill = col_red, colour = col_red
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1899), 
            aes(x = -11.5, y = 14.1, label = scales::dollar(valuation_dollars), angle = -51), 
            colour = col_off_black, size = 3
            ) +
  # Co-ordinates
  coord_fixed() +
  # Theme
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "Public Sans", colour = col_off_black),
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 40), 
                              hjust = 0.5, lineheight = 1.1),
    plot.caption = element_text(size = 8, margin = margin(t = 40), 
                                hjust = 0.5, lineheight = 1.1),
    plot.background = element_rect(fill = col_parchment, colour = NA),
    plot.margin = margin(t = 20, r = 20, b = 0, l = 20)
    ) +
  # Titles
  labs(
    title = "ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY AFRICAN AMERICANS IN GEORGIA .",
    caption = "VISUALISATION: FIONA LEES | @Fi_Lees\nSOURCE: ANTHONY STARKS\nTIDY TUESDAY: WEEK 7, 2022 | DU BOIS CHALLENGE: NO.2, 2022\n"
    )
  
p_plate22
```

<img src="README_files/figure-html/p_plate22-1.png" title="Nested circle chart showing the assessed valuation of taxable property owned by African Americans in Georgia between 1875 and 1899. The value increased from $5,393,885 in 1875 to $13,447,423 in 1899. This chart is a recreation of W.E.B. Du Bois' Plate 22 for the 1900 Paris Exposition." alt="Nested circle chart showing the assessed valuation of taxable property owned by African Americans in Georgia between 1875 and 1899. The value increased from $5,393,885 in 1875 to $13,447,423 in 1899. This chart is a recreation of W.E.B. Du Bois' Plate 22 for the 1900 Paris Exposition." width="100%" style="display: block; margin: auto;" />

Save the plot as an image.  


```r
# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save plot as image
ggsave("DuBois_Plate22.png", p_plate22, height = 11.5, width = 10, units = "in", dpi = 300)

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
##  [1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.8     purrr_0.3.4    
##  [5] readr_2.1.2     tidyr_1.2.0     tibble_3.1.6    tidyverse_1.3.1
##  [9] ggforce_0.3.3   ggplot2_3.3.5   showtext_0.9-5  showtextdb_3.0 
## [13] sysfonts_0.8.5 
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.4.2        sass_0.4.0        bit64_4.0.5       vroom_1.5.7      
##  [5] jsonlite_1.7.3    modelr_0.1.8      bslib_0.3.1       assertthat_0.2.1 
##  [9] highr_0.9         cellranger_1.1.0  yaml_2.2.2        pillar_1.7.0     
## [13] backports_1.4.1   glue_1.6.1        digest_0.6.29     polyclip_1.10-0  
## [17] rvest_1.0.2       snakecase_0.11.0  colorspace_2.0-2  htmltools_0.5.2  
## [21] pkgconfig_2.0.3   broom_0.7.12      haven_2.4.3       scales_1.1.1     
## [25] tweenr_1.0.2      tzdb_0.2.0        generics_0.1.2    farver_2.1.0     
## [29] ellipsis_0.3.2    withr_2.4.3       janitor_2.1.0     cli_3.2.0        
## [33] magrittr_2.0.2    crayon_1.5.0      readxl_1.3.1      evaluate_0.14    
## [37] fs_1.5.2          fansi_1.0.2       MASS_7.3-54       xml2_1.3.3       
## [41] textshaping_0.3.6 tools_4.1.2       hms_1.1.1         lifecycle_1.0.1  
## [45] munsell_0.5.0     reprex_2.0.1      compiler_4.1.2    jquerylib_0.1.4  
## [49] systemfonts_1.0.4 rlang_1.0.1       grid_4.1.2        rstudioapi_0.13  
## [53] labeling_0.4.2    rmarkdown_2.11    gtable_0.3.0      DBI_1.1.2        
## [57] curl_4.3.2        R6_2.5.1          lubridate_1.8.0   knitr_1.37       
## [61] fastmap_1.1.0     bit_4.0.4         utf8_1.2.2        ragg_1.2.1       
## [65] stringi_1.7.6     parallel_4.1.2    Rcpp_1.0.8        vctrs_0.3.8      
## [69] dbplyr_2.1.1      tidyselect_1.1.1  xfun_0.29
```

