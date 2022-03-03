## Freedom in the World

**Note added 1st Mar 2022**: Index such as the "Freedom Index" have potential for bias and/or miscalculation. Data such as these are far from perfect and may misrepresent nuanced political situations or oversimplify difficult to record/measure political nuance.  

The [data](https://freedomhouse.org/report/freedom-world) this week comes from [Freedom House](https://freedomhouse.org/). It provides freedom index scores and freedom statuses for 195 countries and 15 territories. Global freedom statuses are calculated on a weighted scale, based on 10 political rights indicators and 15 civil liberties indicators.The combination of the overall score awarded for political rights and the overall score awarded for civil liberties, after being equally weighted, determines a status of Not Free, Partly Free, or Free. More information on the methodology is provided [here](https://freedomhouse.org/reports/freedom-world/freedom-world-research-methodology).  

[Arthur Cheib](https://github.com/ArthurCheib/analytical-politics-project/blob/main/data/tidy-data-fh-un.csv) kindly made the dataset available to Tidy Tuesday. However, after spending a bit of time reading about the data and how it's collected, I decided to pull a fuller dataset directly from the Freedom House website. This was mainly so I could use the overall index scores in my analysis rather than the separate ratings for civil liberties and political rights.  


### Objectives
A few weeks ago (9th Feb) I saw a [piece in the Economist](https://www.economist.com/graphic-detail/2022/02/09/a-new-low-for-global-democracy) about global democracy. It contained a beeswarm chart I really liked, showing a global democracy index for 167 countries. The challenge I set myself this week was to try to produce a similar chart using Freedom House's freedom index.  


### Learning Points
I learned about a couple of new things this week:  

- **ggbeeswarm package:** I learned how to use geom_beeswarm() to produce a beeswarm chart and position_beeswarm() to position text labels beside points within the chart. It took me a wee while and a bit of messing about to position the text labels the way I wanted, but I got there in the end. I did consider using ggplot2::geom_jitter(), ggforce::geom_sina() and ggdist::stat_dots() to create the beeswarm chart, but in the end I found ggbeeswarm::geom_beeswarm() gave me the look I wanted. There's a really good tutorial on [Cedric Scherer's website](https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/) that covers several of these options.  

- **MetBrewer package** I've been eager to try out Blake R Mills amazing MetBrewer colour palettes ever since they became available. I finally had a good reason this week. After considering all the colour blind friendly palettes, I found [Hiroshige](https://github.com/BlakeRMills/MetBrewer#hiroshige) was a good option for this week's visualisation.  


### Load Packages


```r
library(showtext)
library(ggbeeswarm)
library(MetBrewer)
library(tidyverse)
```


### Import Data
I've saved the [MS Excel file on Freedom House's website](https://freedomhouse.org/report/freedom-world) to my local hard drive, so I'm going to load it from there. I'll do a little tidying of column names as I import the data.  


```r
freedom <- readxl::read_xlsx(
  "data/Aggregate_Category_and_Subcategory_Scores_FIW_2003-2021.xlsx",
  range = "FIW06-21!A1:S3340",
  # Explicitly define the data type in each column to ensure it gets pulled in correctly
  col_types = c("text", "text", "text", "numeric", "text", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
  ) %>% 
 janitor::clean_names()

glimpse(freedom)
```

```
## Rows: 3,339
## Columns: 19
## $ country_territory <chr> "Abkhazia", "Afghanistan", "Albania", "Algeria", "An~
## $ region            <chr> "Eurasia", "Asia", "Europe", "MENA", "Europe", "SSA"~
## $ c_t               <chr> "t", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c~
## $ edition           <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021~
## $ status            <chr> "PF", "NF", "PF", "NF", "F", "NF", "F", "F", "PF", "~
## $ pr_rating         <dbl> 5, 5, 3, 6, 1, 6, 2, 2, 4, 1, 1, 7, 1, 7, 5, 1, 7, 1~
## $ cl_rating         <dbl> 5, 6, 3, 5, 1, 5, 2, 2, 4, 1, 1, 6, 1, 6, 5, 1, 6, 1~
## $ a                 <dbl> 5, 3, 8, 3, 12, 3, 12, 11, 6, 12, 12, 0, 12, 2, 4, 1~
## $ b                 <dbl> 8, 7, 12, 4, 15, 5, 13, 16, 10, 16, 15, 2, 16, 0, 7,~
## $ c                 <dbl> 4, 3, 7, 3, 11, 2, 8, 8, 6, 12, 10, 0, 10, 2, 4, 10,~
## $ add_q             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0~
## $ add_a             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
## $ pr                <dbl> 17, 13, 27, 10, 38, 10, 33, 35, 22, 40, 37, 2, 38, 2~
## $ d                 <dbl> 8, 6, 13, 6, 14, 7, 15, 15, 9, 15, 14, 2, 15, 2, 6, ~
## $ e                 <dbl> 6, 4, 8, 3, 11, 6, 10, 11, 8, 12, 12, 1, 12, 1, 5, 1~
## $ f                 <dbl> 4, 2, 9, 6, 15, 5, 14, 10, 6, 15, 15, 0, 13, 1, 4, 1~
## $ g                 <dbl> 5, 2, 9, 7, 15, 3, 13, 13, 10, 15, 15, 5, 13, 6, 9, ~
## $ cl                <dbl> 23, 14, 39, 22, 55, 21, 52, 49, 33, 57, 56, 8, 53, 1~
## $ total             <dbl> 40, 27, 66, 32, 93, 31, 85, 84, 55, 97, 93, 10, 91, ~
```

Let's have a quick look at the `freedom` data. The freedom index score I'm interested in is stored in the variable called `total`.      


```r
view(freedom)
Hmisc::describe(freedom)
```

```
## freedom 
## 
##  19  Variables      3339  Observations
## --------------------------------------------------------------------------------
## country_territory 
##        n  missing distinct 
##     3339        0      218 
## 
## lowest : Abkhazia       Afghanistan    Albania        Algeria        Andorra       
## highest: West Bank      Western Sahara Yemen          Zambia         Zimbabwe      
## --------------------------------------------------------------------------------
## region 
##        n  missing distinct 
##     3339        0        6 
## 
## lowest : Americas Asia     Eurasia  Europe   MENA    
## highest: Asia     Eurasia  Europe   MENA     SSA     
##                                                                 
## Value      Americas     Asia  Eurasia   Europe     MENA      SSA
## Frequency       571      688      268      685      336      791
## Proportion    0.171    0.206    0.080    0.205    0.101    0.237
## --------------------------------------------------------------------------------
## c_t 
##        n  missing distinct 
##     3339        0        2 
##                       
## Value          c     t
## Frequency   3109   230
## Proportion 0.931 0.069
## --------------------------------------------------------------------------------
## edition 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       16    0.996     2014     5.31     2006     2007 
##      .25      .50      .75      .90      .95 
##     2010     2014     2018     2020     2021 
## 
## lowest : 2006 2007 2008 2009 2010, highest: 2017 2018 2019 2020 2021
##                                                                             
## Value       2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016
## Frequency    206   207   207   209   208   208   209   209   209   210   210
## Proportion 0.062 0.062 0.062 0.063 0.062 0.062 0.063 0.063 0.063 0.063 0.063
##                                         
## Value       2017  2018  2019  2020  2021
## Frequency    209   209   209   210   210
## Proportion 0.063 0.063 0.063 0.063 0.063
## --------------------------------------------------------------------------------
## status 
##        n  missing distinct 
##     3339        0        3 
##                             
## Value          F    NF    PF
## Frequency   1427   890  1022
## Proportion 0.427 0.267 0.306
## --------------------------------------------------------------------------------
## pr_rating 
##        n  missing distinct     Info     Mean      Gmd 
##     3339        0        7    0.968    3.524    2.469 
## 
## lowest : 1 2 3 4 5, highest: 3 4 5 6 7
##                                                     
## Value          1     2     3     4     5     6     7
## Frequency    921   473   430   306   317   443   449
## Proportion 0.276 0.142 0.129 0.092 0.095 0.133 0.134
## --------------------------------------------------------------------------------
## cl_rating 
##        n  missing distinct     Info     Mean      Gmd 
##     3339        0        7    0.971     3.38    2.158 
## 
## lowest : 1 2 3 4 5, highest: 3 4 5 6 7
##                                                     
## Value          1     2     3     4     5     6     7
## Frequency    796   520   472   452   550   368   181
## Proportion 0.238 0.156 0.141 0.135 0.165 0.110 0.054
## --------------------------------------------------------------------------------
## a 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       13    0.972    7.413    4.894        0        0 
##      .25      .50      .75      .90      .95 
##        3        9       12       12       12 
## 
## lowest :  0  1  2  3  4, highest:  8  9 10 11 12
##                                                                             
## Value          0     1     2     3     4     5     6     7     8     9    10
## Frequency    367   119   211   211   123   114   185   117   143   285   185
## Proportion 0.110 0.036 0.063 0.063 0.037 0.034 0.055 0.035 0.043 0.085 0.055
##                       
## Value         11    12
## Frequency    319   960
## Proportion 0.096 0.288
## --------------------------------------------------------------------------------
## b 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       17    0.992    9.698    6.026        0        2 
##      .25      .50      .75      .90      .95 
##        5       11       15       16       16 
## 
## lowest :  0  1  2  3  4, highest: 12 13 14 15 16
##                                                                             
## Value          0     1     2     3     4     5     6     7     8     9    10
## Frequency    195   136   138   167   137   128   108   155   140   121   227
## Proportion 0.058 0.041 0.041 0.050 0.041 0.038 0.032 0.046 0.042 0.036 0.068
##                                               
## Value         11    12    13    14    15    16
## Frequency    183   133   185   215   493   478
## Proportion 0.055 0.040 0.055 0.064 0.148 0.143
## --------------------------------------------------------------------------------
## c 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       13    0.993      6.2    4.295        0        1 
##      .25      .50      .75      .90      .95 
##        3        6       10       11       12 
## 
## lowest :  0  1  2  3  4, highest:  8  9 10 11 12
##                                                                             
## Value          0     1     2     3     4     5     6     7     8     9    10
## Frequency    220   222   231   338   258   199   257   282   210   239   380
## Proportion 0.066 0.066 0.069 0.101 0.077 0.060 0.077 0.084 0.063 0.072 0.114
##                       
## Value         11    12
## Frequency    205   298
## Proportion 0.061 0.089
## --------------------------------------------------------------------------------
## add_q 
##        n  missing distinct     Info     Mean      Gmd 
##     3339        0        5    0.193   0.1515   0.2874 
## 
## lowest : 0 1 2 3 4, highest: 0 1 2 3 4
##                                         
## Value          0     1     2     3     4
## Frequency   3109    69    67    73    21
## Proportion 0.931 0.021 0.020 0.022 0.006
## --------------------------------------------------------------------------------
## add_a 
##        n  missing distinct     Info     Mean      Gmd 
##     2501      838        5    0.081  0.06557   0.1284 
## 
## lowest : 0 1 2 3 4, highest: 0 1 2 3 4
##                                         
## Value          0     1     2     3     4
## Frequency   2432    17    17    27     8
## Proportion 0.972 0.007 0.007 0.011 0.003
## --------------------------------------------------------------------------------
## pr 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       44    0.999    23.21    15.13        1        3 
##      .25      .50      .75      .90      .95 
##       11       26       36       39       40 
## 
## lowest : -3 -2 -1  0  1, highest: 36 37 38 39 40
## --------------------------------------------------------------------------------
## d 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       17    0.989    10.82    5.124      2.9      4.0 
##      .25      .50      .75      .90      .95 
##      7.0     12.0     15.0     16.0     16.0 
## 
## lowest :  0  1  2  3  4, highest: 12 13 14 15 16
##                                                                             
## Value          0     1     2     3     4     5     6     7     8     9    10
## Frequency     46    36    85   110   147   107   174   175   201   171   170
## Proportion 0.014 0.011 0.025 0.033 0.044 0.032 0.052 0.052 0.060 0.051 0.051
##                                               
## Value         11    12    13    14    15    16
## Frequency    184   200   149   310   522   552
## Proportion 0.055 0.060 0.045 0.093 0.156 0.165
## --------------------------------------------------------------------------------
## e 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       13    0.986    7.445     4.42        0        2 
##      .25      .50      .75      .90      .95 
##        4        8       11       12       12 
## 
## lowest :  0  1  2  3  4, highest:  8  9 10 11 12
##                                                                             
## Value          0     1     2     3     4     5     6     7     8     9    10
## Frequency    195   111   147   247   210   180   266   195   266   183   199
## Proportion 0.058 0.033 0.044 0.074 0.063 0.054 0.080 0.058 0.080 0.055 0.060
##                       
## Value         11    12
## Frequency    432   708
## Proportion 0.129 0.212
## --------------------------------------------------------------------------------
## f 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       17    0.995    8.056    5.644        0        1 
##      .25      .50      .75      .90      .95 
##        4        8       13       15       15 
## 
## lowest :  0  1  2  3  4, highest: 12 13 14 15 16
##                                                                             
## Value          0     1     2     3     4     5     6     7     8     9    10
## Frequency    219   138   180   121   309   189   286   225   201   145   134
## Proportion 0.066 0.041 0.054 0.036 0.093 0.057 0.086 0.067 0.060 0.043 0.040
##                                               
## Value         11    12    13    14    15    16
## Frequency    137   212   156   177   405   105
## Proportion 0.041 0.063 0.047 0.053 0.121 0.031
## --------------------------------------------------------------------------------
## g 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       17    0.995    9.382    4.717        3        4 
##      .25      .50      .75      .90      .95 
##        6        9       13       15       15 
## 
## lowest :  0  1  2  3  4, highest: 12 13 14 15 16
##                                                                             
## Value          0     1     2     3     4     5     6     7     8     9    10
## Frequency     34    40    65   122   179   255   254   246   243   254   302
## Proportion 0.010 0.012 0.019 0.037 0.054 0.076 0.076 0.074 0.073 0.076 0.090
##                                               
## Value         11    12    13    14    15    16
## Frequency    148   191   303   237   330   136
## Proportion 0.044 0.057 0.091 0.071 0.099 0.041
## --------------------------------------------------------------------------------
## cl 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0       61    0.999     35.7    19.38        7       12 
##      .25      .50      .75      .90      .95 
##       22       36       52       57       58 
## 
## lowest :  0  1  2  3  4, highest: 56 57 58 59 60
## --------------------------------------------------------------------------------
## total 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     3339        0      102        1    58.91    34.21        8       16 
##      .25      .50      .75      .90      .95 
##       32       62       88       96       98 
## 
## lowest :  -1   0   1   2   3, highest:  96  97  98  99 100
## --------------------------------------------------------------------------------
```


### Explore / Wrangle Data

I'm only going to use the most recent country level data, so I'll filter out the earlier years and data for territories. While I'm at it, I'll drop the variables I'm not interested in at the moment.  


```r
# Drop data not required
countries_2020 <- freedom %>% 
  filter(c_t == "c" & edition == 2021) %>% 
  select(country_territory:cl_rating, pr, cl, total) %>% 
  rename(country = country_territory)
```

Next, I'd like to get a sense of the range of scores in each region of the world.  


```r
# How many countries in each region? 
# What are the range of total scores in each region?
countries_2020 %>% 
  group_by(region) %>% 
  summarise(n_countries = n(),
            min_score = min(total),
            max_score = max(total),
            median_score = median(total),
            mean_score = mean(total),
            stdev_score = sd(total)

  ) %>% 
  ungroup() %>% 
  arrange(mean_score)
```

```
## # A tibble: 6 x 7
##   region   n_countries min_score max_score median_score mean_score stdev_score
##   <chr>          <int>     <dbl>     <dbl>        <dbl>      <dbl>       <dbl>
## 1 MENA              18         1        76         24         27.7        20.4
## 2 Eurasia           12         2        61         21.5       29.1        23.2
## 3 SSA               49         2        92         43         42.4        24.9
## 4 Asia              39         3        99         61         60.5        28.4
## 5 Americas          35        13        98         80         72.7        22.5
## 6 Europe            42        32       100         90         85.6        15.1
```

Okay, now I'm going to change `status` and `region` into factors and give each factor level a meaningful (formatted) label. This will make these variables much easier to work with when I create my chart.  


```r
# Make status and region factors
countries_2020 <- countries_2020 %>% 
  mutate(year = edition - 1, .after = edition,
         status = factor(status, levels = c("NF", "PF", "F"), 
                         labels = c("Not Free", "Partly Free", "Free")),
         region_2 = factor(region, levels = c("MENA", "Eurasia", "SSA", "Asia", "Americas", "Europe"), 
                         labels = c("Middle East &\nNorth Africa", "Eurasia", 
                                    "Sub-Saharan\nAfrica", "Asia &\nAustralasia", 
                                    "North America,\nLatin America\n& the Caribbean", "Europe")),
         )
```

Finally, I want to separate out the top and bottom ranked countries in each region, so that I can label them in my chart.  


```r
# Highest score(s) in each region
countries_2020_high <- countries_2020 %>% 
  group_by(region) %>% 
  slice(which(total == max(total))) %>% 
  ungroup() %>% 
  arrange(region_2, total)
  
# Lowest score(s) in each region
countries_2020_low <- countries_2020 %>% 
  group_by(region) %>% 
  slice(which(total == min(total))) %>% 
  ungroup() %>% 
  arrange(region_2, total)

# Create a flag for the highest and lowest countries in the `countries_2020` dataset
countries_2020 <- countries_2020 %>%
  mutate(hi_low_country = ifelse(country %in% countries_2020_high$country | country %in% countries_2020_low$country, TRUE, FALSE))
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

Now I'll create the chart.    


```r
p_final <- countries_2020 %>% 
  ggplot(aes(x = total, y = region_2, fill = status, colour = hi_low_country)) +
  # Beeswarm
  geom_beeswarm(
    shape = 21, 
    cex = 1.8, 
    size = 3.8, 
    groupOnX = FALSE
    ) +
  # Dot fill colour and borders
  scale_fill_manual(values = met.brewer("Hiroshige", 3)) +
  scale_colour_manual(values = c("darkgrey", "black"), guide = "none") +
  # Text labels for lowest scores
  geom_label(
    data = countries_2020_low, 
    aes(label = country), 
    colour = "grey40",
    fill = "white",
    size = 3.88, 
    # Use position_beeswarm() to match labels to points
    position = position_beeswarm(groupOnX = FALSE, cex = 1.8),
    # Adjust label positions for individual points
    vjust = c(-0.8, -0.8, 1.5, -0.8, -0.8, 1.5, -0.8), 
    hjust = 1, 
    label.padding = unit(0.02, "cm"),
    label.size = NA
    ) +
  # Text labels for highest scores
  geom_label(
    data = countries_2020_high, 
    aes(label = country), 
    colour = "grey40", 
    fill = "white",
    size = 3.88, 
    # Use position_beeswarm() to match labels to points
    position = position_beeswarm(groupOnX = FALSE, cex = 1.8),
    # Adjust label positions for individual points
    hjust = c(0, 0, 0, 0, 0, 0, 0, -0.25, 0),
    vjust = c(1.8, 2.6, 1.6, 1.6, 1.6, -0.8, -1.9, 1.5, 1.6),
    label.padding = unit(0.02, "cm"),
    label.size = NA
    ) +
  # Horizontal lines between each region  
  geom_hline(yintercept = seq(0.5, 6.5, 1), colour = "grey90") +
  # Scales
  scale_x_continuous(limits = c(-5, 105), breaks = seq(0, 100, 10), position = "top") +
  scale_y_discrete(expand = expansion(add = 0.5)) +
  # Styling
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", colour = "grey40", size = 12),
    plot.title = element_text(family = "Roboto Slab", colour = "black", size = 16, 
                              face = "bold", margin = margin(b = 5), hjust = 0),
    plot.subtitle = element_text(size = 12, margin = margin(b = 0), hjust = 0),
    plot.caption = element_text(size = 10, margin = margin(t = 10), hjust = 0),
    plot.background = element_rect(colour = "white", fill = "white"),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = 5)),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = -15, r = 0, b = -15, l = 0),
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    ) +
  # Titles
  labs(
    title = "Global Freedom Index, 2020",
    subtitle = "Freedom index / status of 195 countries, 100 = most free",
    x = "", 
    y = "",
    colour = "",
    fill = "",
    caption = "Note: These type of index have potential for bias / miscalculation. They are far from perfect and may misrepresent / oversimplify nuanced political situations.\n \nGlobal freedom statuses are calculated on a weighted scale, based on 10 political rights indicators and 15 civil liberties indicators.\nThe combination of the overall score awarded for political rights and the overall score awarded for civil liberties, after being equally\nweighted, determines the status of Not Free, Partly Free, or Free. Data based on status Jan 1 - Dec 31 2020.\n \nVisualisation: Fiona Lees (@Fi_Lees) | Source: Freedom House | Tidy Tuesday: Week 8, 2022"
    )

p_final
```

<img src="README_files/figure-html/p_final-1.png" title="Beeswarm chart showing global freedom index scores for 195 countries in six world regions (range 0 - 100, 100 being the most free). The freedom status of each country (not free, partly free, free) is also shown. Overall, scores are highest in Europe (Finland, Norway and Sweden = 100), whereas scores in the Middle East and North Africa are the lowest (Syria = 1)." alt="Beeswarm chart showing global freedom index scores for 195 countries in six world regions (range 0 - 100, 100 being the most free). The freedom status of each country (not free, partly free, free) is also shown. Overall, scores are highest in Europe (Finland, Norway and Sweden = 100), whereas scores in the Middle East and North Africa are the lowest (Syria = 1)." width="100%" style="display: block; margin: auto;" />

Save the plot as an image.  


```r
# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save plot as image
ggsave("freedom_2020.png", p_final, width = 12 , height = 9.5, units = "in", dpi = 300)

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
##  [1] forcats_0.5.1    stringr_1.4.0    dplyr_1.0.8      purrr_0.3.4     
##  [5] readr_2.1.2      tidyr_1.2.0      tibble_3.1.6     tidyverse_1.3.1 
##  [9] MetBrewer_0.1.0  ggbeeswarm_0.6.0 ggplot2_3.3.5    showtext_0.9-5  
## [13] showtextdb_3.0   sysfonts_0.8.5  
## 
## loaded via a namespace (and not attached):
##  [1] fs_1.5.2            lubridate_1.8.0     RColorBrewer_1.1-2 
##  [4] httr_1.4.2          tools_4.1.2         backports_1.4.1    
##  [7] bslib_0.3.1         utf8_1.2.2          R6_2.5.1           
## [10] rpart_4.1-15        vipor_0.4.5         Hmisc_4.6-0        
## [13] DBI_1.1.2           colorspace_2.0-3    nnet_7.3-16        
## [16] withr_2.4.3         tidyselect_1.1.2    gridExtra_2.3      
## [19] rematch_1.0.1       curl_4.3.2          compiler_4.1.2     
## [22] textshaping_0.3.6   cli_3.2.0           rvest_1.0.2        
## [25] htmlTable_2.4.0     xml2_1.3.3          sass_0.4.0         
## [28] checkmate_2.0.0     scales_1.1.1        systemfonts_1.0.4  
## [31] digest_0.6.29       foreign_0.8-81      rmarkdown_2.11     
## [34] base64enc_0.1-3     jpeg_0.1-9          pkgconfig_2.0.3    
## [37] htmltools_0.5.2     highr_0.9           dbplyr_2.1.1       
## [40] fastmap_1.1.0       htmlwidgets_1.5.4   rlang_1.0.1        
## [43] readxl_1.3.1        rstudioapi_0.13     farver_2.1.0       
## [46] jquerylib_0.1.4     generics_0.1.2      jsonlite_1.8.0     
## [49] magrittr_2.0.2      Formula_1.2-4       Matrix_1.4-0       
## [52] Rcpp_1.0.8          munsell_0.5.0       fansi_1.0.2        
## [55] lifecycle_1.0.1     stringi_1.7.6       yaml_2.3.5         
## [58] snakecase_0.11.0    grid_4.1.2          crayon_1.5.0       
## [61] lattice_0.20-45     haven_2.4.3         splines_4.1.2      
## [64] hms_1.1.1           knitr_1.37          pillar_1.7.0       
## [67] reprex_2.0.1        glue_1.6.2          evaluate_0.15      
## [70] latticeExtra_0.6-29 data.table_1.14.2   modelr_0.1.8       
## [73] vctrs_0.3.8         png_0.1-7           tzdb_0.2.0         
## [76] cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1   
## [79] xfun_0.29           janitor_2.1.0       broom_0.7.12       
## [82] ragg_1.2.2          survival_3.2-13     beeswarm_0.4.0     
## [85] cluster_2.1.2       ellipsis_0.3.2
```
