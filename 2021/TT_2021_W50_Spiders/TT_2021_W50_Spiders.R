# Tidy Tuesday - W50 - Spiders -----------------------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2021, Week 50). 
#
# Date: 2021-12-07
#
# Author: Fiona Lees
#
# Data Source: World Spider Catalog (WSC) (https://wsc.nmbe.ch/)
#
# Data Description: Searchable online database covering spider taxonomy.


### Load Packages ------------------------------------------------------------------------------------
library(showtext)
library(rcartocolor)
library(ggraph)
library(igraph)
library(tidyverse)


### Import Data --------------------------------------------------------------------------------------
spiders <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv"
)


### Wrangle Data -------------------------------------------------------------------------------------

# Get 35 most recent spiders in the catalog
df_spiders_recent <- spiders %>% 
  arrange(speciesId) %>% 
  slice_tail(n = 35) %>% 
  select(speciesId, family:subspecies, distribution) %>% 
  group_by(family) %>% 
  mutate(n_in_family = n()) %>% 
  group_by(family, genus) %>% 
  mutate(n_in_genus = n()) %>% 
  ungroup() %>% 
  # Order in descending order of the number of spiders in each family and genus
  arrange(-n_in_family, -n_in_genus, desc(family), desc(genus), desc(species))

# Store the preceding hierarchy in the genus and species columns
df_spiders_recent <- df_spiders_recent %>% 
  mutate(genus = str_c(family, " | ", genus),
         species = str_c(genus, " | ", species))

# Make the edge list
# First, make a connection FROM a root node called 'Spider' TO each distinct spider family
df_family <- df_spiders_recent %>% 
  distinct(family) %>%
  mutate(from = "Spider", .before = family) %>% 
  rename(to = family)

# Next, make a connection FROM each family TO each distinct genus within that family
df_genus <- df_spiders_recent %>% 
  distinct(family, genus) %>% 
  rename(from = family, to = genus)

# Finally, make a connection FROM each genus TO each distinct species within that genus
df_species <- df_spiders_recent %>% 
  distinct(genus, species) %>% 
  rename(from = genus, to = species)

# Now bind all connections together into a single data frame
edge_list <- rbind(df_family, df_genus, df_species)

# Create list of all unique nodes listed in the edge list
node_list <- data.frame(name = unique(c(edge_list$from, edge_list$to))) 

# Separate out family, genus and species again (gives option to use for grouping / labelling later)
node_list <- node_list %>% 
  mutate(family = word(name, 1, sep = fixed(' | ')),
         genus = word(name, 2, sep = fixed(' | ')),
         species = word(name, 3, sep = fixed(' | '))
  )

# Create nested graph object with the igraph library
nested_edge_list <- graph_from_data_frame(edge_list, vertices = node_list)


### Visualise Data -----------------------------------------------------------------------------------

# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Roboto Slab")

# Colours
colours_family <- c(carto_pal(10, "Pastel"))
# Set "spider" root node to the neutral grey at the end of the colour palette.
names(colours_family) <- c(df_family$to, "spider")

# Create visualisation
# Pass nested data to ggraph and set circular to true
p_final <- ggraph(nested_edge_list, layout = 'dendrogram', circular = TRUE) + 
  # Edges
  geom_edge_diagonal2(colour = "grey75") +
  # Nodes (coloured by family)
  geom_node_point(aes(colour = family), size = 2.5) +
  # Labels
  geom_node_text(aes(x = x * 1.05, y = y * 1.05, # Put some space between node and label
                     label = name, 
                     filter = leaf, # Only label leaf nodes
                     angle = -((-node_angle(x, y) + 90) %% 180) + 90, # Adjust angle to aid reading
                     hjust = 'outward'
                     ), 
                 colour = "#FFFFFF", size = 3.88
                 ) +
  # Apply colour palette
  scale_color_manual(values = colours_family) +
  # Make some space for the labels
  xlim(-2.4, 2.4) +
  ylim(-2.3, 2.3) +
  coord_fixed() +
  # Styling
  theme_void() +
  theme(
    text = element_text(family = "Lato", size = 12, colour = "#FFFFFF"),
    plot.title = element_text(family = "Roboto Slab", size = 24, face = "bold", 
                              margin = margin(b = 10), hjust = 0.5),
    plot.subtitle = element_text(size = 14, margin = margin(b = 5), hjust = 0.5, lineheight = 1.1),
    plot.caption = element_text(size = 10, hjust = 0.5, lineheight = 1.1),
    plot.background = element_rect(colour = "grey25", fill = "grey25"),
    plot.margin = margin(rep(10, 10)),
    legend.position = "none"
  ) +
  # Titles
  labs(title = "World's most recently described species of spiders",
       subtitle = "Colour indicates spider family.  Labels show family | genus | species of each spider",
       caption = "Tidy Tuesday: Week 50, 2021\nVisualisation: @Fi_Lees\nData source: World Spider Catalog (2021) and the Natural History Museum of Bern. (Version 22.5, accessed on 7th December 2021)"
  )

# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save plot as an image  
ggsave("recent_spiders.png", p_final, width = 12, height = 12, units = "in", dpi = 300)

# Turn off showtext
showtext_auto(FALSE)

