# load package
# install.packages(c("ggplot2", "ggspatial", "dplyr", "maps", "mapproj"))

library(ggplot2)
library(dplyr)
library(maps)
library(stringr)
library(ggspatial) # for map scale

# input data
Energy_Insecurity <- read.csv( "~/Desktop/study/RECS/Data/RECS/RECS_Energy_Insecurity.csv")

# data of map - the state name's initial letter is lowercase
us_states <- map_data("state") %>% 
  mutate(region = str_to_title(region)) # capitalize

# categorize EI level into 5 tertiles
Energy_Insecurity_tertile <- Energy_Insecurity %>%
  mutate(
    PO_proportion_quintile = ntile(PO_proportion, 5)
  )


# capitalization
Energy_Insecurity_standardized <- Energy_Insecurity_tertile %>%
  mutate(
    # ensure the format
    state = str_to_title(state)
  ) 

# merge state and EI
us_states_energy <- us_states %>%
  left_join(Energy_Insecurity_standardized, by = c("region" = "state"))

# use ggplot to draw the map
ggplot(data = us_states_energy) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = PO_proportion_quintile),
               color = "white", size = 0.2) +
  
  # set color
  scale_fill_gradient2(
    low = "green", 
    high = "red", 
    mid = "white",
    midpoint = median(us_states_energy$PO_proportion_quintile, na.rm = TRUE),
    name = "Energy Insecurity"
  ) +
  
  # coordinate and scale
  coord_fixed(1.3) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true",
    pad_x = unit(0.75, "in"), 
    pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # labels
  theme_void() +
  labs(
    title = "Energy Insecurity by State",
    caption = "Data Source: Your Data Source Here"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )



