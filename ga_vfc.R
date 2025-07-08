### Load packages
library(tidyverse)
library(sf)
library(mapview)

### Vaccines For Children Data South Carolina

# Read in VFC Data
vfc_providers = readxl::read_excel(
  "georgia_vfc.xlsx") %>%
  mutate(
    COUNTY = COUNTY %>%  str_to_title() # Changes all UPPER CASE to Title
  )

child_pov_acs =
  tidycensus::get_acs(geography = "county", #Chooses ZCTA
                      state = "GA",
                      summary_var = 'B17020_001',  # Poverty by age table
                      variables = c("B17020_003",  # 003 = 0-6, 
                                    "B17020_004",  # 004 = 7-11
                                    "B17020_005"), # 005 = 12-17
                      geometry = TRUE,
                      year=2020)   # 2020 last valid year for ACS data

map_df = child_pov_acs %>%
  mutate(
    NAME = NAME %>%  str_to_title() # Changes all UPPER CASE to Title
  ) %>% 
  filter(summary_est > 0) %>%                 # Choose populations more than 0
  select(NAME, summary_est, estimate) %>%   # Select name, pop, child w/ pov est
  group_by(NAME) %>%                        # Group by name
  summarize(child_pov = sum(estimate)) %>%   # Sum into single child pov metric
  mutate(NAME = gsub(' County, Georgia', 
                     '' , 
                     NAME),
         ) %>% 
  full_join(vfc_providers, 
             by = join_by("NAME" == "COUNTY")
             ) %>% 
  rename("county" = 1, 
         "vfc_count" = 4) %>% 
  mutate(vfc_per_county = (child_pov / vfc_count) %>% 
           round()) %>% 
  arrange(county) # Match tigris colname

#Create function of graph
plot_ratio = function(entity) {
  ggplot(map_df, aes(x = vfc_count, y = child_pov)) +        # Create plot
    geom_point(data = filter(map_df, county %in% entity),  # Create red point
               color = "blue", size = 4, shape = 18) + # Of viewed county
    geom_point(data = filter(map_df, !county %in% entity), # Plot other counties
               shape = 22, color = "grey", alpha = 0.8, size = .2) +
    labs(x = "# of VFC Providers in County", 
         y = "# of Impoverished Children in County",
         title = paste("County:",entity)) +                 # Paste ZCTA as title
    theme_bw() +
    theme(plot.title = element_text(
      face = "bold",
      hjust = 0.5)
    )
}

p = map_df$county %>%           #It is important to have $ rather than [],
  #As $ returns a list and [] data frame object
  map( ~ plot_ratio(entity = .x)) # Map plot function across GEOID's
# p[[67]] Verify

### Create interactive map of South Carolina
ga_map = 
  mapview::mapview(       # Create Leaflet map
    map_df,              # Clean name
    zcol="vfc_per_county",       # Clean name
    popup=leafpop::popupGraph(     # Insert popup scatterplots
      p,
      width = 300, 
      height = 300)
  ) 

mapview::mapshot(                # Save leaflet map as HTML file
  ga_map,
  url = paste0(
    getwd(), "/ga_map.html")
)
