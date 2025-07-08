### Load packages
library(tidyverse)
library(sf)
library(mapview)

### Vaccines For Children Data South Carolina

# Read in VFC Data
vfc_providers = read_csv(
    "https://github.com/jakebharmon/saphl_vfc/raw/refs/heads/main/data/ActiveVFCProviders.csv"
    ) %>%
  mutate(
    County = case_when(
      County == "MCCORMICK" ~ "McCormick",  # Updates to proper CamelCase
      TRUE ~ County %>%  str_to_title()     # Changes all UPPER CASE to Title
    ),
    address = paste(`Address 1`,            # Combines address data 
                    `Address 2`,            # for geocoding
                    City,
                    `State/Territory`,
                    Zip),
    geometry = NA                           # Sets null column to hold geometry
  )

# Geocode data using Google Maps 

#api = readLines("saphl_google_api.txt") # Read in API from .txt file
#register_google(key = api)              # Register API key
#getOption("ggmap")                      # Check that API Key is read
#locations = vfc_providers$address %>%   # Geocodes addresses using Google API
#  geocode()
#
#write_csv(locations, "vfc_geocode.csv")

vfc_geocode = read_csv("vfc_geocode.csv")

vfc_providers[14] = st_as_sf(       # Fills geom column with sf of long/lat
                                vfc_geocode, 
                                coords = c("lon", "lat"), 
                                crs = "NAD83", 
                                agr = "constant"
                        )

### Prepare census data on child poverty

child_pov_acs =
  tidycensus::get_acs(geography = "zip code tabulation area", #Chooses ZCTA
          summary_var = 'B17020_001',  # Poverty by age table
          variables = c("B17020_003",  # 003 = 0-6, 
                        "B17020_004",  # 004 = 7-11
                        "B17020_005"), # 005 = 12-17
          geometry = TRUE,year=2020)   # 2020 last valid year for ACS data

child_pov = child_pov_acs %>%
  filter(substr(GEOID,1,2) == "29" &         # Filter for South Carolina code
        summary_est > 0) %>%                 # Choose populations more than 0
  select(GEOID, summary_est, estimate) %>%   # Select name, pop, child w/ pov est
  group_by(GEOID) %>%                        # Group by name
  summarize(child_pov = sum(estimate)) %>%   # Sum into single child pov metric
  mutate(centroid = 
           st_centroid(st_geometry(geometry))# Create centroids of shapefiles
         )

haversine = (
  st_distance(
    child_pov$centroid,                 # Calculate distance from each VFC
    vfc_providers$geometry)             # Provider to county centroid
    * 0.000621371) %>%                  # Meters -> Miles
  data.frame() %>% 
  map( ~ as.numeric(.x)) %>%            # Convert class from units to numeric
  map_df( ~ case_when(.x > 10 ~ FALSE,  # Convert to False if distance > 10 miles
                 .x < 10 ~ TRUE)) %>% 
  mutate(
    GEOID = child_pov[[1]],        # Select first column from geom tibble
    rowsum = pmap_dbl(., sum)) %>% # Sum Trues by rows to find number of VFC's
  select(GEOID, rowsum) %>%        # In 10 mile radius of centroids
  inner_join(child_pov, by="GEOID") %>% # Join with child_pov data 
  mutate(child_per_vfc = 
           ifelse(
             is.finite((child_pov / rowsum)), # Preventing inf and NaN
             (child_pov / rowsum),            # Number of impov chlrn by vfc
             0)   
         ) %>% 
  mutate(
    vfc_breaks = cut(rowsum,               # Bin VFC rowsums for graphing
                 breaks=c(-Inf,2,10,30,Inf)),
    ratio_breaks = cut(child_per_vfc,      # Bin child/vfc ratio for graphing
                      breaks=c(-Inf,100,500,1000,Inf)))

#Create function of graph
plot_ratio = function(entity) {
  ggplot(haversine, aes(x = rowsum, y = child_pov)) +        # Create plot
    geom_point(data = filter(haversine, GEOID %in% entity),  # Create red point
               color = "blue", size = 4, shape = 18) + # Of viewed county
    geom_point(data = filter(haversine, !GEOID %in% entity), # Plot other counties
               shape = 22, color = "grey", alpha = 0.8, size = .2) +
    labs(x = "# of VFC Providers in 10 Mile Radius", 
         y = "# of Impoverished Children in ZCTA",
         title = paste("ZCTA:",entity)) +                 # Paste ZCTA as title
    theme_bw() +
    theme(plot.title = element_text(
      face = "bold",
      hjust = 0.5)
      )
}

p = haversine$GEOID %>%           #It is important to have $ rather than [],
                                  #As $ returns a list and [] data frame object
  map( ~ plot_ratio(entity = .x)) # Map plot function across GEOID's
# p[[1]] Verify

### Create interactive map of South Carolina

`South Carolina` = tigris::zctas(state="SC",year=2010) %>% #Call ZCTA shape files
  inner_join(haversine[c(1,6)],      # Select GEOID and child_per_vfc
             by = join_by(
               "ZCTA5CE10"=="GEOID") # Match tigris colname
             ) %>% 
  arrange(ZCTA5CE10) %>%             # Matches purred maps to correct shapefile
  mutate(child_per_vfc = round(child_per_vfc))  %>% 
  rename("Children per VFC" = 12)   #Rename and round for cleanliness

sc_map = mapview::mapview(       # Create Leaflet map
  `South Carolina`,              # Clean name
  zcol="Children per VFC",       # Clean name
  popup=leafpop::popupGraph(     # Insert popup scatterplots
    p,
    width = 300, 
    height = 300)
  )

mapview::mapshot(                # Save leaflet map as HTML file
  sc_map,
  url = paste0(
    getwd(), "/sc_map.html")
  )
