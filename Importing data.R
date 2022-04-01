## final project Import data
#libraries
library(sf)
library(spdep) # for calculating neighbors of polygons
library(RANN) # for fast nearest neighbor searches
library(pgirmess) # For spatial autocorrelation models. 
library(spatialreg) # for areal regression models. 
library(RColorBrewer) # For nice color pallettes. 
library(ggplot2)
library(tidycensus)
library(tidyverse)
options(tigris_use_cache = TRUE)
library(censusapi)

#
#
# Setting up data
#
#
utah <- st_as_sf(maps::map("state", "ut", plot = F, fill = T))

#acquire trail data
trails <- sf::st_read(dsn = "data-raw", 
                  layer = "TrailsAndPathways")

#trails is an sf object, type linestring, crs: wgs84

Sys.setenv(CENSUS_KEY = "deb07d6137eaa865ee62398a191876d65dfa8fdf")
census_api_key("deb07d6137eaa865ee62398a191876d65dfa8fdf", install = TRUE)
utah_census <- get_acs(state = "UT", geography = "tract", 
                       variables = "B19013_001", geometry = TRUE,
                       year = 2020)
class(utah_census)
#utah_census is an sf object, type multipolygon, crs: NAD83

#subset trails data :
trails <- filter(trails, CartoCode == c("1 - Hiking Only", 
                                        "2 - Hiking and Biking Allowed",
                                        "3 - Paved Shared Use", 
                                        "5 - Biking Only"))
#now we should have about 5000 observations

#
#
# Initial Plot
#
#

ggplot() +
  geom_sf(data = trails, col = "red") +
  geom_sf(data = utah, fill = NA, col = "black", lwd = .5) +
  labs(title = "Map of Utah Trails")


#
#
# Separating chosen counties into tracts and plotting
#
#

ut_income_tract <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  state = "UT",
  year = 2019,
  geometry = TRUE
)

ut_income_county <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "UT",
  year = 2019,
  geometry = TRUE
)

#inital plot of income by county
ggplot(ut_income_county, aes(x = estimate,
                             y = reorder(NAME, estimate))) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Counties in Utah",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar)

#trying to plot income of county on map of utah
ggplot() +
  geom_sf(data = ut_income_county, aes(fill = estimate)) +
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1) +
  geom_sf(data = utah, fill = NA, col = "black", lwd = .5) +
  labs(title = "Map of Utah money") +
  geom_sf(data = trails, col = "blue") 

#filtering tracts by counties
ut_income_tract <- ut_income_tract %>% 
  mutate(county_code = GEOID) %>% 
  separate(county_code, into = c("county_code", "trash"), 
           sep = 5) %>%
  select(! trash)
#
#cache county
#
cache_tract <- ut_income_tract %>%
  filter(county_code == "49005")
#median household income of Cache Valley
ggplot(cache_tract, aes(x = estimate,
                        y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Cache County Tracts",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar) 
  

# attempting to plot the census districts of cache county 
cache_valley <- st_as_sf(maps::map("county", "utah,cache", plot = F, fill = T))
ggplot() +
  geom_sf(data = cache_tract, aes(fill = estimate)) +
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1) +
  geom_sf(data = cache_valley, fill = NA, col = "black", lwd = .5) +
  labs(title = "Map of cache valley money") +
  geom_sf(data = trails, col = "blue") 

 

#
#summit county
#
summit_tract <- ut_income_tract %>%
  filter(county_code == "49043")

ggplot(summit_tract, aes(x = estimate,
                         y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Summit County Tracts",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar)
#
#salt lake county
#
slc_tract <- ut_income_tract %>%
  filter(county_code == "49035")

ggplot(slc_tract, aes(x = estimate,
                      y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Salt Lake County Tracts",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar)

#
#utah county
#
ucounty_tract <- ut_income_tract %>%
  filter(county_code == "49049")

ggplot(ucounty_tract, aes(x = estimate,
                          y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Utah County Tracts",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar)

#
#davis
#
davis_tract <- ut_income_tract %>%
  filter(county_code == "49011")

ggplot(davis_tract, aes(x = estimate,
                        y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Davis County Tracts",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar)
#
#weber
#
weber_tract <- ut_income_tract %>%
  filter(county_code == "49057")

ggplot(weber_tract, aes(x = estimate,
                        y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Weber County Tracts",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar)
#
#washington
#
washington_tract <- ut_income_tract %>%
  filter(county_code == "49053")
ggplot(washington_tract, aes(x = estimate,
                             y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Washington County Tracts",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar)
#
#uintah
#
uintah_tract <- ut_income_tract %>% 
  filter(county_code == "49047")

ggplot(uintah_tract, aes(x = estimate,
                         y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Uintah County Tracts",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = scales::dollar)
