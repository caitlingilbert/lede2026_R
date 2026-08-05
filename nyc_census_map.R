library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

# get census key at: http://api.census.gov/data/key_signup.html

# you can also add this key to your Renviron (overwrite = T if you want to replace existing one)
census_api_key("YOUR API KEY GOES HERE", overwrite = FALSE, install = TRUE)
# For the first time, reload your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check that your key is stored by running:
Sys.getenv("CENSUS_API_KEY")

# If you ever want to look at your .Renviron use this
usethis::edit_r_environ()

# take a look at all variables for 2024
var2024 <- load_variables(2024, "acs5", cache = TRUE)

# look at one variable and map it by county
queens <- get_acs(
  state = "NY",
  county = "Queens",
  geography = "tract",
  variables = "B19013_001", #median household income
  geometry = TRUE,
  year = 2024
)

# take a look at the top of the dataframe
head(queens)

# quickly map this data
queens %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(color = "NA") +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal()

# look at multiple variables from decennial data
racevars <- c(White = "P2_005N",
              Black = "P2_006N",
              Asian = "P2_008N",
              Hispanic = "P2_002N")

queens_race <- get_decennial(
  geography = "tract",
  variables = racevars,
  state = "NY",
  county = "Queens",
  geometry = TRUE,
  summary_var = "P2_001N", # multi-group denominator
  year = 2020,
  sumfile = "pl" # this is the default for 2020
)

head(queens_race)

# facet-map these multiple variables at once
queens_race %>%
  mutate(percent = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = percent)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "% of population\n(2020 Census)")

# deal with shorelines...
plot <- queens_race %>%
  erase_water(year = 2020) %>%
  mutate(percent = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = percent)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "% of population\n(2020 Census)")

plot

# save the plot you just made using code (or use the "Export" button in the Plot viewer)
ggsave("data/figures/plot.png", plot = plot)

# look at an interactive map
library(mapgl)
tidycensus::get_acs(
  geography = "place",
  variables = c("B19025_001", "B19001_001"), #aggregate household income over past year, total number of households in given geography
  state = "NY",
  geometry = TRUE,
  output = "wide"
) %>%
  dplyr::mutate(mean_income = round(B19025_001E / B19001_001E)) %>%
  mapgl::maplibre_view(column = "mean_income")

# look at all states in a map
us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2024,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry() # rescales AK, HI, PR in US-wide map

# let's just see what geographic shapes we're mapping
plot(us_median_age$geometry)

# add in the data layer with colors!
us_median_age %>%
ggplot(aes(fill = estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "RdPu",
                       direction = 1) +
  labs(title = "Median Age by State, 2024",
       caption = "Data source: 2024 1-year ACS, US Census Bureau",
       fill = "ACS estimate") +
  theme_void()
