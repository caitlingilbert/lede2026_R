# Load any required packages
library(tidyverse)

# Read in the data
accidents <- readr::read_csv("https://raw.githubusercontent.com/caitlingilbert/themeparkaccidents/main/data/accidents_clean_detailed.csv")

# Take a look at this dataframe
glimpse(accidents)

# What parks are in this data set?
unique(accidents$park)
unique(accidents$park_specific) # just Disney/Universal

# How many accidents have happened in each park?
accidents %>%
  dplyr::count(park, sort = TRUE)

accidents %>%
  group_by(park) %>%
  summarise(n = n())

accidents_bypark <- accidents %>%
  dplyr::count(park, sort = TRUE)

view(accidents_bypark)

# What are some of these accidents? Let's look at a sample
accidents %>%
  dplyr::slice_sample(n = 30) %>%
  dplyr::select(injury)

# How often is the injury related to a fall? Fainting? Nausea?
accident_type_counts <- accidents %>%
  mutate(injury = str_to_lower(injury)) %>%
  mutate(type = case_when(
    str_detect(injury, "fall|fell|trip") ~ "fall",
    str_detect(injury, "faint|syncope|lowered level of|loss of") ~ "fainting",
    str_detect(injury, "nausea|motion sickness") ~ "nausea",
    .default = "other"
  )) %>%
  count(park, type)
view(accident_type_counts)

# What are the Disney World rides that have the most incidents?
accidents_disney <- accidents %>%
  filter(park == "Disney World")

accidents_disney %>% 
  dplyr::count(ride, sort = T)

# What are all the injuries that have happened on Pirates of the Caribbean?
accidents_pirates <- accidents %>% 
  filter(park == "Disney World" & str_detect(ride, "Pirates"))

view(accidents_pirates)

# Have men or women been more likely to be injured at Magic Kingdom?
accidents %>% 
  filter(park_specific == "Magic Kingdom") %>% 
  dplyr::count(gender)

# How many people have died in every Disney or Universal theme park?
accident_death_counts <- accidents %>%
  mutate(injury = str_to_lower(injury)) %>%
  mutate(death = ifelse(
    str_detect(injury, "die|dead|passed away|fatal"), "y", "n")) %>%
  count(park_specific, death) %>% 
  filter(death == "y" & !is.na(park_specific))
view(accident_death_counts)

# How often do Disney and Universal use "pre-existing" in their injury reports?
accidents %>% 
  filter(park %in% c("Disney World", "Universal")) %>% 
  group_by(park, preexisting) %>% 
  summarise(total = n())

# What other questions do you want to ask?
