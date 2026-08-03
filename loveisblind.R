library(tidyverse)
#library(vroom)
library(rvest)

### s10
s10page <- read_html("https://www.netflix.com/tudum/features/love-is-blind-season-10-cast-instagrams")

card_nodes <- s10page %>%
  html_elements(".default-ltr-iqcdef-cache-0.eoienyz7")

cast_df <- tibble(
  name = card_nodes %>% 
    html_element("h3[data-sel='heading']") %>% 
    html_text2() %>% 
    str_squish(),
  age = card_nodes %>% 
    html_elements("div[data-sel='sub-head-2']") %>% 
    .[html_text2(.) == "Age"] %>% 
    html_element(xpath = "following-sibling::text()") %>% 
    html_text() %>% 
    str_squish() %>% 
    as.integer(),
  occupation = card_nodes %>% 
    html_elements("div[data-sel='sub-head-2']") %>% 
    .[html_text2(.) == "Occupation"] %>% 
    html_element(xpath = "following-sibling::text()") %>% 
    html_text() %>% 
    str_squish(),
  social_link = card_nodes %>% 
    html_element("a") %>% 
    html_attr("href")
) %>% 
  mutate(season = "10")





