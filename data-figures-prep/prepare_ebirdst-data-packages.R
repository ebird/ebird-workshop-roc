library(tidyverse)
library(ebirdst)
library(fs)

ebirst_dir <- "data/ebirdst-data"
dir_create(ebirst_dir)

cl_stats <- read_csv("data/ebirdst-2021_regional-stats_chile.csv") %>%
  filter(region_code == "CL") %>%
  filter(season_code %in% c("breeding", "year_round")) %>%
  select(species_code, percent_population) %>%
  arrange(desc(percent_population))
ducks <- read_csv("data/ebirdst-2021_species-list_chile.csv") %>%
  filter(family == "Anatidae") %>%
  inner_join(cl_stats) %>%
  select(species_code, common_name, percent_population) %>%
  arrange(desc(percent_population))

species <- read_csv("data/ebirdst-2021_species-list_chile.csv") %>%
  inner_join(head(cl_stats, 10)) %>%
  select(species_code, common_name, percent_population) %>%
  arrange(desc(percent_population)) %>%
  pull(species_code)

walk(species, ebirdst_download, path = ebirst_dir,
     pattern = "(percent-population|abundance)_.*_mr",
     force = TRUE)


read_csv("data/ebirdst-2021_species-list_chile.csv") %>%
  inner_join(head(cl_stats, 10)) %>%
  select(common_name, percent_population, is_resident) %>%
  arrange(desc(percent_population)) %>%
  mutate(percent_population = scales::percent(percent_population %>% round(2))) %>%
  format_csv() %>%
  cat()
