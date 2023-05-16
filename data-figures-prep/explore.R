library(arrow)
library(auk)
library(ebirdst)
library(glue)
library(rnaturalearth)
library(sf)
library(units)
library(tidyverse)

cl_states <- ne_states(iso_a2 = "CL", returnclass = "sf") %>%
  select(state_name = name, state_code = iso_3166_2)

# connect checklists to states
checklists <- read_parquet("data/ebird_checklists_chile_2021.parquet")
locs <- checklists %>%
  distinct(checklist_id, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_join(cl_states, left = FALSE) %>%
  st_drop_geometry()

# apply to observations
observations <- read_parquet("data/ebird_observations_chile_2021.parquet") %>%
  inner_join(locs, by = "checklist_id")
state_counts <- observations %>%
  group_by(species_code, state_name, state_code) %>%
  summarize(n_detections = sum(obs_count > 0, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(n_detections))
filter(state_counts,
       species_code == "chutap1",
       state_code %in% c("CL-AR", "CL-LR", "CL-LL"))
grid_sampled <- filter(observations,
       species_code == "chutap1",
       state_code %in% c("CL-AR", "CL-LR", "CL-LL"),
       obs_count > 0 | only_presence_reported == 1) %>%
  select(checklist_id, state_code, state_name) %>%
  inner_join(checklists, by = "checklist_id") %>%
  select(checklist_id, latitude, longitude, day_of_year, year,
         state_code, state_name) %>%
  grid_sample_stratified(case_control = FALSE)
# Chucao Tapaculo: 3380
# Thorn-tailed Rayadito: 2875
grid_sampled %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  write_sf("data/chutap1.gpkg")

# best months
month_counts <- grid_sampled %>%
  mutate(date = as.Date(glue("2024-{day_of_year}"), format = "%Y-%j"),
         month = lubridate::month(date)) %>%
  count(month)
