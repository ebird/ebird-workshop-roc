library(fs)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(ebirdst)
library(auk)

input_dir <- "/ocean/projects/deb200005p/sligocki/post_processing/"
output_dir <- "/ocean/projects/deb200005p/mstrimas/chile-data/"
dir_create(output_dir)

regional_stats <- dir_ls(input_dir, regexp = "ERD2021.*") %>%
  map(path, "web_package/regionalStats-2022.csv") %>%
  map_chr(as.character) %>%
  read_csv() %>%
  filter(str_starts(region_code, "CHL"))

# code lookup
lookup <- ne_download(scale = 10, category = "cultural",
                      type = "admin_1_states_provinces",
                      returnclass = "sf") %>%
  st_drop_geometry() %>%
  filter(adm0_a3 == "CHL") %>%
  distinct(region_code = adm1_code,
         region_code_new = iso_3166_2,
         region_name = name) %>%
  bind_rows(tibble(region_code = "CHL", region_code_new = "CL", region_name = "Chile"))
regional_stats <- regional_stats %>%
  left_join(lookup, by = "region_code") %>%
  select(region_type, region_code = region_code_new, region_name,
         species_code, season_code,
         abundance_mean, percent_population = total_pop_percent,
         percent_region_occupied = range_occupied_percent,
         percent_range_in_region = range_total_percent,
         days_occupation = range_days_occupation)
write_csv(regional_stats, path(output_dir, "ebirdst-2021_regional-stats_chile.csv"),
          na = "")

# species list
chile_stats <- regional_stats %>%
  filter(region_code == "CL",
         percent_population > 0.01 | percent_region_occupied > 0.05) %>%
  distinct(species_code) %>%
  inner_join(ebirdst_runs, by = "species_code") %>%
  inner_join(ebird_taxonomy %>% select(species_code, family, taxon_order),
             by = "species_code") %>%
  arrange(taxon_order) %>%
  select(species_code, common_name, scientific_name, family, is_resident = resident)
write_csv(chile_stats, path(output_dir, "ebirdst-2021_species-list_chile.csv"),
          na = "")
