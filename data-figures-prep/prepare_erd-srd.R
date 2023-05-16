library(DBI)
library(arrow)
library(fs)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(units)
library(tictoc)

erd_dir <- "/ocean/projects/deb200005p/sligocki/erd_prep/erd2021"
srd_dir <- "/ocean/projects/deb200005p/sligocki/stem_hwf/ERD2021_inputs"
output_dir <- "/ocean/projects/deb200005p/mstrimas/chile-data/"
temp_dir <- "/local/"
dir_create(output_dir)

# chile boundary
chile_boundary <- ne_download(scale = 10, category = "cultural",
                              type = "admin_0_countries",
                              returnclass = "sf") %>%
  filter(ISO_A2 == "CL") %>%
  select(iso_a2 = ISO_A2) %>%
  st_make_valid() %>%
  st_wrap_dateline() %>%
  st_buffer(dist = set_units(25, "km"))
bb <- st_bbox(chile_boundary)

# erd ----

# db connection
# str_glue("cp {path(erd_dir, 'erd.db')} {path(temp_dir)}") %>%
#   system()
con <- dbConnect(RSQLite::SQLite(), path(temp_dir, "erd.db"))

# checklists from within chile bounding box
checklists <- tbl(con, "checklists") %>%
  filter(all_obs_reported,
         longitude > !!bb[["xmin"]], longitude < !!bb[["xmax"]],
         latitude > !!bb[["ymin"]], latitude < !!bb[["ymax"]])

# observations from chile
tic()
observations <- tbl(con, "obs") %>%
  inner_join(checklists, by = "checklist_id") %>%
  select(checklist_id, species_code,
         only_presence_reported, valid,
         obs_count) %>%
  mutate(only_presence_reported = only_presence_reported == 1,
         valid = valid == 1,
         obs_count = ifelse(only_presence_reported, NA_integer_, as.integer(obs_count))) %>%
  collect()
toc()

# collect checklists
tic()
checklists <- checklists %>%
  select(checklist_id, observer_id,
         loc_id, longitude, latitude, is_stationary,
         year, day_of_year, hours_of_day, solar_noon_diff_mid,
         effort_hrs, effort_distance_km, num_observers,
         cds_u10, cds_v10, cds_d2m, cds_t2m, cds_hcc, cds_i10fg,
         cds_mcc, cds_lcc, cds_sf, cds_rf, cds_slc, cds_msl,
         eastness_1km_median, eastness_1km_sd,
         eastness_90m_median, eastness_90m_sd,
         northness_1km_median, northness_1km_sd,
         northness_90m_median, northness_90m_sd,
         elev_30m_median, elev_30m_sd,
         elev_250m_median, elev_250m_sd,
         island,
         astwbd_fs_c1_1500_ed, astwbd_fs_c1_1500_pland,
         astwbd_fs_c2_1500_ed, astwbd_fs_c2_1500_pland,
         astwbd_fs_c3_1500_ed, astwbd_fs_c3_1500_pland,
         gp_rtp_1, gp_rtp_2, gp_rtp_3, gp_rtp_4, gp_rtp_5,
         intertidal_fs_c1_1500_ed, intertidal_fs_c1_1500_pland,
         ntl_mean, ntl_sd,
         mcd12q1_lccs1_fs_c1_1500_ed, mcd12q1_lccs1_fs_c1_1500_pland,
         mcd12q1_lccs1_fs_c2_1500_ed, mcd12q1_lccs1_fs_c2_1500_pland,
         mcd12q1_lccs1_fs_c11_1500_ed, mcd12q1_lccs1_fs_c11_1500_pland,
         mcd12q1_lccs1_fs_c12_1500_ed, mcd12q1_lccs1_fs_c12_1500_pland,
         mcd12q1_lccs1_fs_c13_1500_ed, mcd12q1_lccs1_fs_c13_1500_pland,
         mcd12q1_lccs1_fs_c14_1500_ed, mcd12q1_lccs1_fs_c14_1500_pland,
         mcd12q1_lccs1_fs_c15_1500_ed, mcd12q1_lccs1_fs_c15_1500_pland,
         mcd12q1_lccs1_fs_c16_1500_ed, mcd12q1_lccs1_fs_c16_1500_pland,
         mcd12q1_lccs1_fs_c21_1500_ed, mcd12q1_lccs1_fs_c21_1500_pland,
         mcd12q1_lccs1_fs_c22_1500_ed, mcd12q1_lccs1_fs_c22_1500_pland,
         mcd12q1_lccs1_fs_c31_1500_ed, mcd12q1_lccs1_fs_c31_1500_pland,
         mcd12q1_lccs1_fs_c32_1500_ed, mcd12q1_lccs1_fs_c32_1500_pland,
         mcd12q1_lccs1_fs_c41_1500_ed, mcd12q1_lccs1_fs_c41_1500_pland,
         mcd12q1_lccs1_fs_c42_1500_ed, mcd12q1_lccs1_fs_c42_1500_pland,
         mcd12q1_lccs1_fs_c43_1500_ed, mcd12q1_lccs1_fs_c43_1500_pland,
         mcd12q1_lccs1_fs_c255_1500_ed, mcd12q1_lccs1_fs_c255_1500_pland,
         mcd12q1_lccs2_fs_c25_1500_ed, mcd12q1_lccs2_fs_c25_1500_pland,
         mcd12q1_lccs2_fs_c35_1500_ed, mcd12q1_lccs2_fs_c35_1500_pland,
         mcd12q1_lccs2_fs_c36_1500_ed, mcd12q1_lccs2_fs_c36_1500_pland,
         mcd12q1_lccs3_fs_c27_1500_ed, mcd12q1_lccs3_fs_c27_1500_pland,
         mcd12q1_lccs3_fs_c50_1500_ed, mcd12q1_lccs3_fs_c50_1500_pland,
         mcd12q1_lccs3_fs_c51_1500_ed, mcd12q1_lccs3_fs_c51_1500_pland) %>%
  rename(effort_hours = effort_hrs,
         number_observers = num_observers) %>%
  mutate(effort_speed_kmph = effort_distance_km / effort_hours,
         .after = effort_distance_km) %>%
  collect()
toc()
dbDisconnect(con)

# srd ----

# db connection
# str_glue("cp {path(srd_dir, 'srd_3km_all.db')} {path(temp_dir)}") %>%
#   system()
con <- dbConnect(RSQLite::SQLite(), path(temp_dir, "srd_3km_all.db"))

# cells within chile bounding box
tic()
srd <- tbl(con, "srd") %>%
  filter(longitude > !!bb[["xmin"]], longitude < !!bb[["xmax"]],
         latitude > !!bb[["ymin"]], latitude < !!bb[["ymax"]]) %>%
  select(srd_id,
         longitude, latitude,
         eastness_1km_median, eastness_1km_sd,
         eastness_90m_median, eastness_90m_sd,
         northness_1km_median, northness_1km_sd,
         northness_90m_median, northness_90m_sd,
         elev_30m_median, elev_30m_sd,
         elev_250m_median, elev_250m_sd,
         island,
         astwbd_fs_c1_1500_ed, astwbd_fs_c1_1500_pland,
         astwbd_fs_c2_1500_ed, astwbd_fs_c2_1500_pland,
         astwbd_fs_c3_1500_ed, astwbd_fs_c3_1500_pland,
         gp_rtp_1, gp_rtp_2, gp_rtp_3, gp_rtp_4, gp_rtp_5,
         intertidal_fs_c1_1500_ed, intertidal_fs_c1_1500_pland,
         ntl_mean, ntl_sd,
         mcd12q1_lccs1_fs_c1_1500_ed, mcd12q1_lccs1_fs_c1_1500_pland,
         mcd12q1_lccs1_fs_c2_1500_ed, mcd12q1_lccs1_fs_c2_1500_pland,
         mcd12q1_lccs1_fs_c11_1500_ed, mcd12q1_lccs1_fs_c11_1500_pland,
         mcd12q1_lccs1_fs_c12_1500_ed, mcd12q1_lccs1_fs_c12_1500_pland,
         mcd12q1_lccs1_fs_c13_1500_ed, mcd12q1_lccs1_fs_c13_1500_pland,
         mcd12q1_lccs1_fs_c14_1500_ed, mcd12q1_lccs1_fs_c14_1500_pland,
         mcd12q1_lccs1_fs_c15_1500_ed, mcd12q1_lccs1_fs_c15_1500_pland,
         mcd12q1_lccs1_fs_c16_1500_ed, mcd12q1_lccs1_fs_c16_1500_pland,
         mcd12q1_lccs1_fs_c21_1500_ed, mcd12q1_lccs1_fs_c21_1500_pland,
         mcd12q1_lccs1_fs_c22_1500_ed, mcd12q1_lccs1_fs_c22_1500_pland,
         mcd12q1_lccs1_fs_c31_1500_ed, mcd12q1_lccs1_fs_c31_1500_pland,
         mcd12q1_lccs1_fs_c32_1500_ed, mcd12q1_lccs1_fs_c32_1500_pland,
         mcd12q1_lccs1_fs_c41_1500_ed, mcd12q1_lccs1_fs_c41_1500_pland,
         mcd12q1_lccs1_fs_c42_1500_ed, mcd12q1_lccs1_fs_c42_1500_pland,
         mcd12q1_lccs1_fs_c43_1500_ed, mcd12q1_lccs1_fs_c43_1500_pland,
         mcd12q1_lccs1_fs_c255_1500_ed, mcd12q1_lccs1_fs_c255_1500_pland,
         mcd12q1_lccs2_fs_c25_1500_ed, mcd12q1_lccs2_fs_c25_1500_pland,
         mcd12q1_lccs2_fs_c35_1500_ed, mcd12q1_lccs2_fs_c35_1500_pland,
         mcd12q1_lccs2_fs_c36_1500_ed, mcd12q1_lccs2_fs_c36_1500_pland,
         mcd12q1_lccs3_fs_c27_1500_ed, mcd12q1_lccs3_fs_c27_1500_pland,
         mcd12q1_lccs3_fs_c50_1500_ed, mcd12q1_lccs3_fs_c50_1500_pland,
         mcd12q1_lccs3_fs_c51_1500_ed, mcd12q1_lccs3_fs_c51_1500_pland,
         mod44w_oic_fs_c1_1500_ed, mod44w_oic_fs_c1_1500_pland,
         mod44w_oic_fs_c2_1500_ed, mod44w_oic_fs_c2_1500_pland,
         mod44w_oic_fs_c3_1500_ed, mod44w_oic_fs_c3_1500_pland,
         mcd12q1_lccs2_fs_c9_1500_ed, mcd12q1_lccs2_fs_c9_1500_pland) %>%
  collect()
toc()
dbDisconnect(con)

# spatial subset ----

# checklists
checklists_ss <- checklists %>%
  select(checklist_id, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_join(chile_boundary, left = FALSE) %>%
  st_drop_geometry() %>%
  semi_join(checklists, ., by = "checklist_id")
message("Observations decreased from ", scales::comma(nrow(checklists)), " to ",
        scales::comma(nrow(checklists_ss)), " rows")

# observations
observations_ss <- semi_join(observations, checklists_ss, by = "checklist_id")
message("Observations decreased from ", scales::comma(nrow(observations)), " to ",
        scales::comma(nrow(observations_ss)), " rows")

# srd
srd_ss <- srd %>%
  select(srd_id, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_join(chile_boundary, left = FALSE) %>%
  st_drop_geometry() %>%
  semi_join(srd, ., by = "srd_id")
message("Observations decreased from ", scales::comma(nrow(srd)), " to ",
        scales::comma(nrow(srd_ss)), " rows")

# save to parquet
write_parquet(observations_ss, path(output_dir, "ebird_observations_chile_2021.parquet"))
write_parquet(checklists_ss, path(output_dir, "ebird_checklists_chile_2021.parquet"))
write_parquet(srd_ss, path(output_dir, "ebird_prediction-grid_chile_2021.parquet"))
