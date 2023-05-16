library(dplyr)
library(rnaturalearth)
library(sf)

# file to save spatial data
gpkg_file <- "data/gis-data.gpkg"
dir.create(dirname(gpkg_file), showWarnings = FALSE, recursive = TRUE)

# political boundaries
# land border with lakes removed
ne_land <- ne_download(scale = 10, category = "cultural",
                       type = "admin_0_countries_lakes",
                       returnclass = "sf") %>%
  filter(CONTINENT == "South America") %>%
  st_set_precision(1e6) %>%
  st_union() %>%
  st_make_valid()
# state boundaries for chile
bb <- st_bbox(c(xmin = -76, ymin = -55, xmax = -65, ymax = -15),
              crs = 4326) %>%
  st_as_sfc()
ne_states <- ne_download(scale = 10, category = "cultural",
                         type = "admin_1_states_provinces",
                         returnclass = "sf") %>%
  filter(iso_a2 == "CL") %>%
  select(state = name, state_code = iso_3166_2) %>%
  st_make_valid() %>%
  st_intersection(bb)
# country lines
# downloaded globally then filtered to north america with st_intersect()
ne_country_lines <- ne_download(scale = 10, category = "cultural",
                                type = "admin_0_boundary_lines_land",
                                returnclass = "sf") %>%
  st_geometry() %>%
  st_make_valid()
ne_country_lines <- st_intersects(ne_country_lines, ne_land, sparse = FALSE) %>%
  as.logical() %>%
  {ne_country_lines[.]}
# states, north america
ne_state_lines <- ne_download(scale = 10, category = "cultural",
                              type = "admin_1_states_provinces_lines",
                              returnclass = "sf") %>%
  filter(ADM0_A3 == "CHL") %>%
  st_geometry() %>%
  st_make_valid()
# proteced areas
protected_areas <- read_sf("data-raw/chile-protected-areas/snaspe.shp") %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  select(objectid, nombre = Nombre, tipo_snasp = Tipo_Snasp,
         region = Region, cod_region = Cod_Region)

# save all layers to a geopackage
unlink(gpkg_file)
write_sf(ne_land, gpkg_file, "ne_land")
write_sf(ne_states, gpkg_file, "ne_states")
write_sf(ne_country_lines, gpkg_file, "ne_country_lines")
write_sf(ne_state_lines, gpkg_file, "ne_state_lines")
write_sf(protected_areas, gpkg_file, "protected_areas")
