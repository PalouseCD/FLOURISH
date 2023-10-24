#PRISM Precipitation Data Download
# precip units are mm


library(prism)
library(tidyr)
library(readr)
library(rgdal)
library(raster)
library(exactextractr)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra, # handle raster data
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  dplyr, # data wrangling
  tidyr, # data wrangling
  data.table, # data wrangling
  prism, # download PRISM data
  tictoc, # timing codes
  tigris, # to get county sf
  tmap # for mapping
)

#setting path for download
#options(prism.path = "_04_Project_Data/R/FLOURISH/data/PRISM/")

# downloading PRISM data
#get_prism_normals(
#  type = "ppt",
#  resolution = "800m",
#  annual = TRUE,
#  keepZip = FALSE
#)

# reading in field polygons
field_polygons <- st_read(dsn = "data/PRISM", "field_polygons1")


# reading in PRISM data
prism_file <- "data/PRISM/PRISM_ppt_30yr_normal_800mM4_annual_bil/PRISM_ppt_30yr_normal_800mM4_annual_bil.bil"
prism_ppt <- rast(prism_file)

#field_polygons_trans <- spTransform(field_polygons_vect,
                                   # crs(prism_ppt))
field_polygons

prism_ppt


# adding ppt to polygon data

ppt_field <-
  exact_extract(
    prism_ppt,
    field_polygons
  )

ppt_field[1:2] %>% lapply(function(x)head(x))

ppt_combined <- bind_rows(ppt_field, .id = "id") %>%
  as_tibble()

rbindlist(ppt_field, idcol = "id")

# calculating coverage averaged mean of ppt normal in polygons
(
  ppt_by_id <- ppt_combined %>%
    mutate(id = as.numeric(id)) %>%
    group_by(id) %>%
    summarise(ppt_aw = sum(value * coverage_fraction) / sum(coverage_fraction))
  )

# Joining ppt to vector dataset and removing unneccesary columns
field_polygons_ppt <- field_polygons %>%
  mutate(id := row_number()) %>%
  left_join(., ppt_by_id, by = "id") %>%
  select(Field_ID, Producer_N, ppt_aw)


# Converting from mm to inches

field_polygons_ppt <- field_polygons_ppt %>%
  mutate(ppt_in = ppt_aw/25.4)

#writing data as a csv
st_write(field_polygons_ppt, "data/PRISM/fieldppt.csv", driver = "CSV")

