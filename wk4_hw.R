library(countrycode)
library(sf)
library(tmap)
library(tidyverse)
library(janitor)
library(here)
library(countrycode)
library(usethis)

# read in world_outline shape file
world_outline <- st_read(here("world_countries_generalized", "World_Countries_Generalized.shp")) %>% 
  st_transform(4326) %>% 
  clean_names()

# inspect
qtm(world_outline)
View(world_outline)
st_crs(world_outline)

# read in composite indices time series
composite_indices <- read.csv(here("HDR23-24_Composite_indices_complete_time_series.csv"))

# inspect
View(composite_indices)

# create gii_indices dataframe by selecting only relevant columns
# and filter for countries only
gii_indices <- composite_indices %>% 
  dplyr::select(c(1, 2, 3, 4),
                contains("gii_201")) %>% 
  dplyr::filter(!grepl("\\.", iso3))

# inspect
View(gii_indices)

# add iso3c country code column to world_outline before merge
world_outline <- world_outline %>% 
  add_column(iso3 = countrycode(world_outline$iso, "iso2c", "iso3c"),
             .before = "countryaff")

# inspect
View(world_outline)

# merge world_outline and gii_indices
world_gii_indices <- world_outline %>% 
  left_join(.,
            gii_indices,
            by="iso3")

# filter out unwanted columns and rows with NA values in gii_2010 and gii_2019
world_gii_indices_clean <- world_gii_indices %>% 
  dplyr::select(!c("iso",
                   "aff_iso",
                   "countryaff",
                   "country.y")) %>% 
  dplyr::rename("country" = country.x) %>% 
  dplyr::filter(!is.na(gii_2019) & !is.na(gii_2010))

# inspect
View(world_gii_indices_clean)

# add column for difference in gii between 2010 and 2019
world_gii_indices_diff <- world_gii_indices_clean %>% 
  dplyr::mutate(gii_improvement_2010_2019 = -((gii_2019 - gii_2010) * 100)) %>% 
  dplyr::select(c(1, 2, 3, 4),
                "gii_improvement_2010_2019")

View(world_gii_indices_diff)

tm_shape(world_outline) +
  tm_polygons(alpha = 0) +
tm_shape(world_gii_indices_diff) +
  tm_polygons("gii_improvement_2010_2019",
              style="jenks",
              midpoint=0,
              title="GII Improvement",
              palette="RdYlGn") +
tm_compass(position = c("right", "top"), type="arrow", size = 0.8) +
tm_scale_bar(position = c("left", "bottom")) +
tm_layout(main.title = "GII Improvement between 2010 and 2019",
          main.title.position = c("center", "top"))