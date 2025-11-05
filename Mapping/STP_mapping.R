
#################################################################################

library(aws.s3)
library(tidyverse)
library(dplyr)
library(readxl)
library(pheatmap)

# Read the data file with FTE figures by hex7 id number 
data_raw <- s3read_using(FUN = read_xlsx,
                         object = "National_FTE_Industry5d.xlsx",
                         bucket = "hadjilazarop", 
                         opts = list("region" = ""))
data_formated <- data_raw %>% 
  mutate(FTE = as.numeric(FTE))
data_formated %>% 
  filter(TaxYear == 2024) %>% 
  group_by(SIC7_4d) %>% 
  summarise(FTE = sum(FTE, na.rm = TRUE))





#################################################################################

library(aws.s3)
library(tidyverse)
library(scales)
library(sf)

# Read the data file with FTE figures by hex7 id number 
h7csv_raw <- s3read_using(FUN = read.csv,
                          object = "/SA Land Use/hex7_FTE_Industry5d.csv",
                          bucket = "projet-esteem", 
                          opts = list("region" = ""))

# Read the shapefile (only .shp is needed, others are automatically picked up if present)
prefix <- "SA Land Use/hexagonal_shapefiles/UberH3_7"
files <- get_bucket("projet-esteem", prefix = prefix, region = "")

tmp_dir <- tempdir()

for (f in files) {
  key <- f[["Key"]]
  dest <- file.path(tmp_dir, basename(key))
  save_object(object = key, bucket = "projet-esteem", file = dest, region = "")
}

shp_path <- file.path(tmp_dir, "UberH3_7.shp")
hex_sf <- st_read(shp_path)

# Read the JSON file with province polygons
province_json <- s3read_using(FUN = read_sf,
                              object = "/SA Land Use/afs_province.json",
                              bucket = "projet-esteem",
                              opts = list("region" = ""))

province_json <- province_json[-9,] # remove the isolatede island
province_json <- st_set_crs(province_json, 4326) # set CRS to WGS 84

# Read Cape Town City municipality boundary data
prefix <- "SA Land Use/CTC_shapefiles/metropolitan municipality cpt"
files <- get_bucket("projet-esteem", prefix = prefix, region = "")

tmp_dir <- tempdir()

for (f in files) {
  key <- f[["Key"]]
  dest <- file.path(tmp_dir, basename(key))
  save_object(object = key, bucket = "projet-esteem", file = dest, region = "")
}

shp_path <- file.path(tmp_dir,"metropolitan municipality cpt.shp")
CTC_sf <- st_read(shp_path)
CTC_sf <- st_set_crs(CTC_sf,4236)

# Convert the FTE data into numeric format, including turning "<10" to 5 
h7csv_raw_numeric <- h7csv_raw %>%
  mutate(FTE = ifelse(FTE == "<10", 5, FTE)) %>% 
  mutate(FTE = as.numeric(FTE))

# Some basic descriptive statistics
hex_FTE_total <- h7csv_raw_numeric %>%
  group_by(hex7,CAT_B,TaxYear) %>% 
  summarise(FTE = sum(FTE, na.rm = T))

h7csv_raw_numeric %>%
  group_by(CAT_B) %>% 
  summarise(n())

# Join FTE data with h7 polygons and set as sf for plotting
hex_FTE_total_sf <- left_join(hex_FTE_total,hex_sf) %>% 
  st_as_sf()


# FTE in South Africa at the hex level
hex_FTE_total_sf %>% 
  filter(TaxYear == 2022) %>%
  ggplot(.) +
  geom_sf(aes(fill = FTE), colour = NA) +
  geom_sf(data = province_json, fill = NA, colour = "black", linewidth = 0.3) +
  scale_fill_viridis_c(
    name   = "Total FTE",
    option = "plasma",
    trans  = "log10",                   
    labels = comma_format(accuracy = 1)
  ) +
  labs(title = "Employment density (FTE) – hex-grid view",
       caption = "Source: SARS Spatial Tax Panel (hex-7); own calculations") +
  theme_minimal()


hex_FTE_total_sf %>% 
  filter(TaxYear == 2022 & CAT_B == "CPT") %>%
  ggplot(.) +
  geom_sf(aes(fill = FTE), colour = NA) +
  geom_sf(data = CTC_sf, fill = NA, colour = "black", linewidth = 0.3) +
  scale_fill_viridis_c(
    name   = "Total FTE",
    option = "plasma",
    trans  = "log10",                   
    labels = comma_format(accuracy = 1)
  ) +
  labs(title = "Total FTE in Cape Town City – hex-grid view",
       caption = "Source: SARS Spatial Tax Panel (hex-7); own calculations") +
  theme_minimal()



#################################################################################

library(aws.s3)
library(tidyverse)
library(scales)
library(sf)

# Read the data file with FTE figures by hex7 id number 
municipal_csv_raw <- s3read_using(FUN = read.csv,
                          object = "/SA Land Use/Municipal_FTE_Industry5d.csv",
                          bucket = "projet-esteem", 
                          opts = list("region" = ""))


# Read the shapefile (only .shp is needed, others are automatically picked up if present)
prefix <- "SA Land Use/Local_Municipal_Shapefiles/MDB_Local_Municipal_Boundary_2018"
files <- get_bucket("projet-esteem", prefix = prefix, region = "")

tmp_dir <- tempdir()

for (f in files) {
  key <- f[["Key"]]
  dest <- file.path(tmp_dir, basename(key))
  save_object(object = key, bucket = "projet-esteem", file = dest, region = "")
}

shp_path <- file.path(tmp_dir, "MDB_Local_Municipal_Boundary_2018.shp")
municipalities_sf <- st_read(shp_path)


municipal_csv_raw <- municipal_csv_raw %>%
  mutate(
    FTE = as.numeric(ifelse(FTE == "<10", 5, FTE))  # replace "<10" with 5
  )

fte_by_muni <- municipal_csv_raw %>%
  filter(TaxYear == 2014) %>% 
  group_by(CAT_B) %>%
  summarise(FTE_total = sum(FTE, na.rm = TRUE)) %>%
  arrange(desc(FTE_total))

fte_map <- municipalities_sf %>%
  left_join(fte_by_muni, by = c("CAT_B" = "CAT_B"))

ggplot(fte_map) +
  geom_sf(aes(fill = FTE_total), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(
    fill = "Total FTE (2014)",
    title = "Employment Intensity by Municipality, South Africa (2014)"
  ) +
  theme_minimal()


