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
tmp_dir <- tempdir()
shp_path <- file.path(tmp_dir, "UberH3_7.shp")
hex_sf <- st_read(shp_path)

# Read the JSON file with province polygons
province_json <- s3read_using(FUN = read_sf,
                              object = "/SA Land Use/afs_province.json",
                              bucket = "projet-esteem",
                              opts = list("region" = ""))

province_json <- province_json[-9,] # remove the isolatede island
province_json <- st_set_crs(province_json, 4326) # set CRS to WGS 84

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
