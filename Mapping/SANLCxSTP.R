<<<<<<< HEAD
# install.packages("terra")
library(terra)
library(aws.s3)
library(sf)
library(tidyverse)
library(dplyr)


=======
install.packages("terra")
library(terra)

process_tiffs(bla)
>>>>>>> 1922ccb3e021ae72cbe221b316ed503480830e4d

# Read tiff. of SANLC 
SANLCdata <- s3read_using(FUN = rast,
                                  object = "/SA Land Use/SANLC/SA_NLC_2022_ALBERS.tif",
                                  bucket = "projet-esteem", 
                                  opts = list("region" = ""))



# Read the shapefile of municipality borders
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



<<<<<<< HEAD
# df_muni_FTE: prend en colonne les noms des municipalités (212) et en ligne les nombres d'industries (420)
# Read the data file with FTE figures by hex7 id number 
municipal_csv_raw <- s3read_using(FUN = read.csv,
                                  object = "/SA Land Use/Municipal_FTE_Industry5d.csv",
                                  bucket = "projet-esteem", 
                                  opts = list("region" = ""))


municipal_csv_raw <- municipal_csv_raw %>%
  mutate(
    FTE = as.numeric(ifelse(FTE == "<10", 5, FTE))  # replace "<10" with 5
  )
=======
# build df_muni_FTE qui prend en colonne les noms des municipalités (212) et en ligne les nombres d'industries (420)
>>>>>>> 1922ccb3e021ae72cbe221b316ed503480830e4d
df_muni_FTE <- municipal_csv_raw %>% 
  filter(TaxYear == 2023) %>% 
  select(CAT_B, SIC7_4d, FTE) %>% 
  group_by(CAT_B, SIC7_4d) %>% 
  summarise(FTE = sum(FTE, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(
    names_from = CAT_B,   # colonnes = municipalités
    values_from = FTE, 
    values_fill = 0       # remplit les manquants par 0
  )

<<<<<<< HEAD
# df_muni_pixel_LC: the names of municipalities in columns and the 73 land cover map categories in rows : each cell indicating the number of pixel of each LC categories in each municipality  
municipalities_sf <- st_transform(municipalities_sf, crs(SANLCdata))
zonal_counts <- terra::extract(SANLCdata, municipalities_sf, fun=NULL, freq=TRUE)
# SAVE zonal_counts !!!!! 
  
writeRaster(SANLCdata, "SANLCdata_tmp.tif", overwrite = TRUE)
# Reopen from that file (this creates a stable SpatRaster)
SANLCdata <- rast("SANLCdata_tmp.tif")

# 4. Extract counts of each land-cover class per municipality
terraOptions(tempdir = "/home/onyxia/work/terra_tmp", memfrac = 0.7)
zonal_counts <- extract(SANLCdata, municipalities_sf, fun = NULL, freq = TRUE, touches = TRUE, exact = FALSE, cores = 8)




Pour chaque pixel
- trouve la municipalité dans laquelle il est localisé: municipality_name
- indique le type de land cover de ce pixel: LC_name
- Multiplie les vecteurs de taille 420: 
  x nombre de FTE de la municipalité du pixel: colonne de df_muni_FTE correspondante à municipality_name
  x dummy LCxSIC: colonne de df_LCxSIC_corresp_table correspondante à LC_name
  x parts des perimetres: a vector of lenght 420 with the same repeated value (1/df_muni_pixel_LC[LC_name, municipality_name]) # the inverse of the number of pixel of the same LC_name in the municipality
-> outcome per pixel = vector of lenght 420 with the nombre de FTE par secteur sur ce pixel 
-> assigne à ce pixel les nombres de FTE en plusieurs "bandes" sectorielles 
=======
# build df_perim_shares: a dataframe of dimension {420; 214} with 214 columns with each the same number: 1/[number of pixel in the corresponding municipality]


Pour chaque pixel
(- créé une liste de 420 entrées aux noms des secteur: lst_sector_pix)
- trouve la municipalité dans laquelle il est localisé: municipality_name
- indique le type de land cover de ce pixel: LC_name
- Multiplie les vecteurs de taille 420: 
  x nombre de FTE: colonne de df_muni_FTE correspondante à municipality_name
  x dummy LCxSIC: colonne de df_LCxSIC_corresp_table correspondante à LC_name
  x parts des perimetres: colonne de df_perim_shares correspondante à la municipality_name


liste le nombre de FTE par secteur dans cette municipalité 

assigne à ce pixel (une liste, ou un vecteur 400) les nombres de FTE (en plusieurs "bandes") de la municipalité pondérée par 
- le nombre de pixels dans la municipalité
- les 
>>>>>>> 1922ccb3e021ae72cbe221b316ed503480830e4d




################


# Read the data file with FTE figures by hex7 id number 
municipal_csv_raw <- s3read_using(FUN = read.csv,
                                  object = "/SA Land Use/Municipal_FTE_Industry5d.csv",
                                  bucket = "projet-esteem", 
                                  opts = list("region" = ""))


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