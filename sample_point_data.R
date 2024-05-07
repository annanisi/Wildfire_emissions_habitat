library(tidyverse)
library(sf)
library(tmap)
library(raster)
library(viridis)
library(units)

# Load spatial data -----------------------------------------------------

coord.sys <- 32610

# Load study area polygons
sa <- st_read('data/study_area_boundary') # Study area
puma.sa <- st_read('data/puma_study_area_subset') # Subset of study area over which pumas were monitored

# Load puma rasters
dep.sel.df <- read_csv(file = 'data/depredation_and_selection_df.csv')

r.depredation <- dep.sel.df %>%
  dplyr::select(x,y,depredation) %>%
  rasterFromXYZ(crs = coord.sys)# Predicted retaliatory killing risk following depredation

r.selection <- dep.sel.df %>%
  dplyr::select(x,y,selection) %>%
  rasterFromXYZ(crs = coord.sys)# Predicted puma habitat selection

r.lambda <- read_csv(file = 'data/lambda_df.csv') %>%
  rasterFromXYZ(crs = coord.sys) # Predicted puma population growth rate

# Load land use type data
wui <- st_read('data/land_use_types') # land use classifications

# Load household carbon footprint
bg_hhcf <- st_read('data/hhcf_block_group') # HHCF at block group level

# Load burn perimeters
calfire <- st_read('data/burn_perimeters')


# Areas of extent polygons ------------------------------------------------

# Calculate area of study region (defined as the hhcf boundary)
(study_area_size <- st_area(sa) %>%
  units::set_units(km^2)) # 4236.069 [km^2]
(study_area_puma_size <- st_area(puma.sa$geometry) %>%
  units::set_units(km^2)) # 2822.513 [km^2]

# Summarize wui by area ---------------------------------------------------

wui <- wui %>%
  mutate(percent = as.numeric(100*(area_km/study_area_size)))
wui %>% st_drop_geometry() # ~13% inteface, 18% intermix, 11% other (ag), 42% rural, 11% urban

write.csv(wui%>% st_drop_geometry(), file = 'Tables/wui_sum.csv') # Save

# Summarize burned area ---------------------------------------------------

burned_area <- calfire %>%
  st_area() %>%
  units::set_units(km^2)
burned_area  # 423.8 km2 burned since 2020

(burned_area_df <- data.frame(area = c(burned_area),
                             sa_area = c(study_area_size)) %>%
  mutate(percent = as.numeric(100*(area/sa_area)))) # 10% of  study area burned
write.csv(burned_area_df,'Tables/burned_area.csv')


#########################################################################################
####### Generate point data for comparing puma metrics, HCF across land use types #######
#########################################################################################

set.seed(1)

# Generate points ---------------------------------------------------------

pts.hcf <- st_sample(sa, 5000) %>% # Sample points from overall study area to test how hcf varies across land use types
  st_sf()

pts.puma <- st_sample(puma.sa, 5000) %>% # Sample points from within subset of study area over which pumas were monitored to test how puma habitat metrics vary across land use types
  st_sf()

# Get land use info (wui) for both -----------------------------------------

pts.hcf <- pts.hcf %>%
  st_join(dplyr::select(wui, land_use_type), join = st_within)

pts.puma <- pts.puma %>%
  st_join(dplyr::select(wui, land_use_type), join = st_within)

# Get puma habitat info ---------------------------------------------------

pts.puma <- pts.puma %>%
  mutate(lambda = extract(r.lambda,.),
         dep_risk = extract(r.depredation, .),
         selection = extract(r.selection, .))

tm_shape(pts.puma) + tm_dots(col = 'lambda')
tm_shape(pts.puma) + tm_dots(col = 'dep_risk')
tm_shape(pts.puma) + tm_dots(col = 'selection')

# Get HHCF and income info ------------------------------------------------

pts.hcf <- st_join(pts.hcf,
               bg_hhcf %>% dplyr::select("AVG_TRANS_2010","AVG_HOUSING_2010", "AVG_FOOD_2010", "AVG_GOODS_2010", "AVG_SERV_2010", "AVG_TOTAL_2010"),
               join = st_within)

# Visualize
tm_shape(pts.hcf) + tm_dots(col = 'AVG_TRANS_2010')
tm_shape(pts.hcf) + tm_dots(col = 'AVG_TOTAL_2010', breaks = seq(20, 100, 10))
tm_shape(bg_hhcf) + tm_fill(col = 'AVG_TOTAL_2010', breaks = seq(20, 100, 10))


# Add coordinates to dataframes -------------------------------------------

pts.puma <- pts.puma %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2])%>%
  relocate(x,y,.before = 0) %>%
  filter(land_use_type != 'Other')
pts.hcf <- pts.hcf %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2])%>%
  relocate(x,y,.before = 0)%>%
  filter(land_use_type != 'Other')
head(pts.puma)
head(pts.hcf)

write.csv(pts.puma %>% st_drop_geometry(), file = 'data/points_sample_for_puma_metrics.csv')
write.csv(pts.hcf %>% st_drop_geometry(), file = 'data/points_sample_for_hcf.csv')





