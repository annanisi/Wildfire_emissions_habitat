library(sf)
library(raster)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)

# Load data ---------------------------------------------------------------

sa <- st_read('data/study_area_boundary')
bg <- st_read('data/hcf_block_group')

# Set coordinate system
coord.sys <- 32610

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

# Set palettes ------------------------------------------------------------

hcf.pal <- hcl.colors(n = 4, palette = 'YlOrR')
wui.pal <- hcl.colors(n = 4, palette = 'ag_GrnYl')
lambda.pal <- hcl.colors(n = 10, palette   = 'BurgYl')
dep.pal <- plasma(8)
sel.pal <- hcl.colors(n = 10, palette = 'YlGnBu')

# Format data -------------------------------------------------------------

wui <- wui %>%
  filter(land_use_type != 'Other') %>%
  mutate(land_use_type = as.factor(land_use_type),
         land_use_type = fct_relevel(land_use_type, c("Sparse",'Intermix','Interface','Urban')))

ca <- ne_download(scale = 10, type = 'states', category = 'cultural',returnclass = 'sf') %>% filter(name == 'California') %>%
  st_transform( crs = coord.sys)

plot.bb <- st_bbox(sa) # set the same plotting extent


# Statewide map -----------------------------------------------------------
ca.map <- ggplot() +
  geom_sf(data = ca, fill = 'gray80', col = 'gray20') +
  geom_sf(data = sa, color = NA,fill = 'black') +
  geom_sf(data = plot.bb %>% st_as_sfc(), fill = NA, col = 'black') +
  coord_sf() +
  theme_void()
ca.map

# Burn map ----------------------------------------------------------------
calfire <- calfire %>%
  mutate('Wildfire area' = '')

map.burn <- ggplot() +
  geom_sf(data = calfire, mapping = aes(fill = 'Wildfire area'), color = NA) +
  scale_fill_manual(values = "#DC267F") +
  geom_sf(data = sa, color = "black",  lwd =.5,fill = NA,linetype = 1) +
  coord_sf(xlim = c(plot.bb$xmin, plot.bb$xmax), ylim = c(plot.bb$ymin, plot.bb$ymax)) +
  theme_void() +
  theme(legend.position ='none') +
  annotation_scale(mapping = aes(location = 'br'))
map.burn


# HCF map ----------------------------------------------------------------
legend.pos = c(.7,.7)
map.hcf <- ggplot() +
  geom_sf(data = bg, aes(fill = AVG_TOTAL_2010), col = NA) +
  scale_fill_gradientn(colours = rev(hcf.pal), na.value="white", breaks = seq(20, 92, 20),
                    name = expression("Mean household C footprint (tCO"[2]*'e)')) +
  geom_sf(data = sa, color = "black",  lwd =.5,fill = NA,linetype = 1) +
  coord_sf(xlim = c(plot.bb$xmin, plot.bb$xmax), ylim = c(plot.bb$ymin, plot.bb$ymax)) +
  theme_void() +
  guides(fill = guide_colorbar(position = 'inside'))+
  theme(legend.position.inside = legend.pos,
        legend.title = element_text(hjust = 1))+
  annotation_scale(mapping = aes(location = 'br'))
map.hcf

# WUI map ----------------------------------------------------------------

map.wui <- ggplot() +
  geom_sf(data = wui, aes(fill = land_use_type), col = NA) +
  scale_fill_manual(values = wui.pal,
                    name = "Land use type") +
  geom_sf(data = sa, color = "black",  lwd =.5,fill = NA,linetype = 1) +
  coord_sf(xlim = c(plot.bb$xmin, plot.bb$xmax), ylim = c(plot.bb$ymin, plot.bb$ymax)) +
  theme_void() +
  guides(fill = guide_legend(position = 'inside'))+
  theme(legend.position.inside = c(.9,.9),
        legend.title = element_text(hjust = 1))+
  annotation_scale(mapping = aes(location = 'br'))
map.wui

# Lambda map --------------------------------------------------------------

map.lambda <- ggplot() +
   geom_raster(data = r.lambda %>% rasterToPoints() %>% data.frame(),
               mapping = aes(x = x, y = y, fill = lambda)) +
  scale_fill_gradientn(colours = lambda.pal,na.value="white", name = 'Puma population growth') +
   geom_sf(data = sa, color = "black",  lwd =.5,fill = NA,linetype = 1) +
   coord_sf(xlim = c(plot.bb$xmin, plot.bb$xmax), ylim = c(plot.bb$ymin, plot.bb$ymax)) +
   theme_void() +
  guides(fill = guide_colorbar(position = 'inside'))+
  theme(legend.position.inside = legend.pos,
        legend.title = element_text(hjust = 1))+
  annotation_scale(mapping = aes(location = 'br'))
map.lambda


# Depredation risk --------------------------------------------------------

map.dep <- ggplot() +
   geom_raster(data = r.depredation %>% rasterToPoints() %>% data.frame(),
               mapping = aes(x = x, y = y, fill = depredation)) +
   scale_fill_gradientn(colours = dep.pal,na.value="white", name = 'Human-puma conflict risk') +
   geom_sf(data = sa, color = "black",  lwd =.5,fill = NA,linetype = 1) +
   coord_sf(xlim = c(plot.bb$xmin, plot.bb$xmax), ylim = c(plot.bb$ymin, plot.bb$ymax)) +
   theme_void() +
  guides(fill = guide_colorbar(position = 'inside'))+
  theme(legend.position.inside = legend.pos,
        legend.title = element_text(hjust = 1))+
  annotation_scale(mapping = aes(location = 'br'))
map.dep

map.selection <- ggplot() +
    geom_raster(data = r.selection %>%
                  rasterToPoints() %>% data.frame() %>%
                  mutate(selection = if_else(selection >= 3, 3, selection)), # Truncate high values to improve readability
                mapping = aes(x = x, y = y, fill = selection)) +
    scale_fill_gradientn(colours = sel.pal,
                         na.value="white", name = 'Puma space use') +
    geom_sf(data = sa, color = "black",  lwd =.5,fill = NA,linetype = 1) +
    coord_sf(xlim = c(plot.bb$xmin, plot.bb$xmax), ylim = c(plot.bb$ymin, plot.bb$ymax)) +
    theme_void() +
  guides(fill = guide_colorbar(position = 'inside'))+
  theme(legend.position.inside = legend.pos,
        legend.title = element_text(hjust = 1))+
  annotation_scale(mapping = aes(location = 'br'))
map.selection


# Save map components -----------------------------------------------------

png(filename = 'Figures/maps/state_map.png', width = 4, height = 4,res = 500, units = 'in')
ca.map + theme(text = element_text(size = 10))
dev.off()

png(filename = 'Figures/maps/wui_map.png', width = 4, height = 4,res = 500, units = 'in')
map.wui + theme(text = element_text(size = 10), legend.position.inside = c(.85,.85))
dev.off()

png(filename = 'Figures/maps/hcf_map.png', width = 4, height = 4,res = 500, units = 'in')
map.hcf + theme(text = element_text(size = 10), legend.position.inside = c(.7,.81))
dev.off()

png(filename = 'Figures/maps/burn_map.png', width = 4, height = 4,res = 500, units = 'in')
map.burn + theme(text = element_text(size = 12), legend.position.inside = c(.85,.92))
dev.off()

png(filename = 'Figures/maps/lambda_map.png', width = 4, height = 4,res = 500, units = 'in')
map.lambda + theme(text = element_text(size = 10), legend.position.inside = c(.81,.81))
dev.off()

png(filename = 'Figures/maps/dep_map.png', width = 4, height = 4,res = 500, units = 'in')
map.dep + theme(text = element_text(size = 10), legend.position.inside = c(.81,.81))
dev.off()

png(filename = 'Figures/maps/selection_map.png', width = 4, height = 4,res = 500, units = 'in')
map.selection + theme(text = element_text(size = 10), legend.position.inside = c(.81,.81))
dev.off()

