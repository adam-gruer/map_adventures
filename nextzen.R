library(rmapzen)
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
mz_set_tile_host_nextzen()
melb <- st_point(c( 144.9631608, -37.8142176)) %>%  st_sfc(crs = 4326) %>% 
  st_transform(3111) %>% st_buffer(1000) %>% st_transform(4326)
bbox <- st_bbox(melb)
melb_bbox <- mz_rect(bbox[1], bbox[2], bbox[3], bbox[4])
melb_tiles <- mz_vector_tiles(melb_bbox)

walk(names(melb_tiles), function(feature){
  assign(feature, as_sf(melb_tiles[[feature]]), pos = 1)
})

count(st_drop_geometry(roads), kind, sort = TRUE)
  
ggplot() +
 geom_sf(data = water, fill = "lightblue", colour = NA) +
  geom_sf(data = filter(roads, kind %in% c("major_road", "minor_road")),
          aes(colour = kind))  +
  theme_void() + 
  theme(panel.grid.major = element_line(size = 0))


