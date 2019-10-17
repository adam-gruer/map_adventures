library(rmapzen)
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
mz_set_tile_host_nextzen()
melb <- st_point(c( 144.9631608, -37.8142176)) %>%  st_sfc(crs = 4326) %>% 
  st_transform(3111) %>% st_buffer(20000) %>% st_transform(4326)
bbox <- st_bbox(melb)
melb_bbox <- mz_rect(bbox[1], bbox[2], bbox[3], bbox[4])
melb_tiles <- mz_vector_tiles(melb_bbox)

names(melb_tiles)
walk(names(melb_tiles), function(feature){
  if(length(melb_tiles[[feature]]$features) > 0){
  assign(feature, as_sf(melb_tiles[[feature]]), pos = 1)
  }
})

count(st_drop_geometry(transit), kind, sort = TRUE)

  map(melb_tiles, "features") %>% map(length)
ggplot() +
 geom_sf(data = earth, fill = "lightgreen", alpha = 0.1, colour = NA)+
 geom_sf(data = water, fill = "lightblue", colour = NA) +
  
#  geom_sf(data = landuse, aes(fill = kind), colour = NA) +
 geom_sf(data = filter(roads, kind %in% c("major_road", "highway")),
          aes(colour = kind), show.legend = "line")  +
  geom_sf(data = transit, aes(colour = kind), show.legend = "line") +
 geom_sf_label(data = filter(places, kind == "locality"), aes(label = name)) +
  theme_void() + 
  theme(panel.grid.major = element_line(size = 0))

ggsave("metro_melb.png")




