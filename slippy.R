library(sf)
library(slippymath)
melb <- st_point(c( 144.9631608, -37.8142176)) %>%  st_sfc(crs = 4326) %>% 
  st_transform(3111) %>% st_buffer(1000) %>% st_transform(4326)

st_bbox(melb)
uluru_bbox <-
  st_bbox(c(xmin = 131.02084,
            xmax = 131.0535,
            ymin = -25.35461,
            ymax = -25.33568),
          crs = st_crs("+proj=longlat +ellps=WGS84"))

bbox_tile_query(st_bbox(melb))
bbox_to_tile_grid(uluru_bbox, max_tiles = 15)

library(purrr)
library(curl)
library(glue)

tile_grid <- bbox_to_tile_grid(st_bbox(melb), max_tiles = 31)

mapbox_query_string <-
  paste0("https://api.mapbox.com/v4/mapbox.satellite/{zoom}/{x}/{y}.jpg90",
         "?access_token=",
         Sys.getenv("MAPBOX_API_KEY"))

images <-
  pmap(tile_grid$tiles,
       function(x, y, zoom){
         outfile <- glue("{x}_{y}.jpg")
         curl_download(url = glue(mapbox_query_string),
                       destfile = outfile) 
         outfile 
       },
       zoom = tile_grid$zoom)

library(raster)
library(rgdal)

raster_out <- compose_tile_grid(tile_grid, images)
nlayers((raster_out))
library(tmap)
raster::RGB(raster_out)
plotRGB(raster_out)
tm_shape(raster_out) +
  tm_raster()
plotRGB(raster_out)

u.df <- as.data.frame(raster_out, xy = TRUE)
colr <- u.df[,3:5] %>% 
  pmap_chr (function(layer.1,layer.2, layer.3){
    rgb(layer.1, layer.2, layer.3, max =255)
    }) 
u.df$colr <- colr
my.cols = levels(factor(u.df$colr))
names(my.cols) <- my.cols

u.df <- u.df %>% 
  mutate(colr = factor(colr))

ggplot( u.df  ) +
  geom_tile(aes(x = x, y = y, fill = colr)) +
  scale_fill_manual(values = my.cols, guide = "none")

round(0:255 / 8)
#aggregate(raster_out, fact=1)
u.df <- as.data.frame(aggregate(raster_out,fact = 2), xy = TRUE)
colr <- u.df[,3:5] %>% 
  mutate_all(function(x){
      floor((x)/16)*16
      }) %>% 
  pmap_chr (function(layer.1,layer.2, layer.3){
     
   rgb(layer.1 , layer.2 , layer.3 , maxColorValue=255)
  }) 
u.df$colr <- colr
my.cols = levels(factor(u.df$colr))
names(my.cols) <- my.cols

u.df <- u.df %>% 
  mutate(colr = factor(colr))

ggplot( u.df  ) +
  geom_tile(aes(x = x, y = y, fill = colr)) +
  scale_fill_manual(values = my.cols, guide = "none")

raster_out.aggregate <- aggregate(raster_out, fact=4)
raster_out.aggregate
plotRGB(raster_out.aggregate)

## A convenient wrapper for raster image exports using png::writePNG.
raster_to_png(raster_out, "uluru.png")
