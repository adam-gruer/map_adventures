library(raster)
library(tidyverse)
help(pack = "raster")


r_g_b <- replicate(3,
                   raster(nrows = 6,
                          ncols = 6,
                          vals = sample(0:255, 36, replace = TRUE))
)
r
plot(r_g_b)

stack

filename <- system.file("external/rlogo.grd", package="raster")
b <- brick(filename)
raster::st

raster::stack(r_g_b)

slogo <- stack(system.file("external/rlogo.grd", package="raster")) 
nlayers(slogo)
slogo
plot(slogo)
slogo.df <- as.data.frame(slogo, xy = TRUE) %>% 
  pmap_dfr(function(x,y,red,green, blue){
    
    tibble(x = x,
           y = y,
           value = rgb(red, green, blue, max =255))}) 
my.cols = levels(factor(slogo.df$value))
names(my.cols) <- my.cols

slogo.df <- slogo.df %>% 
  mutate(value = factor(value))

ggplot( slogo.df  ) +
  geom_tile(aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = my.cols, guide = "none")
plotRGB(slogo)

