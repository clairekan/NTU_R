library(ggmap)
library(ggplot2)
library(gganimate)
library(viridis)
library(RColorBrewer)
library(raster)
library(tidyr)
library(tweenr)
library(magick)
library(stringr)
library(lubridate)

res <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res.csv')
sbi <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_sbi.csv')

# Map
map <- get_map(location = c(min(res$lng), min(res$lat), max(res$lng), max(res$lat)), maptype = "toner")

# Map + Data
res.stat.map <- ggmap(map, darken = c(0.5, "white")) %+% res + aes(x = lng, y = lat, z = X307) +
  stat_summary_2d(fun = median, alpha = 0.6) +
  scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), space = 'Lab') +
  labs(x = "Longitude", y = "Latitude") +
  coord_map() +
  ggtitle('Remaining Amount of Youbike in Taipei')

print(res.stat.map)


# res
res3 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res1.csv')
res3_g <- gather(res3, time, per, X7:X24)
res3_g$time <- str_replace(res3_g$time, 'X', '')

# Animation
res.stat.map.ani <- ggmap(map, darken = c(0.5, "white")) %+% res3_g + aes(x = lng, y = lat, z = per) +
  stat_summary_2d(fun = median, alpha = 0.6) +
  scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), space = 'Lab') +
  labs(title = 'Monday, {closest_state}', x = "Longitude", y = "Latitude") +
  coord_map() +
  transition_states(time, transition_length = 1, state_length = 1) 

print(res.stat.map.ani)

# Location
ggmap(map, darken = c(0.5, "white")) %+% res + aes(x = lng, y = lat) +
  geom_jitter()