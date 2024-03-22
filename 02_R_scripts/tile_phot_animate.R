# 1 Libraries----

library(tidyverse)
library(sf)
library(gganimate)
library(readr)
library(ggfx)
library(showtext)
library(vctrs)
library(sf)
library(imager)

# 2 Fonts----
font_add_google("Lato", "lato")
font_add_google(name = "Leckerli One", family = "Leckerli")
font_add_google(name = "Pacifico", family = "Pacifico")
font_add_google(name = "Zen Dots", family = "Zen")
font_add_google(name = "Goldman", family = "Goldman")

showtext_auto()
showtext_opts(dpi = 300)

# 3 No.4----

#bck_po <- "#d6d2c4"

# theme_custom <- theme_void()+
#   theme(
#     plot.margin = margin(1,1,10,1,"pt"),
#     plot.background = element_rect(fill=bck_po,color=NA),
#     legend.position = "bottom",
#     legend.title = element_text(hjust=0.5,color="white",face="bold"),
#     legend.text = element_text(color="white")
#   )

mclaren_no4 <- read.csv('./00_raw_data/lando_no4_points_horizontal_flip.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

mclaren_no4_polygon <- mclaren_no4 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 3857) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

ggplot(mclaren_no4_polygon, aes())+
  geom_sf()

ggplot(mclaren_no4_polygon, aes())+
  geom_sf()+
  labs(fill="Member of a sport association")+
  guides(
    fill=guide_legend(
      nrow=1,
      title.position="top",
      label.position="bottom"
    ))

grd_mclaren_no4 <- st_make_grid(
  mclaren_no4_polygon, # map name 
  n = c(20,20)) %>%
  st_sf() %>% 
  mutate(id=row_number())

# Extract mclaren no4 centroids----
cent_grd_mclaren_no4 <- grd_mclaren_no4 %>%
  st_centroid()

# Take a look at the results
ggplot()+
  geom_sf(grd_mclaren_no4, mapping = aes(geometry=geometry))+
  geom_sf(cent_grd_mclaren_no4, mapping = aes(geometry=geometry), pch=21, size=0.5)+
  theme_void()

# Intersect centroids with basemap
cent_grd_mclaren_no4_clean <- cent_grd_mclaren_no4 %>%
  st_intersection(mclaren_no4_polygon)

# Make a centroid without geom
# (convert from sf object to tibble)
cent_grd_mclaren_no4_no_geom <- cent_grd_mclaren_no4_clean %>%
  st_drop_geometry()

# Join with grid thanks to id column
grd_mclaren_no4_clean <- grd_mclaren_no4 %>%
  #filter(id%in%sel)%>%
  left_join(cent_grd_mclaren_no4_no_geom)

# *6.1 nhs board area----
plot_no4 <- ggplot() +
  geom_sf(
    # drop_na() is one way to suppress the cells outside the country
    grd_mclaren_no4_clean %>% tidyr::drop_na(), 
    mapping = aes(geometry = geometry)) +
  geom_sf(cent_grd_mclaren_no4_clean, mapping = aes(geometry = geometry), fill=NA, pch=21, size=0.5) +
  geom_sf(mclaren_no4_polygon, mapping = aes(geometry = geometry))

plot_no4

grd_mclaren_no4_clean_t <- grd_mclaren_no4_clean %>% 
  bind_cols(random_t)

random_t <- rep(sample(1:5),1, each=80) %>% 
  sample()

random_t <- as_tibble(random_t) %>% 
  rename(t = value)

colnames(random_t)[1] <- 't'
colnames(my_dataframe)[2] ="c2"

scatter_plot_animate_no4 <- ggplot() + 
  geom_sf(
    grd_mclaren_no4_clean_t, 
    mapping = aes(geometry = geometry)) +
  transition_time(t) 

animate(scatter_plot_animate_no4, nframes = 30)

anim_save("./04_animate_gifs/sixth_saved_animation_logo_animate2.gif", height = 372, width = 538, units = "px")

# https://art-from-code.netlify.app/day-1/session-1/

# https://apps.automeris.io/wpd/

# https://happygitwithr.com/rstudio-git-github

# 4 Oscar tiled----

# https://blog.djnavarro.net/posts/2021-10-19_rtistry-posts/
# https://fronkonstin.com
# https://github.com/aschinchon/monsters-tiled/blob/master/tile.R

# Point to the place where your image is stored
oscar <- './00_raw_data/OP81_track_wall_crop1.jpg'
lando <- './00_raw_data/Lando1_cropped.jpg'

# Load and convert to grayscale
load.image(oscar) %>%
  grayscale() -> img

plot(img)

# The image is summarized into s x s squares 
s <- 2

# Resume pixels using mean: this decreases drastically the resolution of the image
img %>% 
  as.data.frame() %>%
  mutate(x = cut(x, round(dim(img)[1]/s, 0), labels = FALSE),
         y = cut(y, round(dim(img)[2]/s, 0), labels = FALSE)) %>%
  group_by(x, y) %>%
  summarise(value = mean(value)) -> df

# Create new variable to be used to define size and color of the lines of tiles
df %>% mutate(z = cut(value, breaks = 20, labels = FALSE)) -> df

# Initialize plot 
plot <- ggplot()

# Resulting plot will be build with 20 layers: one layer per each different value of z 
for (i in 1:20){
  sub_data = df %>% filter(z==i)
  plot <- plot + geom_tile(aes(x, y),
                           size = 2*i/(20-1)-2/(20-1),
                           fill = "#E27231",
                           col = paste0("gray", round(((100-5)*i)/(20-1)+5-(100-5)/(20-1), 0)),
                           data = df %>% filter(z==i))
}

# Last tweaks
plot_oscar_animate <- plot +
  coord_fixed() +
  scale_y_reverse() +
  theme_void() + 
  transition_states(z, transition_length = 3, state_length = 3, wrap = FALSE) + 
  shadow_mark() +
  enter_fade() +
  exit_fade()

animate(plot_oscar_animate, fps = 30, duration = 20, end_pause = 100)

#plot + transition_time(x) + shadow_mark()

anim_save("./04_animate_gifs/second_saved_animation_oscar_cubes.gif", height = 372, width = 538, units = "px")


dim(img)
dim(oscar)
width(img)
height(img)
depth(img)
spectrum(img)
img

# Point to the place where your image is stored
no81 <- './00_raw_data/No81_F1_square_cropped.jpg'

# Load and convert to grayscale
load.image(no81) %>%
  grayscale() -> img_no81

plot(img_no81)

# The image is summarized into s x s squares 
s <- 3

# Resume pixels using mean: this decreases drastically the resolution of the image
img_no81 %>% 
  as.data.frame() %>%
  mutate(x = cut(x, round(dim(img_no81)[1]/s, 0), labels = FALSE),
         y = cut(y, round(dim(img_no81)[2]/s, 0), labels = FALSE)) %>%
  group_by(x, y) %>%
  summarise(value = mean(value)) -> df_no81

# Create new variable to be used to define size and color of the lines of tiles
df_no81 %>% mutate(z = cut(value, breaks = 20, labels = FALSE)) -> df_no81

# Initialize plot 
plot_no81 <- ggplot()

# Resulting plot will be build with 20 layers: one layer per each different value of z 
for (i in 1:20){
  sub_data = df_no81 %>% filter(z==i)
  plot_no81 <- plot_no81 + geom_tile(aes(x, y),
                                     size = 2*i/(20-1)-2/(20-1),
                                     fill = "#E27231",
                                     col = paste0("gray", round(((100-5)*i)/(20-1)+5-(100-5)/(20-1), 0)),
                                     data = df_no81 %>% filter(z==i))
}

# Last tweaks
plot_no81 <- plot_no81 +
  coord_fixed() +
  scale_y_reverse() -> plot_no81

#plot_no81 + transition_manual(-x, cumulative = TRUE)
plot_no81 + transition_states(z) + shadow_mark()

animate(plot_no81, fps = 30, duration = 10)

df_to_bind <- df %>% 
  mutate(image = 'oscar')

df_no81_to_bind <- df_no81 %>% 
  mutate(image = 'no81')

two_animate_df <- bind_rows(df_to_bind,
                            df_no81_to_bind)

# Initialize plot 
plot_two_animate <- ggplot()

# Resulting plot will be build with 20 layers: one layer per each different value of z 
for (i in 1:20){
  sub_data = two_animate_df %>% filter(z==i)
  plot_two_animate <- plot_two_animate + geom_tile(aes(x, y),
                                                   size = 2*i/(20-1)-2/(20-1),
                                                   fill = "#E27231",
                                                   col = paste0("gray", round(((100-5)*i)/(20-1)+5-(100-5)/(20-1), 0)),
                                                   data = two_animate_df %>% filter(z==i))
}

# Last tweaks
plot_two_animate <- plot_two_animate +
  coord_fixed() +
  scale_y_reverse() -> plot_two_animate

# plot_two_animate + transition_manual(case_when(image == 'oscar' ~ x, TRUE ~ -x), cumulative = TRUE)
plot_two_animate + transition_time(case_when(image == 'oscar' ~ x, TRUE ~ -x)) + shadow_mark()

plot_two_animate + transition_components(case_when(image == 'oscar' ~ x, TRUE ~ -x), 
                                         enter_length = 3, 
                                         exit_length = 3) +
  enter_fade() +
  exit_fade() 

# df_no81_edit <- df_no81 %>% 
#   filter(x > 3 & x < 48 & y >12 & y < 38)
# 
# # Initialize plot 
# plot_no81_edit <- ggplot()
# 
# # Resulting plot will be build with 20 layers: one layer per each different value of z 
# for (i in 1:20){
#   sub_data = df_no81_edit %>% filter(z==i)
#   plot_no81_edit <- plot_no81_edit + geom_tile(aes(x, y),
#                                      size = 2*i/(20-1)-2/(20-1),
#                                      fill = "#E27231",
#                                      col = paste0("gray", round(((100-5)*i)/(20-1)+5-(100-5)/(20-1), 0)),
#                                      data = df_no81_edit %>% filter(z==i))
# }
# 
# # Last tweaks
# plot_no81_edit <- plot_no81_edit +
#   coord_fixed() +
#   scale_y_reverse() -> plot_no81_edit
# 
# #plot_no81_edit + transition_manual(-x, cumulative = TRUE)
# plot_no81_edit + transition_time(x) + shadow_mark()


plot_oscar / plot_no81

# Point to the place where your image is stored
b_of_f <- './00_raw_data/bride_of_frankenstein2_cropped.jpeg'

# Load and convert to grayscale
load.image(b_of_f) %>%
  grayscale() -> img_b_of_f

plot(img_b_of_f)

# The image is summarized into s x s squares 
s <- 10

# Resume pixels using mean: this decreases drastically the resolution of the image
img_b_of_f %>% 
  as.data.frame() %>%
  mutate(x = cut(x, round(dim(img_b_of_f)[1]/s, 0), labels = FALSE),
         y = cut(y, round(dim(img_b_of_f)[2]/s, 0), labels = FALSE)) %>%
  group_by(x, y) %>%
  summarise(value = mean(value)) -> df_b_of_f

# Create new variable to be used to define size and color of the lines of tiles
df_b_of_f %>% mutate(z = cut(value, breaks = 20, labels = FALSE)) -> df_b_of_f

#df_b_of_f <- df_b_of_f %>% group_by(x) %>% slice_sample(n = 45) %>% ungroup()

plot_b_of_f <- ggplot()

pal_b_of_f <- colorRampPalette(c("steelblue4", "mistyrose"))
colours_b_of_f <- pal_b_of_f(20)
colours_b_of_f
colours_b_of_f[1]

pal_fill_b_of_f <- colorRampPalette(c("olivedrab4", "deeppink"))
colours_fill_b_of_f <- pal_fill_b_of_f(20)
colours_fill_b_of_f

# Resulting plot will be build with 20 layers: one layer per each different value of z 
for (i in 1:20){
  sub_data = df_b_of_f %>% filter(z==i)
  plot_b_of_f <- plot_b_of_f + geom_tile(aes(x, y),
                                         size = 2*i/(20-1)-2/(20-1),
                                         fill = colours_fill_b_of_f[i],
                                         col = colours_b_of_f[i],
                                         data = df_b_of_f %>% filter(z==i))
}

# Last tweaks
plot_b_of_f_animate <- plot_b_of_f +
  coord_fixed() +
  scale_y_reverse() +
  theme_void() 
# + 
#   transition_states(z, transition_length = 3, state_length = 3, wrap = FALSE) + 
#   shadow_mark() +
#   enter_fade() +
#   exit_fade()

plot_b_of_f_animate

animate(plot_b_of_f_animate, fps = 30, duration = 20, end_pause = 100)

ggsave('./03_plots/b_of_f_tiled.png', plot, height =  8 , width =  6)


# Last tweaks
plot_oscar_animate <- plot +
  coord_fixed() +
  scale_y_reverse() +
  theme_void() + 
  transition_states(z, transition_length = 3, state_length = 3, wrap = FALSE) + 
  shadow_mark() +
  enter_fade() +
  exit_fade()

animate(plot_oscar_animate, fps = 30, duration = 20, end_pause = 100)

# gil photo

# Point to the place where your image is stored
gil <- './00_raw_data/gil_cropped.jpg'

# Load and convert to grayscale
load.image(gil) %>%
  grayscale() -> img_gil

plot(img_gil)

# The image is summarized into s x s squares 
s <- 52.5

# Resume pixels using mean: this decreases drastically the resolution of the image
img_gil %>% 
  as.data.frame() %>%
  mutate(x = cut(x, round(dim(img_gil)[1]/s, 0), labels = FALSE),
         y = cut(y, round(dim(img_gil)[2]/s, 0), labels = FALSE)) %>%
  group_by(x, y) %>%
  summarise(value = mean(value)) -> df_gil

# Create new variable to be used to define size and color of the lines of tiles
df_gil %>% mutate(z = cut(value, breaks = 20, labels = FALSE)) -> df_gil

# Initialize plot 

plot_gil <- ggplot()

pal <- colorRampPalette(c("steelblue4", "mistyrose"))
colours <- pal(20)
colours
colours[1]

pal_fill <- colorRampPalette(c("olivedrab3", "turquoise2"))
colours_fill <- pal_fill(20)

# Resulting plot will be build with 20 layers: one layer per each different value of z 
for (i in 1:20){
  sub_data = df_gil %>% filter(z==i)
  plot_gil <- plot_gil + geom_tile(aes(x, y),
                                         size = 2*i/(20-1)-2/(20-1),
                                         fill = "olivedrab3",
                                         col = paste0("gray", round(((100-5)*i)/(20-1)+5-(100-5)/(20-1), 0)),
                                         data = df_gil %>% filter(z==i))
}

for (i in 1:20){
  sub_data = df_gil %>% filter(z==i)
  plot_gil <- plot_gil + geom_tile(aes(x, y),
                                   size = 2*i/(20-1)-2/(20-1),
                                   fill = colours_fill[i],
                                   col = colours[i],
                                   data = df_gil %>% filter(z==i))
}

# Last tweaks
plot_gil_animate <- plot_gil +
  coord_fixed() +
  scale_y_reverse() +
  theme_void() 
# + 
#   transition_states(z, transition_length = 3, state_length = 3, wrap = FALSE) +
#   shadow_mark() +
#   enter_fade() +
#   exit_fade()

plot_gil_animate

animate(plot_gil_animate, fps = 30, duration = 20, end_pause = 100)

ggsave('./03_plots/gill_tiled.png', plot, height =  8 , width =  6)





