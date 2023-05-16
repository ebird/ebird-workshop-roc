library(sf)
library(terra)
library(tidyverse)
library(glue)
library(units)


# landscape-metrics.png ----

set.seed(1)

pt <- st_sfc(st_point(x = c(0, 0)), crs = 4326)
buffer <- st_buffer(pt, 3000)
bb <- st_bbox(buffer)
radius <- st_linestring(x = matrix(c(0, 0, 0, bb["ymax"]), 2, 2)) %>%
  st_sfc()
landscape <- rast(st_buffer(st_as_sf(buffer), 500))
values(landscape) <- sample(1:3, ncell(landscape), replace = TRUE)
cls <- data.frame(id = 1:3,
                  cover = c("Urban", "Grassland", "Forest"))
levels(landscape) <- cls

png("images/landscape-metrics.png", width = 800, height = 700, pointsize = 20)
par(mar = c(0, 0, 0, 0))
plot(landscape, axes = FALSE)
plot(buffer, add = TRUE, lwd = 2)
plot(radius, add = TRUE, lwd = 4, lty = 2)
plot(pt, pch = 19, add = TRUE, cex = 2)
text(0.005, 0.015, "1.5km")
dev.off()


# grid-sampling.png ----

set.seed(1)

# generate random points
grid_size <- 0.125
pts <- data.frame(x = runif(1000, 0, 1) %>% sqrt(),
                  y = runif(1000, 0, 1)^2,
                  obs = sample(c("non-detection", "detection"),
                               1000, replace = TRUE,
                               prob = c(0.98, 0.02))) %>%
  mutate(obs = factor(obs, levels = c("detection", "non-detection")),
         x_grid = x %/% grid_size,
         y_grid = y %/% grid_size,
         grid_cell = paste(x_grid, y_grid, sep = "-"))
pts_sampled <- pts %>%
  group_by(obs, grid_cell) %>%
  slice_sample(n = 1) %>%
  ungroup()
grid_vertical <- data.frame(x = seq(0, 1, by = grid_size),
                            xend = seq(0, 1, by = grid_size),
                            y = rep(0, 1 + 1 / grid_size),
                            yend = rep(1, 1 + 1 / grid_size))
grid_horizontal <- data.frame(y = seq(0, 1, by = grid_size),
                              yend = seq(0, 1, by = grid_size),
                              x = rep(0, 1 + 1 / grid_size),
                              xend = rep(1, 1 + 1 / grid_size))

pct <- scales::percent(mean(pts$obs == "detection"))
gg1 <- ggplot(pts) +
  aes(x = x, y, color = obs, size = obs) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())

gg2 <- ggplot(pts) +
  aes(x = x, y, color = obs, size = obs) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  facet_wrap(~ obs, nrow = 1) +
  labs(x = NULL, y = NULL) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_blank())

gg3 <- ggplot() +
  geom_point(data = pts,
             aes(x = x, y, color = obs, size = obs),
             show.legend = FALSE) +
  geom_segment(data = grid_vertical,
               aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_segment(data = grid_horizontal,
               aes(x = x, xend = xend, y = y, yend = yend)) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  facet_wrap(~ obs, nrow = 1) +
  labs(x = NULL, y = NULL) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_blank())

gg4 <- ggplot() +
  geom_point(data = pts_sampled,
             aes(x = x, y, color = obs, size = obs),
             show.legend = FALSE) +
  geom_segment(data = grid_vertical,
               aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_segment(data = grid_horizontal,
               aes(x = x, xend = xend, y = y, yend = yend)) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  facet_wrap(~ obs, nrow = 1) +
  labs(x = NULL, y = NULL) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_blank())

pct_sampled <- scales::percent(mean(pts_sampled$obs == "detection"))
gg5 <- ggplot(pts_sampled) +
  aes(x = x, y, color = obs, size = obs) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = c("#4daf4a", "#55555599")) +
  scale_size_discrete(range = c(3, 1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())


ggsave("images/grid-sampling_1.png", gg1, width = 400, height = 200, units = "px",
       scale = 7, bg = "white")
ggsave("images/grid-sampling_2.png", gg2, width = 400, height = 200, units = "px",
       scale = 7, bg = "white")
ggsave("images/grid-sampling_3.png", gg3, width = 400, height = 200, units = "px",
       scale = 7, bg = "white")
ggsave("images/grid-sampling_4.png", gg4, width = 400, height = 200, units = "px",
       scale = 7, bg = "white")
ggsave("images/grid-sampling_5.png", gg5, width = 400, height = 200, units = "px",
       scale = 7, bg = "white")
