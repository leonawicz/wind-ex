library(raster)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(sf)

world <- map_data("world")

x <- readRDS("wind_speed_power.rds")
d1 <- as(x[[1]], "SpatialPixelsDataFrame") %>% data.frame %>% tbl_df
d2 <- as(x[[2]], "SpatialPixelsDataFrame") %>% data.frame %>% tbl_df
d <- bind_rows(speed = d1, power = d2, .id = "wind")
names(d) <- c("wind", "2010 - 2039", "2040 - 2069", "2070 - 2099", "2000 - 2100", "x", "y")
d <- gather(d, time, value, -wind, -x, -y) %>% mutate(value = 100 * value) %>% rename(`Percent change` = value)
ds30 <- filter(d, wind == "speed" & time != "2000 - 2100")
ds100 <- filter(d, wind == "speed" & time == "2000 - 2100")
dp30 <- filter(d, wind == "power" & time != "2000 - 2100")
dp100 <- filter(d, wind == "power" & time == "2000 - 2100")

breaks <- list(
  round(seq(min(ds30["Percent change"]), max(ds30["Percent change"]), length = 10)),
  round(seq(min(ds100["Percent change"]), max(ds100["Percent change"]), length = 10)),
  round(seq(min(dp30["Percent change"]), max(dp30["Percent change"]), length = 10)),
  sort(c(-10, round(seq(min(dp100["Percent change"]), max(dp100["Percent change"]), length = 10))))
)
sfg <- lapply(breaks, function(x) scale_fill_gradient2(low = "darkblue", high = "darkred", breaks = sort(c(0, x[-which.min(abs(x))]))))
gde <- list(
  guides(fill = guide_legend(title = "% \u0394", nrow = 1, direction = "horizontal", label.position = "bottom", keywidth = unit(1.1, "cm"), label.hjust = 0.5)),
  guides(fill = guide_legend(title = "% \u0394", reverse = TRUE))
)
thm <- list(
  theme(strip.background = element_blank(), legend.position = c(0.5, 0.025)),
  theme(legend.key.width = unit(0.3, "cm"), legend.key.height = unit(1.1, "cm"), legend.position = c(0, 0.075))
)
gw <- geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.25)
gt <- geom_tile(aes(x, y, fill = `Percent change`), alpha = 1) 
fct <- facet_wrap(~time, nrow = 2)

g1a <- ggplot(ds30) + gt + fct + sfg[[1]] + coord_equal() + theme_map() + gde[[1]] + thm[[1]] + gw +
  labs(title = "Projected percent change in wind speed", subtitle = "From 1980 - 2099 by future period")
g1b <- ggplot(ds100) + gt + sfg[[2]] + coord_equal() + theme_map() + gde[[2]] + thm[[2]] + gw +
  labs(title = "2000 - 2100 projected percent change in wind speed")
g2a <- ggplot(dp30) + gt + fct + sfg[[3]] + coord_equal() + theme_map() + gde[[1]] + thm[[1]] + gw +
  labs(title = "Projected percent change in wind speed cubed", subtitle = "Proportional to power, from 1980 - 2099 by future period")
g2b <- ggplot(dp100) + gt + sfg[[4]] + coord_equal() + theme_map() + gde[[2]] + thm[[2]] + gw +
  labs(title = "2000 - 2100 projected percent change in wind speed cubed", subtitle = "Proportional to power")

ggsave("plots/wind_speed0.png", g1a, width = 10, height = 7, dpi = 300)
ggsave("plots/wind_speed1.png", g1b, width = 10, height = 7, dpi = 300)
ggsave("plots/wind_power0.png", g2a, width = 10, height = 7, dpi = 300)
ggsave("plots/wind_power1.png", g2b, width = 10, height = 7, dpi = 300)

# Lower 48 states
proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
r <- 100 * crop(subset(x[[2]], 4), extent(-126, -66, 24, 50))
names(r) <- "data"
pixel <- as(r, "SpatialPixelsDataFrame")
poly <- as(pixel, "SpatialPolygonsDataFrame")
poly_sf <- as(poly, "sf")
us <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

breaks <- round(seq(min(r[]), max(r[]), length = 10))
sfg <- scale_fill_gradient2(low = "darkblue", high = "darkred", breaks = sort(c(0, breaks[-which.min(abs(breaks))])))
gde <- guides(fill = guide_legend(title = "% \u0394", reverse = TRUE))
thm <- theme(legend.key.width = unit(1, "cm"), legend.key.height = unit(1, "cm"), legend.position = c(-0.01, 0.52),
             panel.grid.major = element_line(colour = "transparent"))

g <- ggplot() +  geom_sf(data = poly_sf, aes(fill = data), color = "white", alpha = 0.8, size = 1) + 
  geom_sf(data = us, fill = "transparent", color = "#444444", size = 1) + coord_sf(crs = proj) +
  sfg + theme_map(base_size = 20) + gde + thm +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(title = "2000 - 2100 projected percent change in wind speed cubed", subtitle = "Proportional to power")
ggsave("plots/wind_power_us.png", g, width = 16, height = 9.6, dpi = 300)

caption <- "Wind power is proportional to the cube of wind speed. Figure X shows estimated percent change in the annual average of daily wind speed cubed from year 2000 to 2100. Percent change is based on general circulation models (GCMs) quantile mapped to the 2.5-degree European Re-analysis (ERA-40) baseline."
