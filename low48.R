library(raster)
library(ggplot2)
library(ggthemes)
library(sf)

x <- readRDS("wind_speed_power.rds")
proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
r <- 100 * crop(subset(x[[2]], 4), extent(-126, -66, 24, 50))
names(r) <- "data"
poly <- as(as(as(r, "SpatialPixelsDataFrame"), "SpatialPolygonsDataFrame"), "sf")
us <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

breaks <- round(seq(min(r[]), max(r[]), length = 10))
sfg <- scale_fill_gradient2(low = "darkblue", high = "darkred", breaks = sort(c(0, breaks[-which.min(abs(breaks))])))
gde <- guides(fill = guide_legend(title = "% \u0394", reverse = TRUE))
thm <- theme(legend.key.width = unit(1, "cm"), legend.key.height = unit(1, "cm"), legend.position = c(-0.01, 0.52),
             panel.grid.major = element_line(colour = "transparent"))

g <- ggplot() +  geom_sf(data = poly, aes(fill = data), color = "white", alpha = 0.8, size = 1) + 
  geom_sf(data = us, fill = "transparent", color = "#444444", size = 1) + 
  coord_sf(crs = proj) + sfg + gde + theme_map(base_size = 20) + thm +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(title = "2000 - 2100 projected percent change in wind speed cubed", subtitle = "Proportional to power")
ggsave("plots/wind_power_us.png", g, width = 16, height = 9.6, dpi = 300)

# not used
caption <- "Wind power is proportional to the cube of wind speed. Figure X shows estimated percent change in the annual average of daily wind speed cubed from year 2000 to 2100. Percent change is based on general circulation models (GCMs) quantile mapped to the 2.5-degree European Re-analysis (ERA-40) baseline."
