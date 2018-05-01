library(ggplot2)
library(raster)
path1 <- "/Data/Base_Data/Climate/World/GCM_raw/IPCC_AR5_daily/historical_tifs/quantile_mapped"
path2 <- "/Data/Base_Data/Climate/World/GCM_raw/IPCC_AR5_daily/projected_tifs/rcp60/quantile_mapped"
files1 <- purrr::map(c("uas", "vas"), ~list.files(file.path(path1, .x), "GFDL.*tif$", full = TRUE))
files2 <- purrr::map(c("uas", "vas"), ~list.files(file.path(path2, .x), "GFDL.*tif$", full = TRUE))
years <- 1958:2100

# functions
get_mean <- function(x, files, days = NULL, cube = FALSE){
  if(is.null(days)){
    x1 <- readAll(brick(files[[1]][x]))
    x2 <- readAll(brick(files[[2]][x]))
  } else {
    x1 <- subset(readAll(brick(files[[1]][x])), days)
    x2 <- subset(readAll(brick(files[[2]][x])), days)
  }
  x <- sqrt(x1^2 + x2^2)
  if(cube) x <- x^3
  calc(x, mean)
}

lm_betas <- function(x, ...) if(any(is.na(x))) c(NA, NA) else as.numeric(lm(x ~ years)$coefficients)

pct_change <- function(years, mod){
  int <- subset(mod, 1)
  slope <- subset(mod, 2)
  proj_fitted <- int + slope * years[2]
  clim_fitted <- int + slope * years[1]
  proj_fitted / clim_fitted - 1
}

prep <- function(files, days = NULL, cube = FALSE, cores = 64){
  rotate(brick(
    parallel::mclapply(seq_along(files[[1]]), get_mean, 
                       files = files, days = days, cube = cube, mc.cores = cores)
    ))
}

# wind
b1 <- prep(files1)
b2 <- prep(files2)
b <- brick(stack(b1, b2, quick = TRUE))
names(b) <- years
r_clim <- calc(subset(b, which(years %in% 1980:2009)), mean)
b_pct <- b / r_clim - 1
s_lm <- calc(b, lm_betas, years = years)
w_pct <- brick(stack(
  calc(subset(b_pct, which(years %in% 2010:2039)), mean),
  calc(subset(b_pct, which(years %in% 2040:2069)), mean),
  calc(subset(b_pct, which(years %in% 2070:2099)), mean),
  pct_change(c(2000, 2100), s_lm)
))

# wind cubed (proportional to power)
b1 <- prep(files1, cube = TRUE)
b2 <- prep(files2, cube = TRUE)
b <- brick(stack(b1, b2, quick = TRUE))
names(b) <- years
r_clim <- calc(subset(b, which(years %in% 1980:2009)), mean)
b_pct <- b / r_clim - 1
s_lm <- calc(b, lm_betas, years = years)
p_pct <- brick(stack(
  calc(subset(b_pct, which(years %in% 2010:2039)), mean),
  calc(subset(b_pct, which(years %in% 2040:2069)), mean),
  calc(subset(b_pct, which(years %in% 2070:2099)), mean),
  pct_change(c(2000, 2100), s_lm)
))

out <- list(speed = w_pct, power = p_pct)
saveRDS(out, "/workspace/UA/mfleonawicz/wind_speed_power.rds")

# xy <- xyFromCell(r_pct_slope, which.max(r_pct_slope[]))
# xy
# d <- data.frame(year = 2020:2099, magnitude = as.numeric(extract(b, xy)))
# d2 <- data.frame(year = 2020:2099, magnitude = as.numeric(extract(b, c(-105, 39.7))))
# 
# p1 <- ggplot(d, aes(year, magnitude)) + geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") + geom_point() +
#   labs(title = "Projected wind proportional to power off southern coast of Tasmania", subtitle = "Annual average of cubed daily average speeds", 
#        x = "Year", y = expression(Wind~speed~(m/s))) + snapplot::theme_snap() +
#   scale_x_continuous(limits = c(2019, 2100), expand = c(0, 0.8))
# ggsave("/workspace/UA/mfleonawicz/wind0.png", p1, width = 10, height = 7, dpi = 300)
# 
# p2 <- ggplot(d2, aes(year, magnitude)) + geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") + geom_point() +
#   labs(title = "Projected wind proportional to power, Denver metropolitan area", subtitle = "Annual average of cubed daily average speeds", 
#        x = "Year", y = expression(Wind~speed~(m/s))) + snapplot::theme_snap() +
#   scale_x_continuous(limits = c(2019, 2100), expand = c(0, 0.8))
# ggsave("/workspace/UA/mfleonawicz/wind1.png", p2, width = 10, height = 7, dpi = 300)
