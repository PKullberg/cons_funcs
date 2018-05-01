# raster smoothing with negative exponential function 
library(raster)
library(rasterVis)
library(RColorBrewer)

# test landscape
par(mfrow = c(1,1))
tl <- raster(matrix(NA, 10, 10))
tl[1:2, 1:2] <- 1.2
tl[8:9, 8:9] <- 1
tl[7:6, 7:6] <- 2
tl[9, 1:4] <- 1.2
tl <- tl / 2
plot(tl, col = brewer.pal(3, "Blues"))

# smoother visualization
## landscape use 
alpha = 2/1
plot(seq(0,6, length.out = 100), exp(-alpha * seq(0,6, length.out = 100)), type = "l", xlab = "distance (km)", ylab = "value")
abline(h = exp(-alpha), lty = 2)
abline(v = 1, lty = 3)

# smoother fucntion for a cell. Takes in raster (ras), cel number (cell_n), and alpha parameter that scales landscape use
cell_nex_smooth <- function(ras, cell_n, alpha) {
  i_raster <- ras
  i_raster[] <- NA
  i_raster[cell_n] <- 1
  i_dist <- distance(i_raster)
  i_dist_nex <- calc(i_dist, fun = function(x) {exp(-alpha * x)})
  i_val_nex <- overlay(i_dist_nex, ras, fun = "*")
  sum(i_val_nex[], na.rm = T)
  }

# create smoothed raster (This is rather bad way for working with rasters, but I just wanted to be quick)
smoothed <- tl
smoothed[] <- sapply(seq(ncell(tl)), function(x) cell_nex_smooth(tl, x, 0.00007))
smoothed <- smoothed / maxValue(smoothed)

# Additionally create summary statistics for individual patches
just_smooth <- mask(smoothed, tl)
clumps <- clump(tl, directions = 4)
new_vals <- zonal(smoothed, clumps, mean)
cons <- reclassify(clumps, new_vals)

vis_block <- stack(tl, smoothed, just_smooth, cons)
redTheme10 <- rasterTheme(region = brewer.pal(9, "Blues"))
levelplot(vis_block, par.settings = redTheme10, names.attr = c("test landscape", "smoothed landscape", "smoothed patches", "aggreagated patches"))
new_vals

