# raster smoothing with negative exponential function 
library(raster)

# test landscape
par(mfrow = c(1,1))
tl <- raster(matrix(NA, 10, 10))
tl[1:2, 1:2] <- 1
tl[8:9, 8:9] <- 1
tl[7:6, 7:6] <- 1
tl[9, 1:4] <- 1
plot(tl)

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
smoothed[] <- sapply(seq(ncell(tl)), function(x) cell_nex_smooth(tl, x, 0.0001))

# plot the results
par(mfrow = c(1,2))
plot(tl)
plot(smoothed)


# Additionallly create summary statistics for indiwidual patches
clumps <- clump(tl, directions = 4)
new_vals <- zonal(smoothed, clumps, sum)
cons <- reclassify(clumps, new_vals)

par(mfrow = c(2,2))
plot(tl)
plot(smoothed/maxValue(smoothed))
plot(cons)


