# Zonation imitator
library(raster)

suma = function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)

z_priority <- function(features, z, g) {
  
  rr <- as.array(features)
  nr <- rr
  nr[] <- NA
  pr <- rr
  pr[] <- NA
  
  out_ras <- pr[ , ,1]
  rem_ord <- matrix(NA, ncell(out_ras), 2)

  zs <- vector()
  
  for (i in 1:ncell(pr)){
    for (j in 1:dim(rr)[3]) {
      nr[,,j] <- rr[,,j] / sum(rr[,,j], na.rm = T)
      pr[,,j] <- z * sum(rr[,,j], na.rm = T)^(1 - z) * nr[,,j]
    }
    
    V <- apply(pr, c(1, 2), function(x) suma(x^g)^(1/g))
    
    w_mat <- which(V == min(V, na.rm = T), arr.ind = T)
    rem_ord[i, ] <- w_mat[sample(nrow(w_mat), size = 1), ]
    rr[rem_ord[i, 1], rem_ord[i, 2], ] <- NA
    
    out_ras[rem_ord[i, 1], rem_ord[i, 2]] <- i 
  }
  raster(out_ras)
}

r1 <- matrix(runif(100), 10, 10)
r2 <- matrix(runif(100), 10, 10)
r2[1:50] <- NA
r2[90:100] <- 2
r3 <- matrix(NA, 10, 10)
r3[1:5] <- 1
r4 <- matrix(NA, 10, 10)
r4[90:100] <- 1
r5 <- matrix(NA, 10, 10)
r5[90:100] <- 1
r6 <- matrix(NA, 10, 10)
r6[90:100] <- 1

feat <- stack(raster(r1), raster(r2), raster(r3), raster(r4), raster(r5), raster(r6))

zs1 <- z_priority(feat, 0.25, g = 50)

