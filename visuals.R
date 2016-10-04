library(scatterplot3d)
x <- rnorm(100)
y <- jitter(1:100)
z <- sample(1:1000, 100)

scatterplot3d(x = x,
              y = y,
              z = z)

library(rgl)
plot3d(x, y, z,col="red", size=3)
scatter3d(x, y, z)
