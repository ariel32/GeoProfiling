library(raster)
library(rasterVis)
library(gstat)
library(dismo); library(rgdal)

setwd("E:/work")
d <- read.csv(file = "ServierAnalytics/prestance1010.csv", sep = ";")
cs.location = read.csv("ServierAnalytics/ChemShopData.csv", sep = ";")

data = cbind(cs.location[,5:6], apply(d[,2:length(d)], 1, mean))
names(data) <- c("x", "y", "z")

x.range = seq(from = min(data[,1]), to = max(data[,1]), by = 0.001)
y.range = seq(from = min(data[,2]), to = max(data[,2]), by = 0.001)
g <- gstat(id="log", formula = data$z~1, locations = ~x+y,
           data = data)

#Plot the variogram:
#plot(variogram(g))
#Fit a model variogram to the sample variogram:
v.fit <-   fit.variogram(variogram(g), vgm(0.5,"Sph",1000,0.01))

#plot(variogram(g),v.fit)

grd <- expand.grid(x=x.range, y=y.range)
pr_ok <- krige(id="log",z~1, locations=~x+y, model=v.fit, data=data, newdata=grd)

# convert kriging data into dataframe [x,y,z]
nd = pr_ok[,1:3]
names(nd) <- c("x", "y", "z")
coordinates(nd) <- ~x+y

# rasterizing irregular points
rast <- raster(ncol = 100, nrow = 100)
extent(rast) <- extent(nd)
r = rasterize(nd, rast, nd$z, fun = mean)

#levelplot(r, layers=1, FUN.margin=mean, contour=TRUE)

rproblv <- levelplot(r, margin = FALSE,
                     contour = TRUE,
                     par.settings = rasterTheme(region = matlab.like(n = 10)), 
                     alpha.regions = 0.35)
print(rproblv)


m1 <- gmap(x = extent(r), type = "mobile", zoom = 11)

colors <- m1@legend@colortable
migmaplv <- levelplot(m1, maxpixels = ncell(m1),
                      col.regions = colors, 
                      at = 0:255, panel = panel.levelplot.raster,
                      interpolate = TRUE, colorkey = FALSE, 
                      margin = FALSE)
