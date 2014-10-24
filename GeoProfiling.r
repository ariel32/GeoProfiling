library(raster)
library(rasterVis)
library(gstat)
library(dismo)
library(rgdal)
library(colorRamps)

setwd("E:/work")
d <- read.csv(file = "ServierAnalytics/prestance105.csv", sep = ";")
cs.location = read.csv("ServierAnalytics/ChemShopData.csv", sep = ";")
#t.parameter = apply(d[,2:length(d)], 1, mean)
t.parameter = apply(d[,2:length(d)], 1, sum)

# data binding and naming
data = cbind(cs.location[,5:6], t.parameter)
names(data) <- c("x", "y", "z")

x.range = seq(from = min(data$x-0.1), to = max(data$x+0.1), by = 0.001)
y.range = seq(from = min(data$y-0.1), to = max(data$y+0.1), by = 0.001)
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
rast <- raster(ncol = 300, nrow = 300)
extent(rast) <- extent(nd)
r = rasterize(nd, rast, nd$z, fun = mean)

#levelplot(r, layers=1, FUN.margin=mean, contour=TRUE)

# map with alpha channel and contours
r2 <- levelplot(r, margin = FALSE,
                contour = TRUE,
                par.settings = rasterTheme(region = matlab.like(n = 10)), 
                alpha.regions = 0.35)
#print(r2)

# native map
m1 <- gmap(x = extent(r), type = "roadmap", zoom = 11)#, crs = CRS("+init=epsg:3857"))

# raster's projection
r.pr <- projectRaster(from = r, crs = CRS("+init=epsg:3857"))

# plot(m1)
# plot(r.pr, add = T, legend = F, col = rev(rainbow(10, alpha = 0.35)))

micolor <- rev(rainbow(12, alpha = 0.35))
transp <- rainbow(12, alpha = 0)
micolor[1:3] <- transp[1]

plot(m1)
plot(r.pr, add = T, legend = F, col = micolor)
