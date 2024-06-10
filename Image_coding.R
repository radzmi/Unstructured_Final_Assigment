# Load all library required
library(imager)

# Load Image
# Flower Image
flower1 <- load.image("/Users/PC 8/Documents/Image/flower1.jpg")
plot(flower1)

flower2 <- load.image("/Users/PC 8/Documents/Image/flower2.jpg")
plot(flower2)

# Scenery Image
scenery1 <- load.image("/Users/PC 8/Documents/Image/scenery1.jpg")
plot(scenery1)

scenery2 <- load.image("/Users/PC 8/Documents/Image/scenery2.jpg")
plot(scenery2)

# Person Image
person1 <- load.image("/Users/PC 8/Documents/Image/person1.jpg")
plot(person1)

person2 <- load.image("/Users/PC 8/Documents/Image/person2.jpg")
plot(person2)

# Building Image
building1 <- load.image("/Users/PC 8/Documents/Image/building1.jpg")
plot(building1)

building2 <- load.image("/Users/PC 8/Documents/Image/building2.jpg")
plot(building2)

# Other Image
cat1 <- load.image("/Users/PC 8/Documents/Image/cat1.jpg")
plot(cat1)

cat2 <- load.image("/Users/PC 8/Documents/Image/cat2.jpg")
plot(cat2)

# Edge Detection
# flower image
flower1.xedges <- deriche(flower1,2,order=2,axis="x") #Edge detector along x-axis
flower1.yedges <- deriche(flower1,2,order=2,axis="y") #Edge detector along y-axis
plot(flower1.xedges)
plot(flower1.yedges)

flower2.xedges <- deriche(flower2,2,order=2,axis="x") #Edge detector along x-axis
flower2.yedges <- deriche(flower2,2,order=2,axis="y") #Edge detector along y-axis
plot(flower2.xedges)
plot(flower2.yedges)

# Scenery Image
scenery1.xedges <- deriche(scenery1,2,order=2,axis="x") #Edge detector along x-axis
scenery1.yedges <- deriche(scenery1,2,order=2,axis="y") #Edge detector along y-axis
plot(scenery1.xedges)
plot(scenery1.yedges)

scenery2.xedges <- deriche(scenery2,2,order=2,axis="x") #Edge detector along x-axis
scenery2.yedges <- deriche(scenery2,2,order=2,axis="y") #Edge detector along y-axis
plot(scenery2.xedges)
plot(scenery2.yedges)

# Person Image
person1.xedges <- deriche(person1,2,order=2,axis="x") #Edge detector along x-axis
person1.yedges <- deriche(person1,2,order=2,axis="y") #Edge detector along y-axis
plot(person1.xedges)
plot(person1.yedges)

person2.xedges <- deriche(person2,2,order=2,axis="x") #Edge detector along x-axis
person2.yedges <- deriche(person2,2,order=2,axis="y") #Edge detector along y-axis
plot(person2.xedges)
plot(person2.yedges)

# Building Image
building1.xedges <- deriche(building1,2,order=2,axis="x") #Edge detector along x-axis
building1.yedges <- deriche(building1,2,order=2,axis="y") #Edge detector along y-axis
plot(building1.xedges)
plot(building1.yedges)

building2.xedges <- deriche(building2,2,order=2,axis="x") #Edge detector along x-axis
building2.yedges <- deriche(building2,2,order=2,axis="y") #Edge detector along y-axis
plot(building2.xedges)
plot(building2.yedges)

# Cat Image
cat1.xedges <- deriche(cat1,2,order=2,axis="x") #Edge detector along x-axis
cat1.yedges <- deriche(cat1,2,order=2,axis="y") #Edge detector along y-axis
plot(cat1.xedges)
plot(cat1.yedges)

cat2.xedges <- deriche(cat2,2,order=2,axis="x") #Edge detector along x-axis
cat2.yedges <- deriche(cat2,2,order=2,axis="y") #Edge detector along y-axis
plot(cat2.xedges)
plot(cat2.yedges)

# Segmentation

im <- load.example("coins")
plot(im)


threshold(flower2) %>% plot

# Performing Linear model
library(dplyr)

d <- as.data.frame(flower2)

## Subsamble, fit a linear model
m <- sample_n(d,1e4) %>% lm(value ~ x*y,data=.)

## Correct by removing the trend
flower2 <- flower2-predict(m,d)
out <- threshold(flower2)
plot(out)

# Cleaning
out <- clean(out,3) %>% imager::fill(7)
plot(flower2)
highlight(out)

# watershed
bg <- (!threshold(flower2,"10%"))
fg <- (threshold(flower2,"90%"))
imlist(fg,bg) %>% plot(layout="row")

#Build a seed image where fg pixels have value 2, bg 1, and the rest are 0
seed <- bg+2*fg
plot(seed)

edges <- imgradient(flower2,"xy") %>% enorm
p <- 1/(1+edges)
plot(p)

ws <- (watershed(seed,p)==1)
plot(ws)

ws <- bucketfill(ws,1,1,color=2) %>% {!( . == 2) }
plot(ws)

clean(ws,5) %>% plot

split_connected(ws) %>% purrr::discard(~ sum(.) < 100) %>%
  parany %>% plot

# Plot
layout(t(1:2))
plot(im,main="Thresholding")
highlight(out)

plot(im,main="Watershed")
out2 <- clean(ws,5)
highlight(out2,col="green")


# Histogram Equalization









































# Morphological Operations
# Change colour to grayscale
flower1 <- grayscale(flower1)
plot(flower1)

# Try to perform linear regression
df <- as.data.frame(flower1)
head(df,5) 

m <- lm(value ~ x + y,data=df) #linear trend
summary(m)

# Plotting before and after trend removal
layout(t(1:2))
flower1_morpho <- flower1-fitted(m)
plot(flower1,main="Before")
plot(flower1_morpho,main="After trend removal")

paste0(c(80,60,40),"%") %>% 
  map_il(~ threshold(flower1_morpho,.)) %>% 
  plot(layout="row") #80,60,40 can be changed based on picture

# change the picture to pixset 
flowerpixset <- threshold(flower1_morpho,"60%")
px <- as.pixset(1-flowerpixset) 
plot(px)

# Grow picture by 3 pixels
grow(px,3) %>% plot(main="Growing by 3 pixels")

# Shrink picture by 3 pixels
shrink(px,3) %>% plot(main="Shrinking by 3 pixels")

# Plot
layout(t(1:2))
plot(px,main="Original")
shrink(px,3) %>% grow(3) %>% plot(main="Shrink, then grow")

layout(t(1:2))
plot(px,main="Original")
grow(px,3) %>% shrink(3) %>% plot(main="Grow, then shrink")

plot(flower1)
fill(px,5) %>% clean(3) %>% highlight

### Grayscale picture
# As the picture already in gray colour, will proceed with linear regression
# Try to perform linear regression
df2 <- as.data.frame(flower2)
head(df2,5) 

m <- lm(value ~ x + y,data=df2) #linear trend
summary(m)

# Plotting before and after trend removal
layout(t(1:2))
flower2_morpho <- flower2-fitted(m)
plot(flower2,main="Before")
plot(flower2_morpho,main="After trend removal")

paste0(c(70,50,30),"%") %>% 
  map_il(~ threshold(flower2_morpho,.)) %>% 
  plot(layout="row") #80,60,40 can be changed based on picture

# change the picture to pixset 
flowerpixset2 <- threshold(flower2_morpho,"45%")
px2 <- as.pixset(1-flowerpixset2) 
plot(px2)

# Grow picture by 3 pixels
grow(px2,3) %>% plot(main="Growing by 3 pixels")

# Shrink picture by 3 pixels
shrink(px2,3) %>% plot(main="Shrinking by 3 pixels")

# Plot
layout(t(1:2))
plot(px2,main="Original")
shrink(px2,3) %>% grow(3) %>% plot(main="Shrink, then grow")

layout(t(1:2))
plot(px2,main="Original")
grow(px2,3) %>% shrink(3) %>% plot(main="Grow, then shrink")

plot(flower2)
fill(px2,5) %>% clean(3) %>% highlight


