####### Segmentation

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


######### Histogram Equalization

grayscale(flower1) %>% hist(main="Luminance values in flower1 picture")

R(flower1) %>% hist(main="Red channel values in flower1 picture")

library(ggplot2)
library(dplyr)
bdf <- as.data.frame(flower1)
head(bdf,3)

bdf <- mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

x <- rnorm(100)
layout(t(1:2))
hist(x,main="Histogram of x")
f <- ecdf(x)
hist(f(x),main="Histogram of ecdf(x)")

flower1.g <- grayscale(flower1)
f <- ecdf(flower1.g)
plot(f,main="Empirical CDF of luminance values")

f(flower1.g) %>% hist(main="Transformed luminance values")

f(flower1.g) %>% str

f(flower1.g) %>% as.cimg(dim=dim(flower1.g)) %>% 
  plot(main="With histogram equalisation")

#Hist. equalisation for grayscale
hist.eq <- function(im) as.cimg(ecdf(im)(im),dim=dim(im))

#Split across colour channels,
cn <- imsplit(flower1,"c")
cn #we now have a list of images

cn.eq <- map_il(cn,hist.eq) #run hist.eq on each
imappend(cn.eq,"c") %>% plot(main="All channels equalised") #recombine and plot

library(purrr)
#Convert to HSV, reduce saturation, convert back
RGBtoHSV(flower1) %>% imsplit("c") %>%
  modify_at(2,~ . / 2) %>% imappend("c") %>%
  HSVtoRGB %>% plot(rescale=FALSE)

#Turn into a function
desat <- function(im) RGBtoHSV(im) %>% imsplit("c") %>%
  modify_at(2,~ . / 2) %>% imappend("c") %>%
  HSVtoRGB

#Split image into 3 blocks, reduce saturation in middle block, recombine
imsplit(flower1,"x",3) %>% modify_at(2,desat) %>%
  imappend("x") %>% plot(rescale=FALSE)

########## Morphological Operations
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