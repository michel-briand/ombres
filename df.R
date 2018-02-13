# apprendre à manipuler les data frame

model_names <- c("resnet50", "vgg16","vgg19","inception_resnet_v2","inception_v3")
model_width <- c( 224, 224, 224, 299, 299)
model_height <- c( 224, 224, 224, 299, 299)
model_model <- c(NA, NA, NA, NA, NA)
df <- data.frame(model_names, model_width, model_height, model_model)

# apprendre à redresser une image couchée
# http://sylvana.net/jpegcrop/exif_orientation.html
photo_debout <- "/home/michel/PHOTOS/Charlotte/ur/famille/Les loulous/2014/fevrier/20140222_201352.jpg"
photo_couchee <- "/home/michel/PHOTOS/Charlotte/ur/famille/Les loulous/2014/aout/20140822_161707.jpg"
library(exifr)
system.time( ec <- read_exif(photo_couchee) )
print(ec["Orientation"])
ed <- read_exif(photo_debout)
print(ed["Orientation"])

library(imager)
ir <- ic <- load.image(photo_couchee)
#plot(ic)

o <- ec["Orientation"][[1]]
switch(o,
  { print("1: orientation normale F") },
  { print("2: orientation mirroir vertical F regardant vers la gauche") },
  { print("3: orientation mirroir vertical et horizontal F tête en bas regardant vers la gauche") },
  { print("4: orientation mirroir vertical F tête en bas") },
  { print("5: orientation mirroir vertical couchée F pointe à gauche regardant vers le bas") },
  { print("6: orientation couchée F regardant vers le haut")
    ir <- imrotate(ic, angle = 90) },
  { print("7: orientation miroir vertical couchée F pointe à droite regardant vers le haut") },
  { print("8: orientation couchée F regardant vers le bas")}
)

#ir <- imrotate(ic)
plot(ir)
dim(ic)

library(jpeg)
jc <- readJPEG(photo_couchee)
plot(1:2, type='n')
rasterImage(jc, 1, 1.25, 1.1, 1)

library(magick)
str(magick::magick_config())
ic_ <- image_read(photo_couchee)
image_info(ic_)
plot(ic_)
image_rotate(ic_, 90) %>% plot()

library(imager)
library(purrr)
parrots <- load.example("parrots")
plot(parrots)
#Define a function that converts to YUV, blurs a specific channel, and converts back
bchan <- function(im,ind,sigma=5) { 
  im <- RGBtoYUV(im)
  channel(im,ind) <- isoblur(channel(im,ind),sigma); 
  YUVtoRGB(im)
}
#Run the function on all three channels and collect the results as a list
blurred <- map_il(1:3,~ bchan(parrots,.))
names(blurred) <- c("Luminance blur (Y)","Chrominance blur (U)","Chrominance blur (V)")
plot(blurred)


##The tidyverse package loads dplyr, purrr, etc. 
library(tidyverse)
library(imager)
##Optional: cowplot has nicer defaults for ggplot
library(cowplot) 
