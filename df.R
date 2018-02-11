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
ec <- read_exif(photo_couchee)
print(ec["Orientation"])
ed <- read_exif(photo_debout)
print(ed["Orientation"])

library(imager)
ic <- load.image(photo_couchee)
plot(im)

o <- ec["Orientation"][[1]]
switch(o,
  { print("1: orientation normale F") },
  { print("2: orientation mirroir vertical F regardant vers la gauche") },
  { print("3: orientation mirroir vertical et horizontal F tête en bas regardant vers la gauche") },
  { print("4: orientation mirroir vertical F tête en bas") },
  { print("5: orientation mirroir vertical couchée F pointe à gauche regardant vers le bas") },
  { print("6: orientation couchée F regardant vers le haut")
    ir <- imrotate(ic) },
  { print("7: orientation miroir vertical couchée F pointe à droite regardant vers le haut") },
  { print("8: orientation couchée F regardant vers le bas")}
)

plot(ir)
