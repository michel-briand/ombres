#
# L'image attendue par le modèle est petite (224x224 ou 299x299).
#
# Le but de ce script est de tester comment on peut aller au délà.
#
# Dans un premier temps :
# - faire agir le modèle sur des sous-parties de l'image
#   - en utilisant un pavement multiple de la dimension attendue
#   - en fenêtre glissante
# Dans un second temps (à faire) :
# - modifier le modèle ?
#   - rajouter des couches en amont ?
#   - utiliser le modèle ou un autre modèle 
#     pour découvrir des sous-parties qui nous intéressent
#

require(shiny)
require(keras)
require(dplyr) # left_join

# Chemin des photos sur le disque local
PATHS_TO_PHOTOS <<- c("~/PHOTOS","photos")

# Variables globales
log_file <- "app.log"
#LOG# sink(log_file, append=TRUE, split=TRUE) 

path_to_photos <- ""
photos <- c()
nb_photos <- length(photos)
index <- 1

# Quelques modèles de réseaux de neurones profonds pour la reconnaissance d'images
#
# resnet50 : le plus léger et le plus efficace (224x224)
#
model_names <- c("resnet50", "vgg16","vgg19","inception_resnet_v2","inception_v3")
model_width <- c( 224, 224, 224, 299, 299)
model_height <- c( 224, 224, 224, 299, 299)
model_model <- c(NA, NA, NA, NA, NA)
model_preprocess <- c(NA, NA, NA, NA, NA)
models <- data.frame(model_names, model_width, model_height, model_model, model_preprocess)
modelSelected <- ""

#
# Charge un modèle en mémoire et l'ajoute au tableau (data frame?) des modèles
#
load_model <- function(name) {
  print(paste0("load model ", name))
  r <- models[model_names == name,]
  if (is.na(r$model_model)) {
    switch (name,
            resnet50 = {
              model <- application_resnet50(weights = 'imagenet')
              preprocess = preprocess_image
            },
            vgg16 = {
              model <- application_vgg16(weights = 'imagenet')
              preprocess = preprocess_image
            },
            vgg19 = {
              model <- application_vgg19(weights = 'imagenet')
              preprocess = preprocess_image
            },
            inception_resnet_v2 = {
              model <- application_inception_resnet_v2(weights = 'imagenet')
              preprocess = preprocess_image_inception_v2
            },
            inception_v3 = {
              model <- application_inception_v3(weights = 'imagenet')
              preprocess = preprocess_image_inception_v3
            }
    )
    # On ne peut pas mettre une fonction (type==closure) directement dans un vecteur :)
    # donc on créé une liste
    models$model_model[model_names == name] <<- c(model)
    models$model_preprocess[model_names == name] <<- c(preprocess)
    # pour accéder au modèle on utilisera:
    m <- models$model_model[model_names == name][[1]]
    # pour vérifier l'objet on fait
    print(typeof(m))
    # [1] "closure"
  }
}

# Traduction de classes d'images en français,
# (créé une variable globale 'fr_tlb' qui contient les noms
# de classe d'image en français)
load("imagenet_class_index_fr.Rda")

#
# Construit la liste des photos, met à jour nb_photos et index
#
refresh_photo_list <- function(path) {
  print(paste("path to photos:", path))
  photos <<- list.files(path, pattern = ".*[JjPp][PpNn][Gg]", full.names = TRUE, recursive = TRUE)  
  nb_photos <<- length(photos)
  print(paste(nb_photos,"photos in list"))
  index <<- 1
}

#
# Charge une image et la prépare pour analyse
#
preprocess_image <- function(image_path, height, width){
  if (!file.exists(image_path)) {
    print(paste("file does not exist:", image_path))
    return(NULL)
  }
  print(paste("load photo & preprocess imagenet:", image_path))
  image_load(image_path, target_size = c(height, width)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, dim(.))) %>%
    imagenet_preprocess_input()
}

preprocess_image_inception_v2 <- function(image_path, height, width){
  if (!file.exists(image_path)) {
    print(paste("file does not exist:", image_path))
    return(NULL)
  }
  print(paste("load photo & preprocess inception v2:", image_path))
  image_load(image_path, target_size = c(height, width)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, dim(.))) %>%
    inception_resnet_v2_preprocess_input()
}

preprocess_image_inception_v3 <- function(image_path, height, width){
  if (!file.exists(image_path)) {
    print(paste("file does not exist:", image_path))
    return(NULL)
  }
  print(paste("load photo & preprocess inception v3:", image_path))
  image_load(image_path, target_size = c(height, width)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, dim(.))) %>%
    inception_v3_preprocess_input()
}

#
# Analyse l'image
#
identify_image <- function(image_path) {
  r <- models[model_names == modelSelected,]
  if (is.na(r$model_model)) {
    print("error: no model object (r$model_model is NA)")
    return()
  }
  if (is.na(r$model_preprocess)) {
    print("error: no proprocess function (r$model_preprocess is NA)")
    return()
  }
  height <- r$model_height
  width <- r$model_width
  model <- r$model_model[[1]]
  preprocess <- r$model_preprocess[[1]]
  print(paste("identify_image:", modelSelected, height, width, image_path))
  i <- preprocess(image_path, height, width)
  if (is.null(i)) return()
  preds <<- model %>% predict(i)
  dpreds <<- imagenet_decode_predictions(preds, top = 3)[[1]] %>%
    left_join(fr_tlb,by="class_name") %>%
    select (-class_description)
  return(dpreds)
}

random_image <- function() {
  index <<- sample(1:nb_photos, 1)
  image_path <<- photos[index]
  print(paste("random image:", index, image_path))
}

# Charge modèle
modelSelected <- "resnet50"
load_model(modelSelected)
r <- models[model_names == modelSelected,]
model <- r$model_model[[1]]

# initialisation
path_to_photos <- PATHS_TO_PHOTOS[[1]]
refresh_photo_list(path_to_photos)
random_image()
image_path <- "/home/michel/PHOTOS/michel/Zu/2016/IMAG0547.jpg"

# charge l'image
m <- image_load(image_path) %>% image_to_array()
dim(m)

# Extract sub-images
L <- floor( dim(m)[[1]] / r$model_width )
H <- floor( dim(m)[[2]] / r$model_height )
collec_dpreds <- c()

for (i in 1:L) {
  x1 <- r$model_width*(i-1)+1
  x2 <- r$model_width*(i)
  for (j in 1:H) {
    y1 <- r$model_height*(j-1)+1
    y2 <- r$model_height*(j)
    # FORMULE MAGIQUE: seq pour l'extraction d'un rectangle dans une matrice
    mi <- m[seq(x1,x2,1),seq(y1,y2,1), 1:3]
    #print(dim(mi))
    mi_r <- array_reshape(mi, dim = c(1, dim(mi)))
    #print(dim(mi_r))
    mi_rp <- imagenet_preprocess_input(mi_r)
    #print(dim(mi_rp))
    print(paste("x1=",x1,"x2=",x2,"y1=",y1,"y2=",y2))
    preds <<- model %>% predict(mi_rp)
    dpreds <<- imagenet_decode_predictions(preds, top = 1)[[1]]
    #print(dpreds)
    collec_dpreds <<- rbind(collec_dpreds, dpreds) #imagenet_decode_predictions(preds, top = 1)[[1]])
  }
}

# affiche le tableau des résultats trié par score
collec_dpreds[order(collec_dpreds$score),] %>% select(class_description,score)
# en anglais et en français
collec_dpreds[order(collec_dpreds$score),] %>% left_join(fr_tlb,by="class_name") %>% select(-class_name)
