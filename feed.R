#
# feed = alimenter le corpus avec des photos
#

require(keras)
preprocess_image <- function(image_path, height, width){
  image_load(image_path, target_size = c(height, width)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, dim(.))) %>%
    imagenet_preprocess_input()
}

#model <- application_vgg16(weights = 'imagenet', include_top = FALSE)
model <- application_resnet50(weights = 'imagenet')

# liste des fichiers
P <- "~/PHOTOS"
f <- list.files(P, pattern = ".*[JjPp][PpNn][Gg]", recursive = TRUE)

# choix d'un fichier
index <- 100

ip <- paste(P, f[index], sep = "/")
# prends une image, identifie son "type", on ne garde que des paysages
# soit on utilise un DNN pré-entrainé, soit on demande à l'utilisateur
# charge l'image et la redimentionne

i <- preprocess_image(ip, 224, 224)

preds <- model %>% predict(i)

imagenet_decode_predictions(preds, top = 3)[[1]]
