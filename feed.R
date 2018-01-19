#
# feed = alimenter le corpus avec des photos
#
if(!require(keras)){install.packages("keras")
  library(keras)}
if(!require(magick)){install.packages("magick")
  library(magick)}
 if(!require(dplyr)){install.packages("dplyr")
  library(dplyr)}
load("~/.keras/models/imagenet_class_index_fr.Rda")
preprocess_image <- function(image_path, height=299, width=299){
  system(paste0("jhead -autorot ",image_path))
  image_load(image_path, target_size = c(height, width)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, dim(.))) %>%
    #imagenet_preprocess_input(mode="tf")
    inception_resnet_v2_preprocess_input
}

#model <- application_vgg16(weights = 'imagenet', include_top = FALSE)
#model <- application_resnet50(weights = 'imagenet')
model <- application_inception_resnet_v2(classes = 1000)

# liste des fichiers
P <- "~/Images/40ans-cp"
f <- list.files(P, pattern = ".*[JjPp][PpNn][Gg]", recursive = TRUE)

# choix d'un fichier
ip <- paste0(P,"/", sample(f,1))
# prends une image, identifie son "type", on ne garde que des paysages
# soit on utilise un DNN pré-entrainé, soit on demande à l'utilisateur
# charge l'image et la redimentionne
preds <- model %>% predict(preprocess_image(ip))
#imagenet_decode_predictions(preds, top = 3)[[1]]
imagenet_decode_predictions(preds, top = 3)[[1]] %>% left_join(fr_tlb,by="class_name") %>% select (-class_description)
