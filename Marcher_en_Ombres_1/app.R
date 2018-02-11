#
# Application Shiny de démonstration
#

require(shiny)
require(keras)
require(dplyr) # left_join

# Chemin des photos sur le disque local
PATHS_TO_PHOTOS <<- c("~/PHOTOS","photos")

# Variables globales
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
models <- data.frame(model_names, model_width, model_height, model_model)
modelSelected <- ""

#
# Charge un modèle en mémoire et l'ajoute au tableau (data frame?) des modèles
#
load_model <- function(name) {
  print(paste0("load model ", name))
  r <- models[model_names == name,]
  if (is.na(r$model_model)) {
    switch (name,
          resnet50 = { model <- application_resnet50(weights = 'imagenet') },
          vgg16={ model <- application_vgg16(weights = 'imagenet') },
          vgg19={ model <- application_vgg19(weights = 'imagenet') },
          inception_resnet_v2={ model <- application_inception_resnet_v2(weights = 'imagenet') },
          inception_v3={ model <- application_inception_v3(weights = 'imagenet') }
    )
    # On ne peut pas mettre une fonction (type==closure) directement dans un vecteur :)
    # donc on créé une liste
    models$model_model[model_names == name] <<- c(model)
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
  print(paste("loading photo:", image_path))
  image_load(image_path, target_size = c(height, width)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, dim(.))) %>%
    imagenet_preprocess_input()
}

#
# Analyse l'image
#
identify_image <- function(image_path) {
  r <- models[model_names == modelSelected,]
  if (is.na(r$model_model)) {
    print("error: no model (r$model_model is NA)")
    return()
  }
  height <- r$model_height
  width <- r$model_width
  model <- r$model_model[[1]]
  print(paste("identify_image:", modelSelected, height, width, image_path))
  i <- preprocess_image(image_path, height, width)
  if (is.null(i)) return()
  preds <<- model %>% predict(i)
  dpreds <<- imagenet_decode_predictions(preds, top = 3)[[1]] %>%
    left_join(fr_tlb,by="class_name") %>%
    select (-class_description)
  return(dpreds)
}

#
# Page web interactive
#
ui <- fluidPage(
  
  title = "Marcher en Ombre",
  
  titlePanel("Sélectionner des images de paysage"),

  sidebarLayout(
    sidebarPanel(
      selectInput("pathToPhotos", "Chemin vers les photos", PATHS_TO_PHOTOS),
      textInput("imgpath", "Photo", ""),
      actionButton("prevButton", "Préc"),
      actionButton("nextButton", "Suiv"),
      actionButton("randomButton", "Hasard"),
      hr(),
      
      selectInput("modelSelect", "Choix du modèle", models$model_names),
      actionButton("modelButton", "Identifier l'image avec le modèle")
    ),
    
    mainPanel(
      h3(textOutput("feedback_header")),
      tableOutput("dpreds"),
      imageOutput("img", width = "400px", height = "300px"),
      hr(),
      
      htmlOutput("console")
    )
  )
)

#
# Traitement des actions de l'utilisateur
#
server <- function(input, output, session) {
  
  console_text <- reactiveValues()
  
  # permet de choisir le répertoire des photos
  observe({
    input$pathToPhotos
    path_to_photos <<- input$pathToPhotos
    console_text[["log"]] <- capture.output( refresh_photo_list(path_to_photos) )
    updateTextInput(session, "imgpath", value = photos[index])
  })  
  
  # sélectionne la photo précédente
  observe({
    input$prevButton
    if (index == 1) return()
    index <<- index - 1
    ip <- photos[index]
    updateTextInput(session, "imgpath", value = ip)
  })
  
  # sélectionne la photo suivante
  observe({
    input$nextButton
    if (index == nb_photos) return()
    index <<- index + 1
    ip <- photos[index]
    updateTextInput(session, "imgpath", value = ip)
  })
  
  # sélectionne une photo au hasard
  observe({
    input$randomButton
    index <<- sample(1:nb_photos, 1)
    ip <- photos[index]
    updateTextInput(session, "imgpath", value = ip)
  })
  
  # charge le modèle sélectionné
  observe({
    modelSelected <<- input$modelSelect
    console_text[["log"]] <- capture.output( load_model(modelSelected) )
  })
  
  # affiche la photo sélectionnée
  output$img <- renderImage({
    list(src = input$imgpath, width="100%")
  }, deleteFile = FALSE)

  # analyse la photo
  observeEvent(input$modelButton, {
    ip <- photos[index]
    console_text[["log"]] <- capture.output( dpreds <<- identify_image(ip) )
    output$dpreds <- renderTable(dpreds)
  }, ignoreInit = TRUE)
  
  # pour vérifier la sélection actuelle
  output$feedback_header <- renderText({
    paste0("Résultats de l'analyse par le modèle ", input$modelSelect)
  })

  output$console <- renderUI({
    HTML(paste(console_text[["log"]],"<br/>"))
  })
}

#
# Démarrage de l'application Shiny
#
shinyApp(ui, server)
 