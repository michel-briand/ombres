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
models <- c("resnet50", "vgg16","vgg19","inception_resnet_v2","inception_v3")
model_resnet50 <- application_resnet50(weights = 'imagenet')
model_vgg16 <- application_vgg16(weights = 'imagenet')
model_vgg19 <- application_vgg19(weights = 'imagenet')
model_inception_resnet_v2 <- application_inception_resnet_v2(weights = 'imagenet')
model_inception_v3 <- application_inception_v3(weights = 'imagenet')
model <- model_resnet50

#
# Charge un modèle en mémoire et l'ajoute au tableau (data frame?) des modèles
#
load_model <- function(name) {

}

# Traduction de classes d'images en français,
# (créé une variable globale 'fr_tlb' qui contient les noms
# de classe d'image en français)
load("imagenet_class_index_fr.Rda")

#
# Construit la liste des photos, met à jour nb_photos et index
#
refresh_photo_list <- function(path) {
  photos <<- list.files(path, pattern = ".*[JjPp][PpNn][Gg]", full.names = TRUE, recursive = TRUE)  
  nb_photos <<- length(photos)
  index <<- 1
}

#
# Charge une image et la prépare pour analyse
#
preprocess_image <- function(image_path, height, width){
  if (!file.exists(image_path)) return(NULL)
  image_load(image_path, target_size = c(height, width)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, dim(.))) %>%
    imagenet_preprocess_input()
}

#
# Analyse l'image
#
identify_image <- function(image_path) {
  height <- 224
  width <- 224
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
      
      selectInput("modelSelect", "Choix du modèle", models),
      actionButton("modelButton", "Identifier l'image avec le modèle")
    ),
    
    mainPanel(
      h3(textOutput("feedback_header")),
      tableOutput("dpreds"),
      imageOutput("img", width = "400px", height = "300px")
    )
  )
)

#
# Traitement des actions de l'utilisateur
#
server <- function(input, output, session) {
  
  observe({
    input$pathToPhotos
    path_to_photos <<- input$pathToPhotos
    refresh_photo_list(path_to_photos)
    updateTextInput(session, "imgpath", value = photos[index])
  })  
  
  observe({
    input$prevButton
    if (index == 1) return()
    index <<- index - 1
    ip <- photos[index]
    updateTextInput(session, "imgpath", value = ip)
  })
  
  observe({
    input$nextButton
    if (index == nb_photos) return()
    index <<- index + 1
    ip <- photos[index]
    updateTextInput(session, "imgpath", value = ip)
  })
  
  observe({
    input$randomButton
    index <<- sample(1:nb_photos, 1)
    ip <- photos[index]
    updateTextInput(session, "imgpath", value = ip)
  })
  
  observe({
    modelSelected <<- input$modelSelect
    switch (modelSelected,
            resnet50={ model <<- model_resnet50 },
            vgg16={ model <<- model_vgg16 },
            vgg19={ model <<- model_vgg19 },
            inception_resnet_v2={ model <<- model_inception_resnet_v2 },
            inception_v3={ model <<- model_inception_v3 }
            )
  })
  
  output$img <- renderImage({
    list(src = input$imgpath, width="100%")
  }, deleteFile = FALSE)

  observeEvent(input$modelButton, {
    ip <- photos[index]
    dpreds <<- identify_image(ip)
    output$dpreds <- renderTable(dpreds)
  }, ignoreInit = TRUE)
  
  #
  # Essentiellement pour vérifier la sélection actuelle
  #
  output$feedback_header <- renderText({
    paste0("Résultats de l'analyse par le modèle ", input$modelSelect)
  })

}

#
# Démarrage de l'application Shiny
#
shinyApp(ui, server)
