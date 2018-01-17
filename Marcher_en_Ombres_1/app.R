require(shiny)
require(keras)

PATHS_TO_PHOTOS <<- c("~/PHOTOS","photos")
path_to_photos <- ""
nb_photos <- 0

refresh_photo_list <- function(path) {
  photos <<- list.files(path, pattern = ".*[JjPp][PpNn][Gg]", full.names = TRUE, recursive = TRUE)  
  nb_photos <<- length(photos)
  index <<- 1
  print(photos)
}

preprocess_image <- function(image_path, height, width){
  image_load(image_path, target_size = c(height, width)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, dim(.))) %>%
    imagenet_preprocess_input()
}

identify_image <- function(image_path) {
  i <- preprocess_image(image_path, 224, 224)
  preds <<- model %>% predict(i)
  dpreds <<- imagenet_decode_predictions(preds, top = 3)[[1]]
  return(dpreds)
}

# Quelques modèles
# models <- c("resnet50", "vgg16","vgg19","inception_resnet_v2","inception_v3")

models <- c("resnet50", "vgg16","vgg19","inception_resnet_v2","inception_v3")

model_resnet50 <- application_resnet50(weights = 'imagenet')
model_vgg16 <- application_vgg16(weights = 'imagenet')
model_vgg19 <- application_vgg19(weights = 'imagenet')
model_inception_resnet_v2 <- application_inception_resnet_v2(weights = 'imagenet')
model_inception_v3 <- application_inception_v3(weights = 'imagenet')
model <- model_resnet50

index <- 1
#ip <- photos[index]
#print(paste("Image path:", ip))
#dpreds <- identify_image(ip)

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
      
      selectInput("modelSelect", "Choix du modèle",
                  models),
      actionButton("modelButton", "Identifier l'image avec le modèle")
    ),
    
    mainPanel(
      h3(textOutput("momo")),
      tableOutput("dpreds"),
      imageOutput("img", width = "400px", height = "300px")
    )
  )
)



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

  observe({
    input$modelButton
    ip <- photos[index]
    if (is.null(ip) || length(ip) == 0) return()
    if (!file.exists(ip)) return()
    print(ip)
    if (!is.null(ip) && length(ip) > 0) {
      dpreds <<- identify_image(ip)
      output$dpreds <- renderTable(dpreds)
    }
  })
  
  output$momo <- renderText({
    paste0("Résultats de l'analyse par le modèle ", input$modelSelect)
  })

}

shinyApp(ui, server)
