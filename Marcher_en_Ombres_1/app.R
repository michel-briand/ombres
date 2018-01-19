require(shiny)
if(!require(keras)){install.packages("keras")
  library(keras)}
 if(!require(dplyr)){install.packages("dplyr")
  library(dplyr)}
load("../imagenet_class_index_fr.Rda")

PATHS_TO_PHOTOS <<- c("~/Images/40ans-cp/","photos")
path_to_photos <- ""
nb_photos <- 0

refresh_photo_list <- function(path) {
  photos <<- list.files(path, pattern = ".*[JjPp][PpNn][Gg]", full.names = TRUE, recursive = TRUE)  
  nb_photos <<- length(photos)
  index <<- 1
  print(photos)
}

preprocess_image_inception <- function(image_path, model){
  system(paste0("jhead -autorot ",image_path)) # need apt-get install jhead or equivalent
         image_load(image_path, target_size = c(height=299, width=299)) %>%
           image_to_array() %>%
           array_reshape(dim = c(1, dim(.))) %>%
           inception_resnet_v2_preprocess_input()
}
preprocess_image_other <- function(image_path, model){
  system(paste0("jhead -autorot ",image_path)) # need apt-get install jhead or equivalent
         image_load(image_path, target_size = c(height=224, width=224)) %>%
           image_to_array() %>%
           array_reshape(dim = c(1, dim(.))) %>%
           imagenet_preprocess_input(mode="tf")
}

identify_image <- function(image_path, model) {
  i <- preprocess_image(image_path , model)
  preds <<- model %>% predict(i)
  dpreds <<- imagenet_decode_predictions(preds, top = 3)[[1]] %>%
    left_join(fr_tlb,by="class_name") %>%
    select (-class_description)

  return(dpreds)
}

models <- c("inception_resnet_v2","resnet50","vgg19","inception_v3") #,"vgg16")
#model_resnet50 <- application_resnet50(classes=1000) # 100 MB download
#model_vgg16 <- application_vgg16(classes=1000) # 550 MB download
model_vgg19 <- application_vgg19(classes=1000) #570 MB download
model_inception_resnet_v2 <- application_inception_resnet_v2(classes = 1000) # 220 MB download
#model_inception_v3 <- application_inception_v3(weights = 'imagenet')
#model <- application_vgg16(weights = 'imagenet', include_top = FALSE)
#model <- application_resnet50(weights = 'imagenet')
model <- application_inception_resnet_v2(classes = 1000)

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
            inception_resnet_v2={ model <<- model_inception_resnet_v2
                                  preprocess_image <<-preprocess_image_inception },
            #resnet50={ model <<- model_resnet50
            #           model$name <<-"resnet50"},
            vgg19={ model <<- model_vgg19
                    preprocess_image <<-preprocess_image_other })
            #            vgg16={ model <<- model_vgg16 
    #               model$name <<-"vgg16"})
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
      dpreds <<- identify_image(ip,model)
      output$dpreds <- renderTable(dpreds)
    }
  })
  
  output$momo <- renderText({
    paste0("Résultats de l'analyse par le modèle ", input$modelSelect)
  })

}

shinyApp(ui, server)
