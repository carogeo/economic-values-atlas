#' map_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_selections_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shinyWidgets::pickerInput(ns("jobsInput"),
                              label = shiny::HTML(paste0("<h3>Job activity</h3>")), 
                              choices = filter(eva_vars, type == "jobs")$name, 
                              options = list(`actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 1"), 
                              multiple = T,
                              selected = filter(eva_vars, type == "jobs")$name),
    hr(),
    shinyWidgets::pickerInput(ns("marketInput"),
                              label = shiny::HTML("<h3>Neighborhood & Market Health</h3>"), 
                              choices=filter(eva_vars, type == "market")$name, 
                              options = list(`actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 1"), 
                              multiple = T,
                              selected = filter(eva_vars, type == "market")$name),
    
    hr(),
    shinyWidgets::pickerInput(ns("inclusivityInput"),
                              label = shiny::HTML("<h3>Inclusivity</h3>"), 
                              choices=filter(eva_vars, type == "inclusivity")$name, 
                              options = list(`actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 1"),
                              multiple = T,
                              selected = filter(eva_vars, type == "inclusivity")$name),
    
    hr(),
#    shinyWidgets::pickerInput(ns("developabilityInput"),
#                              label = shiny::HTML("<h3>Developability</h3>"), 
#                              choices=filter(eva_vars, type == "developability")$name, 
#                              options = list(`actions-box` = TRUE, 
#                                             size = 10,
#                                             `selected-text-format` = "count > 1"),
#                              multiple = T,
#                              selected = filter(eva_vars, type == "developability")$name),
#    
#    hr(),
    shinyWidgets::pickerInput(ns("realestateInput"),
                              label = shiny::HTML("<h3>Real Estate & Commercial Vitality</h3>"), 
                              choices=filter(eva_vars, type == "realestate")$name, 
                              options = list(`actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 1"),
                              multiple = T,
                              selected = filter(eva_vars, type == "realestate")$name),
    
#    hr(),
#    actionButton(ns("goButton"), "Update map", class = "btn-success"),
    
    # shiny::h4("Selected variables"),
    # textOutput(ns("selectedvars0")), #if want to print variables on shiny this works
    
    # textOutput(ns("selectedvars25"))
    

  )
}
    
#' map_selections Server Function
#'
#' @noRd 
mod_map_selections_server <- function(input, output, session){
  ns <- session$ns
  
  #uncomment if want to print variables included
  # output$selectedvars0 <- renderText({
  #   input$goButton
  #   a <- isolate(input$jobsInput)
  #   b <- isolate(input$marketInput)
  #   c <- isolate(input$inclusivityInput)
  #   d <- isolate(input$developabilityInput)
  #   e <- isolate(input$realestateInput)
  #   toprint <- paste(a, b, c, d, e, sep = "; ")
  #   toprint
  #   })
  
  # output$selectedvars25 <- renderText(input$peopleInput %>% rbind(input$placeInput))
  
  input_values <- reactiveValues() # start with an empty reactiveValues object.
  
#  observeEvent(input$goButton,{
#      input$goButton
#    
#    input_values$jobsInput <- input$jobsInput
#    input_values$marketInput <- input$marketInput
#    input_values$inclusivityInput <- input$inclusivityInput
#    input_values$developabilityInput <- input$developabilityInput
#    input_values$realestateInput <- input$realestateInput
    
#    input_values$allInputs <- as_tibble(input$jobsInput) %>%
#      rbind(as_tibble(input$marketInput)) %>%
#      rbind(as_tibble(input$inclusivityInput)) %>%
#      rbind(as_tibble(input$developabilityInput)) %>%
#      rbind(as_tibble(input$realestateInput)) 
#  }, ignoreNULL = FALSE)
  
  
   observeEvent(input$jobsInput, { # only update when the user changes the eva input
     input_values$jobsInput <- input$jobsInput # create/update the eva input value in our reactiveValues object
     input_values$allInputs <- as_tibble(input$jobsInput) %>%
       rbind(as_tibble(input$marketInput)) %>%
       rbind(as_tibble(input$inclusivityInput)) %>%
       #       rbind(as_tibble(input$developabilityInput)) %>%
       rbind(as_tibble(input$realestateInput)) 
   }, ignoreNULL = FALSE)
   
   observeEvent(input$marketInput, { # only update when the user changes the eva input
     input_values$marketInput <- input$marketInput # create/update the eva input value in our reactiveValues object
     input_values$allInputs <- as_tibble(input$jobsInput) %>%
       rbind(as_tibble(input$marketInput)) %>%
       rbind(as_tibble(input$inclusivityInput)) %>%
       #       rbind(as_tibble(input$developabilityInput)) %>%
       rbind(as_tibble(input$realestateInput)) 
   }, ignoreNULL = FALSE)
   
   observeEvent(input$inclusivityInput, { # only update when the user changes the eva input
     input_values$inclusivityInput <- input$inclusivityInput # create/update the eva input value in our reactiveValues object
     input_values$allInputs <- as_tibble(input$jobsInput) %>%
       rbind(as_tibble(input$marketInput)) %>%
       rbind(as_tibble(input$inclusivityInput)) %>%
       #       rbind(as_tibble(input$developabilityInput)) %>%
       rbind(as_tibble(input$realestateInput)) 
   }, ignoreNULL = FALSE)
  
  # observeEvent(input$developabilityInput, { # only update when the user changes the eva input
  #   input_values$developabilityInput <- input$developabilityInput # create/update the eva input value in our reactiveValues object
  # })
  
   observeEvent(input$realestateInput, { # only update when the user changes the eva input
     input_values$realestateInput <- input$realestateInput # create/update the eva input value in our reactiveValues object

     input_values$allInputs <- as_tibble(input$jobsInput) %>%
       rbind(as_tibble(input$marketInput)) %>%
       rbind(as_tibble(input$inclusivityInput)) %>%
       rbind(as_tibble(input$developabilityInput)) %>%
       rbind(as_tibble(input$realestateInput)) 
   }, ignoreNULL = FALSE)
  
  return(input_values)
  
}
    
## To be copied in the UI
# mod_map_selections_ui("map_selections_ui_1")
    
## To be copied in the server
# callModule(mod_map_selections_server, "map_selections_ui_1")
 
