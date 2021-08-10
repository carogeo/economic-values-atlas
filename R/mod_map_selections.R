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
    
    radioButtons(
      ns("geoInput"),
      label = h4("Choose geography to map"),
      choices = c("Tract", "Corridor"),# inline=T,
      selected = c("Tract")
    ),
    
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
  
  observe({
    
    input_values$geoInput <- input$geoInput
    input_values$jobsInput <- input$jobsInput
    input_values$marketInput <- input$marketInput
    input_values$inclusivityInput <- input$inclusivityInput
    input_values$realestateInput <- input$realestateInput
    
    input_values$allInputs <- as_tibble(input$jobsInput) %>%
      rbind(as_tibble(input$marketInput)) %>%
      rbind(as_tibble(input$inclusivityInput)) %>%
      rbind(as_tibble(input$geoInput)) %>%
      rbind(as_tibble(input$realestateInput)) 
  }#, ignoreNULL = FALSE
  )
  
  
  return(input_values)
  
}
    
## To be copied in the UI
# mod_map_selections_ui("map_selections_ui_1")
    
## To be copied in the server
# callModule(mod_map_selections_server, "map_selections_ui_1")
 
