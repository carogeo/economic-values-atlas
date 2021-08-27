#' map_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    # HTML("<p>Select variables of interest at the left and update map to view results. Green values and high ranks correspond to 'opportunity zones' where economic investments could have disporportionately positive impacts for the future prosperity of our entire region. Click on any tract to get more information.</p>"),
    leafletOutput(ns("map"), height = 700)#,
    
    # wellPanel(textOutput(ns("selected_tract")))
    
  )
}
    
#' map_overview Server Function
#'
#' @noRd 
mod_map_overview_server <- function(input, output, session,
                                    map_selections,
                                    map_util#,
                                    # current_tab
                                    ){
  ns <- session$ns
  
  output$map <- renderLeaflet({ #  map --------
    leaflet() %>%
      setView(
        lat = 39.5, #st_coordinates(map_centroid)[2], #44.963,
        lng = -98.35, #st_coordinates(map_centroid)[1], #-93.22,
        zoom = 5
      ) %>%
      addMapPane(name = "Stamen Toner", zIndex = 430) %>%
      addProviderTiles("Stamen.TonerLines",
                       group = "Stamen Toner"
      ) %>%
      addMapPane(name = "Carto Positron", zIndex = 430) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = leafletOptions(pane = "Carto Positron"),
                       group = "Carto Positron") %>%
      addProviderTiles("CartoDB.PositronNoLabels",
                       group = "Carto Positron"
      ) %>%
      addProviderTiles("Stamen.TonerLabels", 
                       options = leafletOptions(pane = "Stamen Toner"),
                       group = "Stamen Toner") %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Aerial Imagery"
      ) %>%
      
      #### regional specific other data layers
#      addMapPane("Corridor Tracts", zIndex = 430) %>%
#      addPolygons(
#        # Markers(
#        data = corridor_tracts,
#        group = "Corridor Tracts",
#        weight=4,
#        color="white",
#        opacity = 1,
#        highlightOptions = highlightOptions(
#          stroke = TRUE,
#          color = "white",
#          weight = 8,
#          bringToFront = TRUE,
#          opacity = 1
#        ),
#        fillColor = "transparent",# councilR::colors$transitRed,
#        popup = ~paste0("Corridor Name: ", corridor_tracts$corridor_number), 
#        #                        "<br>Average score: ", round(map_util$map_data2$MEAN, 3),
#        #                        "<br>Rank of score: ", map_util$map_data2$RANK, " out of ", nrow(map_util$map_data2)),
#        options = pathOptions(pane = "Corridor Tracts")
#      ) %>%
    # addMapPane("Corridors", zIndex = 470) %>%
    #   addPolygons(
    #     # Markers(
    #     data = corridors,
    #     group = "Corridors",
    #     weight=.1,
    #     color="white",
    #     opacity = 1,
    #     highlightOptions = highlightOptions(
    #       stroke = TRUE,
    #       color = "white",
    #       weight = 4,
    #       bringToFront = TRUE,
    #       opacity = 1,
    #       fillOpacity = 1
    #     ),
    #     fillColor = "white",
    #     fillOpacity = 1,
    #     popup = ~paste0("Corridor Name: ", corridors$Name), 
    #     #                        "<br>Average score: ", round(map_util$map_data2$MEAN, 3),
    #     #                        "<br>Rank of score: ", map_util$map_data2$RANK, " out of ", nrow(map_util$map_data2)),
    #     options = pathOptions(pane = "Corridors")
    #   ) %>%
      
#      hideGroup("Corridors") %>%
      addLayersControl(
        position = "bottomright",
        # overlayGroups = c(),
        baseGroups = c(
          "Carto Positron",
          "Stamen Toner",
          "Aerial Imagery"
        ),
#         overlayGroups = c(
#           "Scores",
# #          "Corridor Tracts",
#           "Corridors"
#         ),
        options = layersControlOptions(collapsed = T)
      )  
  })
  
  
  # #printing this works, but unclear how to save it....  
  # map_layer_selection <- reactiveValues()
  # observe({
  #   selected_groups <- req(input$map_groups)
  #   # print(selected_groups)
  #   return(selected_groups)
  #   })
  
  #leaflet observe events -----------
  
  # this stackoverflow was helpful: https://stackoverflow.com/questions/47465896/having-trouble-with-leafletproxy-observeevent-and-shiny-leaflet-application
  
  toListen_mainleaflet <- reactive({
    list(
      # current_tab,
      map_util$map_data2,
#      map_selections$goButton
      map_selections$jobsInput,
      map_selections$marketInput,
      map_selections$inclusivityInput,
      map_selections$realestateInput,
      map_selections$geoInput
    )
  })
  
  observeEvent(toListen_mainleaflet(),
               {
                 if (is.null(map_util$map_data2)) {
                   print('nodata')
                 } else {
                   print("rendering polygons")
                   leafletProxy("map") %>%
                     clearGroup("Scores") %>%
                     addMapPane("Scores", zIndex = 400) %>%
                     addPolygons(
                       data = map_util$map_data2 %>% st_transform(4326),
                       group = "Scores",
                       stroke = TRUE,
                       color =  "white",
                       opacity = 0.9,
                       weight = 0.5, #0.25,
                       fillOpacity = 0.7,
                       smoothFactor = 0.2,
                       highlightOptions = highlightOptions(
                         stroke = TRUE,
                         color = "#FFFFFF",
                         weight = 4,
                         bringToFront = TRUE,
                         opacity = 1,
                         fillOpacity=0.7
                       ),
                       fillColor = ~ colorNumeric(
                         n = 5,
                         palette = "magma",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       )(map_util$map_data2 %>% select("MEAN") %>% .[[1]]),
                       popup = ~paste0("ID: ", map_util$eva_data_main$NAME, 
                                       "<br>Average score: ", round(map_util$map_data2$MEAN, 3),
                                       "<br>Rank of score: ", map_util$map_data2$RANK, " out of ", nrow(map_util$map_data2)),
                       options = pathOptions(pane = "Scores"),
                       layerId = ~tract_string
                     ) %>%
                     # maybe want to add this: https://stackoverflow.com/questions/42245302/shiny-leaflet-highlight-polygon
                     
                     addLegend(
                       # labFormat = labelFormat2(),#labelFormat(prefix = "(", suffix = ")", digits = 5),
                       title = "Average scores",
                       position = "bottomleft",
                       group = "Scores",
                       layerId = "Scores",
                       pal = colorNumeric(
                         n = 5,
                         palette = "magma",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       ),
                       values = (map_util$map_data2 %>% select("MEAN") %>% .[[1]])
                     )
                   
                 }
               })

  #leaflet print geoid -----------
  
  #ideally want to do nested reactive values?!?
  #https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
  #but this is not working out well for me right now....
  # r <- reactiveValues(test = reactiveValues())
  # observe({
  #   event <- input$map_shape_click
  #   r$test$selected_tract <- (tractoutline$GEOID[tractoutline$GEOID == event$id])
  # })
  # # return(selected_tract)
  
  # #this works, but want to save it
  # observe({
  #   event <- input$map_shape_click
  #   output$selected_tract <- renderText(map_util$map_data2$tract_string[map_util$map_data2$tract_string] == event$id)#renderText(tractoutline$GEOID[tractoutline$GEOID == event$id])
  # })

  #save the selected tract
  vals <- reactiveValues()
  observe({
    event <- input$map_shape_click
    vals$selected_tract <- (map_util$map_data2$tract_string[map_util$map_data2$tract_string == event$id])
  })
  
  return(vals)
 
}
    
## To be copied in the UI
# mod_map_overview_ui("map_overview_ui_1")
    
## To be copied in the server
# callModule(mod_map_overview_server, "map_overview_ui_1")
 
