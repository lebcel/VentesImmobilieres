#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
#library(RColorBrewer)
#library(scales)
#library(lattice)
library(dplyr)
library(rdrop2)

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

shpfin <- readRDS("shpfin.rds")

cleantable2 <- shpfin %>%
    dplyr::select(
        Commune=Commune,
        Code.postal=Code.postal,
        Valeur.fonciere=Valeur.fonciere,
        Nombre.pieces.principales=Nombre.pieces.principales,
        Superficie=Surface.reelle.bati,
        Classes.Surf=Classes.Surf,
        Type.local=Type.local,
        Population=Dyn_Popu,
        Lat = latitude,
        Long = longitude
        
    )

# Define UI for application that draws a histogram
ui <- navbarPage("Certificat Data Science CEPE", id="nav",
                 
                 tabPanel("Carte interactive",
                          div(class="outer",
                              
                              tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",width = 330, height = "auto",
                                            
                                            ###########################################################################################################################
                                            # FILTRES MAP !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                            h2("Zoom"),
                                            selectInput("Id_Commune1", "Commune   ",sort(cleantable2$Commune),selected = " ")
                              ),
                              
                          )
                 ),
                 
                 tabPanel("Donnees",
                          
                          ###########################################################################################################################
                          # FILTRES DONNEES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          fluidRow(
                              column(3,selectInput("id_Commune", "Commune   ", c("Commune"="", cleantable2$Commune), multiple=TRUE)),
                              column(3,selectInput("id_Classes.Surf", "Classes.Surf   ", c("Classes.Surf"="", cleantable2$Classes.Surf), multiple=TRUE)) ,
                              column(3,selectInput("id_Type.local", "Type.local   ", c("Type.local"="", cleantable2$Type.local), multiple=TRUE)) ,
                              column(3,selectInput("id_Nombre.pieces", "Nombre.pieces   ", c("Nombre.pieces.principales"="", cleantable2$Nombre.pieces.principales), multiple=TRUE)) 
                          ),
                          fluidRow(
                              dataTableOutput(outputId = "dataset_obs")
                          )
                 ),
                 
                 conditionalPanel("false", icon("crosshair"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ## Interactive Map ###########################################
    
    # Creation de la carte 
    output$map <- renderLeaflet({
        
        #labels <- sprintf(
        #  "<br/> Test <br/><br/> Test <br/><br/> Test <br/>%f - %s - %s <br/> Test <br/> <sup>2</sup>",
        #  mean(shpfin$PxEstime[shpfin$Section==shpfin$Section & shpfin$Commune==shpfin$Commune]),
        #  shpfin$Section,
        #  shpfin$Commune
        #) %>% lapply(htmltools::HTML)
        
        labels <- sprintf(
            "<strong>Prix estime : %s</strong> <br/><br/>
       Info cadastre : %s <br/>
       . Nb ventes : %s <br/>
       . Pourc Appt : %s <br/><br/>
       Info ville : %s <br/>
       . Population : %s <br/>
       . Nb commerces : %s <br/>",
            shpfin$PxEstimeMoy, # Px mean(shpfin$PxEstime[shpfin$Section==shpfin$Section & shpfin$Commune==shpfin$Commune])
            shpfin$Section, # cadastre
            shpfin$nb_ventes, # vente
            shpfin$PourcAppt, # pourc
            shpfin$Commune, # ville
            shpfin$Dyn_Popu, # popu
            shpfin$Dyn_NbCommerces # comm
        ) %>% lapply(htmltools::HTML)
        
        #labels <- sprintf(
        #  "<strong>Prix estime : %s</strong>",
        #  shpfin$nb_ventes
        #) %>% lapply(htmltools::HTML)
        
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            )  %>% 
            addPolygons(data = shpfin$geometry,
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.9,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.1,
                            bringToFront = TRUE),
                        label = labels
            ) %>% 
            setView(lng = 2.3488, lat = 48.8534, zoom = 6)
        
    })
    
    observeEvent(input$Id_Commune1,
                 { leafletProxy("map", session) %>%
                         setView(lng = unique(cleantable2[cleantable2$Commune==input$Id_Commune1,]$Lat),
                                 lat= unique(cleantable2[cleantable2$Commune==input$Id_Commune1,]$Long),
                                 zoom=12)
                 })
    
    #zoneSelect <- reactive({
    #  if (is.null(input$map_bounds)) return(shpfin[FALSE,])
    #  bounds <- input$map_bounds
    #  latRng <- range(bounds$north, bounds$south)
    #  lngRng <- range(bounds$east, bounds$west)
    #  latRng
    #  lngRng
    #  subset(shpfin,
    #         latitude >= latRng[1] & latitude <= latRng[2] &
    #           longitude >= lngRng[1] & longitude <= lngRng[2])
    #})
    
    ###########################################################################################################################
    # Fonction popup MAP !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #showZipcodePopup <- function(blabla, lat, lng) {
    #  #selectedZip <- shpfin[shpfin$CodeParc2 == paste(blabla,"-",blabla, sep = "", collapse = NULL),] #blabla
    #  selectedZip <- shpfin[shpfin$CodeParc == "34172-AD",]
    #  content <- as.character(
    #    tags$h4("Score:", blabla,lng,lat) #as.character(mean(selectedZip$PxEstime))
    #  )
    #  leafletProxy("map") %>% addPopups(lng, lat, content, layerId = blabla)
    #}
    
    # Montre popup quand on clique
    #observe({
    #  leafletProxy("map") %>% clearPopups()
    #  event <- input$map_shape_click
    #  if (is.null(event)) return()
    #  
    #  isolate({
    #    showZipcodePopup(event$id, event$lat, event$lng)
    #  })
    #})
    
    ## Data Explorer ###########################################
    
    dataset <- reactive(get(input$id_dataset, "package:datasets"))
    
    observe({
        updateSelectInput(session, inputId = "id_Commune",choices = sort(cleantable2$Commune)) 
    })
    
    observe({
        updateSelectInput(session, inputId = "id_Classes.Surf",choices = sort(cleantable2$Classes.Surf))
    })
    
    observe({
        updateSelectInput(session, inputId = "id_Type.local",choices = sort(cleantable2$Type.local) )
    })
    
    observe({
        updateSelectInput(session, inputId = "id_Nombre.pieces",choices = sort(cleantable2$Nombre.pieces.principales) )
    })
    
    output$dataset_obs <- renderDataTable(
        shpfin %>% filter(is.null(input$id_Commune) | Commune %in% input$id_Commune,
                          is.null(input$id_Type.local) | Type.local %in% input$id_Type.local,
                          is.null(input$id_Classes.Surf) | Classes.Surf %in% input$id_Classes.Surf,
                          is.null(input$id_Nombre.pieces) | Nombre.pieces.principales %in% input$id_Nombre.pieces
        ) %>% 
            select(Date.mutation,Commune,Section,CODGEO,Code.postal,Code.departement,Type.local,PxEstime,Classes.Surf,Surface.reelle.bati,Nombre.pieces.principales,
                   Bpe_proximite_Taxi_VTC,Bpe_proximite_Services_aux_particuliers,Bpe_superieure_Services_aux_particuliers,Bpe_proximite_Sports_loisirs_culture,
                   Dyn_ScoreEquipSante,Dyn_RevDeptMoy,Dyn_NbCommerces,Dyn_ScoreFiscal,Dyn_Popu,Dyn_DepMoySalHor,Dyn_ScoreCroissPopu,Dyn_NbInfLib,
                   Dyn_NbResidSecond,Dyn_ScoreUrba,IndInflaB2015Mens,Inst_Milit,Inst_BaseLois),
        options = list(pageLength = 50)
    )
    colnames(shpfin)
}

# Run the application 
shinyApp(ui = ui, server = server)
