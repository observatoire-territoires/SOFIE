## Application de diagnostic territorial sur l'accès à l'emploi des femmes
# CL, OT, janvier 2019

rm(list = ls())

# Chargement des packages
library(sf)
library(dplyr)
library(readxl)
library(geojsonio)

library(shiny)
library(Rcpp)
library(httpuv)
library(shinythemes)
library(leaflet)
library(geojsonio)
library(htmltools)
library(tidyverse)
library(grDevices)
library(ggplot2)
library(ggrepel)
library(plotly)
library(shinyWidgets)
library(DT)
library(shinycssloaders)
library(readr)

# Chargement des métadonnées
meta <- read_xlsx("data/meta.xlsx", sheet = "Selection2019", range = "B1:H15")

# Chargement des données
dataAE_b100 <- read.csv2("data/dataAE_b100.csv")
dataAE <- read.csv2("data/dataAE.csv", stringsAsFactors = F) %>%
  gather("Indic", "valeur", 3:7)
dataAE_fr <- read.csv2("data/dataAE_fr.csv", stringsAsFactors = F)

dataFL_b100 <- read.csv2("data/dataFL_b100.csv", stringsAsFactors = F)
dataFL <- read.csv2("data/dataFL.csv", stringsAsFactors = F) %>%
  gather("Indic", "valeur", 2:9)
dataFL_fr <- read.csv2("data/dataFL_fr.csv", stringsAsFactors = F)

# Construction du tableau de données pour le graphique 1
data_gr1 <- dataAE_b100 %>%
  mutate(epci2018 = as.character(epci2018)) %>%
  filter(epci2018 != "Total") %>%
  gather("Indic", "valeur_b100", 3:7) %>%
  mutate(Indic = gsub("_b100", "", Indic)) %>%
  left_join(dataAE, by = c("epci2018", "SEXE", "Indic")) %>%
  left_join(dataAE_fr, by = c("SEXE", "Indic")) %>%
  left_join(select(meta, ID, Indicateur, Unite, Indicateur_s), by = c("Indic" = "ID")) %>%
  mutate(SEXE = ifelse(SEXE == 'F', 'Femmes', 'Hommes')) %>%
  group_by(epci2018, Indic) %>%
  mutate(diffFH = max(valeur_b100, na.rm = T)-min(valeur_b100, na.rm = T)) %>%
  ungroup() %>%
  group_by(epci2018, SEXE) %>%
  mutate(SUM_DIFF = sum(abs(100-valeur_b100))) %>%
  mutate(SUM_Diff_FH = sum(diffFH)) %>%
  ungroup()

# Construction du tableau de données pour le graphique 2
data_gr2 <- dataFL_b100 %>%
  mutate(epci2018 = as.character(epci2018)) %>%
  filter(epci2018 != "Total") %>%
  gather("Indic", "valeur_b100", 2:9) %>%
  mutate(Indic = gsub("_b100", "", Indic)) %>%
  left_join(dataFL, by = c("epci2018", "Indic")) %>%
  left_join(dataFL_fr, by = "Indic") %>%
  left_join(select(meta, ID, Indicateur, Unite, Indicateur_s), by = c("Indic" = "ID"))

# Correspondance commune epci
### FIXME : Test en ajoutant un EPCI contenant une apostrophe : marche bien pour moi
### FIXME : ne pas oublier de supprimer le add_row() après avoir réintegré les apostrophes
comepci <- read_xlsx("data/tabcomepci.xlsx") %>%
  add_row(com2018 = "01000",
          libcom = "Test d'apostrophe",
          epci2018 = 20000001,
          libepci = "Test d'apostrophe 2",
          dep = "01",
          libdep = "Ain",
          .before = 1) %>%
  mutate(IDlgcom = paste0(com2018, " - ", libcom)) %>%
  mutate(IDlgepci = paste0(epci2018, " - ", libepci)) %>%
  mutate(IDlgdep = paste0(dep, " - ", libdep)) %>%
  mutate(epci2018 = as.character(epci2018))

# Chargement des fonds de cartes
fra <- geojson_read("geo/fra_WGS84.json", what = "sp")
epci_gjs <- geojson_read("geo/epci2018_WGS84.json", what = "sp")
crcl <- geojson_read("geo/crclDROM.geojson", what = "sp")
cap <- geojson_read("geo/capitales.geojson", what = "sp")

for (column in colnames(epci_gjs@data)) {
  if (is.factor(epci_gjs@data[ , column])) {
    char <- as.character(epci_gjs@data[ , column])
    Encoding(char) <- "UTF-8"
    epci_gjs@data[ , column] <- as.factor(char)
  }
}

for (column in colnames(cap@data)) {
  if (is.factor(cap@data[ , column])) {
    char <- as.character(cap@data[ , column])
    Encoding(char) <- "UTF-8"
    cap@data[ , column] <- as.factor(char)
  }
}

#######################################################################################################
# APPLICATION
#######################################################################################################
  
  ui <- fluidPage(
    
    # Lien css
    includeCSS("style.css"),
    tags$head(
      tags$style(HTML("
              @import url('//fonts.googleapis.com/css?family=Bree+Serif');
              @import url('//fonts.googleapis.com/css?family=Ubuntu');
            ")),
      tags$title(HTML("SOFIE")),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )
    ),
    
    # Titre
    fluidRow(
      column(id = "titleband", width = 6,
             h1("SOFIE"),
             p(id = "sub", "Système d'Observation sur les Femmes et d'Information sur l'Emploi"))
    ),
    
    # Onglets
    navlistPanel(widths = c(1,11), id = "tabset", 
                 selected = icon("home", lib = "glyphicon"),
                 tabPanel(class = "active", icon("home", lib = "glyphicon"),
                            fluidRow(id = "bodyhome"),
                            fluidRow(
                                     column(6, align = "center", id = "col1",
                                            tags$div(id = "txtintro",
                                              HTML(
                                                "Au-delà des questions d’inégalités salariales, <span style = 'background-color : #955d7f;'>les femmes et les hommes n’ont pas un accès équivalent à l’emploi</span>. Plus souvent précaires ou à temps partiel, les femmes rencontrent un grand nombre d’obstacles dans leur parcours vers l’emploi, notamment parce qu’elles assument toujours la majeure partie des tâches domestiques. <span style = 'background-color : #955d7f;'>Les freins à l’emploi des femmes ne sont alors pas les mêmes selon les territoires</span> : familles nombreuses, non-mixité de l’offre d’emploi, éloignement de l’école,... <br/><br/>
                                                <span style = 'font-size : 1.2em ;'>SOFIE vous aide à établir votre propre bilan de l’accès des femmes à l’emploi dans votre territoire.</span>"
                                              )
                                            ),
                                            selectInput("chxdep1",
                                                        label = NULL,
                                                        choices = c("Votre département" = "", unique(comepci$IDlgdep)),
                                                        multiple = FALSE),
                                            #uiOutput("chxepci1")
                                            selectInput("chx1", label = NULL,
                                                        choices = c("Votre EPCI" = "", unique(comepci$IDlgepci)),
                                                        multiple = FALSE)
                                            ),
                                     column(6,
                                            withSpinner(leafletOutput("mapI"), color = "#ff8a78", type = 7),
                                            actionButton("go", "Commencer l'exploration >>"))
                            )
                          ),
                 tabPanel(icon("dashboard", lib = "glyphicon"), value = "tab2",
                          fluidRow(id = "row1",
                            column(6,
                                   absolutePanel(class = "panarr",
                                                 tags$a(div(class = "arrow"), href="#row2")),
                                   h2("L'accès à l'emploi des femmes\ndans les EPCI français"),
                                   absolutePanel(id = "recherche", top = 200, left = 25,
                                                 selectInput("chx2",
                                                             label = NULL,
                                                             choices = c("Votre EPCI" = "", unique(comepci$IDlgepci)),
                                                             multiple = FALSE,
                                                             selectize = T)
                                                 #uiOutput("inputcom")
                                                 ),
                                   withSpinner(leafletOutput("map"), color = "#ff8a78", type = 7)),
                            column(6, class = "v2",
                                   uiOutput("disp_libepci"),
                                     column(1),
                                     column(10, class = "v2txt",
                                                   p(class = "tttxt", "Un territoire pour lequel de ... freins ... l'accès à l'emploi des femmes"),
                                                   br(),
                                                   br(),
                                                   p("L'EPCI sélectionnée appartient à la classe X. Cette classe regroupe X EPCI soit X% des EPCI français."),
                                                   br(),
                                                   p("Cette classe se caractérise par un accès à l'emploi des femmes ... avec un taux de ... particulièrement élevé mais une part de ... inférieure à la moyenne nationale."),
                                                   br(),
                                                   p("Dans ces EPCI, les freins à l'emploi sont ... . L'effort doit porter principalement sur l'amélioration de ..., indispensable à l'amélioration de l'accès à l'emploi des femmes et à la diminution des écarts observés avec les hommes."))
                                   )),
                          fluidRow(id = "row2",
                                   column(6,
                                          absolutePanel(class = "panarr",
                                                        tags$a(div(class = "arrow"), href="#row3")),
                                          materialSwitch(inputId = "switchval", label = "Hommes", status = "default"),
                                          withSpinner(plotlyOutput("graph1"), color = "#ff8a78", type = 7),
                                          absolutePanel(top = 170, left = 130,
                                                        div(id = "fdf1", p("SITUATION PLUS FAVORABLE"))),
                                          absolutePanel(top = 170, right = 10,
                                                        div(id = "fdf2", p("DIFFICULTÉS ACCENTUÉES"))),
                                          br(),
                                          p(class = "nb", "Sources : Insee-RP 2015, traitements de l'Observatoire des territoires, 2019"),
                                          br(),
                                          p(class = "nb", "Note de lecture : dans ce graphique, les valeurs sont standardisées en référence à la valeur nationale pour les femmes qui vaut 100 pour chaque indicateur.
                                            La ligne en pointillés représente cette valeur nationale. Plus les valeurs sont situées à gauche de cette ligne et plus la situation est favorable comparativement à la situation des femmes en France ;
                                            inversement, plus les valeurs sont situées à droite de cette ligne et plus la situation est défavorable comparativement à la situation des femmes en France.")),
                                   column(6, class = "v2",
                                          uiOutput("disp_libepci1"),
                                          column(1),
                                          column(10, class = "v2txt",
                                                 br(),
                                                 uiOutput("comgr1"))
                                          )),
                          fluidRow(id = "row3",
                                   absolutePanel(id = "dir",
                                                 actionButton("dir1", "Comprendre les indicateurs"),
                                                 tags$a(id = "dir2", div("Chercher un autre territoire"), href="#row1")),
                                   column(6,
                                          withSpinner(plotlyOutput("graph2"), color = "#ff8a78", type = 7),
                                          absolutePanel(top = 170, left = 130,
                                                        div(id = "fdf1", p("SITUATION PLUS FAVORABLE"))),
                                          absolutePanel(top = 170, right = 10,
                                                        div(id = "fdf2", p("DIFFICULTÉS ACCENTUÉES"))),
                                          br(),
                                          p(class = "nb", "Sources : Insee-RP 2015, DEPP-Base Scolarité 2015-2016, Insee-METRIC 2016, traitements de l'Observatoire des territoires, 2019"),
                                          br(),
                                          p(class = "nb", "Note de lecture : dans ce graphique, les valeurs sont standardisées en référence à la valeur nationale pour les femmes qui vaut 100 pour chaque indicateur.
                                            La ligne en pointillés représente cette valeur nationale. Plus les valeurs sont situées à gauche de cette ligne et plus la situation est favorable comparativement à la situation des femmes en France ;
                                            inversement, plus les valeurs sont situées à droite de cette ligne et plus la situation est défavorable comparativement à la situation des femmes en France.")),
                                   column(6, class = "v2",
                                          uiOutput("disp_libepci2"),
                                          column(1),
                                          column(10, class = "v2txt",
                                                 br(),
                                                 uiOutput("comgr2"))
                                          ))),
                 tabPanel(icon("book", lib = "glyphicon"), value = "tab3",
                          fluidRow(
                              column(6, h2("Les indicateurs")))),
                 tabPanel(icon("question-sign", lib = "glyphicon"), value = "tab4",
                          fluidRow(
                            column(6, h2("Informations")))))
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    ## REACTIVE VALUES ##
    chxepci <- reactive({
      
      if(is.null(input$chx2)){
        react2 <- input$chx1
        react2 <- react2[!is.na(react2)]
      }
      else{
        react2 <- input$chx2
        react2 <- react2[!is.na(react2)]
      }
      return(react2)
    })
    
    curr_epci <- reactive({

      if(is.null(input$chx2)){
        react1 <- unique(comepci$epci2018[comepci$epci2018 == substr(input$chx1, 1, 9)])
        react1 <- react1[!is.na(react1)]
      }
      else{
        react1 <- unique(comepci$epci2018[comepci$epci2018 == substr(input$chx2, 1, 9)])
        react1 <- react1[!is.na(react1)]
      }
      return(react1)
    })
    
    curr_dep <- reactiveVal(value = 0)
    
    
    ## REACTIVE FUNCTIONS ##
    # Menu de recherche d'EPCI par nom INTRO
    # output$chxepci1 <- renderUI({
    #   selectInput("chx1",
    #               label = NULL,
    #               choices = c("Votre EPCI" = "", unique(comepci$IDlgepci[comepci$IDlgdep == input$chxdep1])),
    #               multiple = FALSE)
    # })
    
    observeEvent(input$chxdep1, {
      if (curr_dep() != input$chxdep1){
        curr_dep(input$chxdep1)
        updateSelectInput(session = session, inputId = "chx1", label = NULL,
                          choices = c("Votre EPCI" = "", unique(comepci$IDlgepci[comepci$IDlgdep == curr_dep()])))
      }
    }
    )
    
    # Menu de recherche d'EPCI par nom
    # output$inputcom <- renderUI({
    #   selectInput("chx2",
    #               label = NULL,
    #               choices = c("Votre EPCI" = "", unique(comepci$IDlgepci)),
    #               multiple = FALSE, 
    #               selected = input$chx1,
    #               selectize = T)
    # })
    
    # Affichage du nom de l'EPCI
    output$disp_libepci <- renderUI({
      p(class = "ttv", unique(comepci$libepci[comepci$IDlgepci == chxepci()]))
    })
    
    output$disp_libepci1 <- renderUI({
      p(class = "ttv", paste0(unique(comepci$libepci[comepci$IDlgepci == chxepci()])))
    })
    
    output$disp_libepci2 <- renderUI({
      p(class = "ttv", paste0(unique(comepci$libepci[comepci$IDlgepci == chxepci()])))
    })
    
    # Bouton GO
    observeEvent(input$go, {
      
      if(!is.null(input$chx1)){
        if(nchar(input$chx1) > 0){
        updateTabsetPanel(session, "tabset",
                          selected = "tab2")
        }
        else{
          showNotification("Sélectionnez un EPCI avant de commencer l'exploration.")
        }
      }
  
    })
    
    # Bouton to onglet indicateurs
    observeEvent(input$dir1, {
      updateTabsetPanel(session, "tabset",
                        selected = "tab3")
    })
    
    ############
    ## CARTES ##
    ############
    
    ####################
    # Carte localisation
    output$mapI <- renderLeaflet({
      
      if(!is.null(input$chx1)){
      
        # Infobulles
        labels <- sprintf(
          "<strong>%s</strong>",
          epci_gjs$libgeo
        ) %>% lapply(htmltools::HTML)
        
        # Carte
        if(length(curr_epci()) != 0){
          
          # Définition de l'emprise
          box <- st_bbox(subset(epci_gjs, codgeo == curr_epci())) %>%
            as.vector()
          
          # Carte
          leaflet() %>%
            addPolygons(data = crcl,
                        fillColor = 'transparent',
                        color = "#8f8f8f",
                        opacity = 1,
                        weight = 1) %>%
            fitBounds(box[1] - .3, box[2] - .3, box[3] + .3, box[4] + .3) %>%
            addPolygons(data = epci_gjs,
                        layerId = ~codgeo,
                        fillColor = "#ff8a78",
                        weight = 0.8,
                        color = "white",
                        fillOpacity = 1,
                        highlight = highlightOptions(
                          weight = 0,
                          fillColor = "#e86753",
                          color = NULL,
                          fillOpacity = 1,
                          bringToFront = FALSE),
                        label = labels,
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "15px",
                          direction = "auto")) %>%
            addCircleMarkers(data = cap,
                             fillColor = "#333232",
                             fillOpacity = 1,
                             color = "#333232",
                             opacity = 1,
                             radius = 2,
                             label = ~LIBGEO,
                             labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                                         style = list(
                                                           "color" = "#333232",
                                                           "font-family" = "Source Sans Pro",
                                                           "font-size" = "9px",
                                                           "background-color" = "rgba(255,255,255,0.7)",
                                                           "border" = "0px",
                                                           "padding" = "1px"
                                                         ))) %>%
            addPolygons(data = subset(epci_gjs, codgeo == curr_epci()),
                            fillColor = 'transparent',
                            color = "#333232",
                            opacity = 1,
                            weight = 3)
        }
      else{
        
        # Définition de l'emprise
        box <- st_bbox(epci_gjs) %>%
                 as.vector()
          
        # Définition de la carte
        leaflet() %>%
          addPolygons(data = crcl,
                      fillColor = 'transparent',
                      color = "#8f8f8f",
                      opacity = 1,
                      weight = 1) %>%
          fitBounds(box[1] - .3, box[2] - .3, box[3] + .3, box[4] + .3) %>%
          addPolygons(data = epci_gjs,
                      layerId = ~codgeo,
                      fillColor = "#ff8a78",
                      weight = 0.8,
                      color = "white",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 0,
                        fillColor = "#e86753",
                        color = NULL,
                        fillOpacity = 1,
                        bringToFront = FALSE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addCircleMarkers(data = cap,
                           fillColor = "#333232",
                           fillOpacity = 1,
                           color = "#333232",
                           opacity = 1,
                           radius = 2,
                           label = ~LIBGEO,
                           labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                                       style = list(
                                                         "color" = "#333232",
                                                         "font-family" = "Source Sans Pro",
                                                         "font-size" = "9px",
                                                         "background-color" = "rgba(255,255,255,0.7)",
                                                         "border" = "0px",
                                                         "padding" = "1px"
                                                       )))
        }
      }
    })
    
    # Comportement au clic
    observeEvent(input$mapI_shape_click, {
      p <-  input$mapI_shape_click
      if(!is.null(p$id)){
        
        dep <- unique(comepci$IDlgdep[comepci$epci2018 == p$id])
        dep <- dep[!is.na(dep)]
        
        updateSelectInput(session, "chxdep1", selected = dep)
        
        res <- p$id
        res <- unique(comepci$IDlgepci[comepci$epci2018 == res])
        res <- res[!is.na(res)]
        curr_dep(dep)
       
        updateSelectInput(session, "chx1",
                          choices = c("Votre EPCI" = "", unique(comepci$IDlgepci[comepci$IDlgdep == curr_dep()])), selected = res)
        
        updateSelectInput(session, "chx2", choices = c("Votre EPCI" = "", unique(comepci$IDlgepci)), selected = res)
        
      }
    })
    
    
    
    ############
    # Carte typo
    output$map <- renderLeaflet({
      
      if(!is.null(input$chx2)){
        
        # Définition de l'emprise en fonction du choix de l'EPCI
        box <- st_bbox(subset(epci_gjs, codgeo == curr_epci())) %>%
          as.vector()
        
        # Infobulles
        labels <- sprintf(
          "<strong>%s</strong>",
          epci_gjs$libgeo
        ) %>% lapply(htmltools::HTML)
        
        # Carte
        leaflet() %>%
          addPolygons(data = crcl,
                      fillColor = 'transparent',
                      color = "#8f8f8f",
                      opacity = 1,
                      weight = 1) %>%
          fitBounds(box[1] - .3, box[2] - .3, box[3] + .3, box[4] + .3) %>%
          addPolygons(data = epci_gjs,
                      layerId = ~codgeo,
                      fillColor = "#ff715b",
                      weight = 0.8,
                      color = "white",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 0,
                        fillColor = "#e86753",
                        color = NULL,
                        fillOpacity = 1,
                        bringToFront = FALSE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addCircleMarkers(data = cap,
                           fillColor = "#333232",
                           fillOpacity = 1,
                           color = "#333232",
                           opacity = 1,
                           radius = 2,
                           label = ~LIBGEO,
                           labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                                       style = list(
                                                         "color" = "#333232",
                                                         "font-family" = "Source Sans Pro",
                                                         "font-size" = "9px",
                                                         "background-color" = "rgba(255,255,255,0.7)",
                                                         "border" = "0px",
                                                         "padding" = "1px"
                                                       ))) %>%
          addPolygons(data = subset(epci_gjs, codgeo == curr_epci()),
                      fillColor = 'transparent',
                      color = "#333232",
                      opacity = 1,
                      weight = 3)
      }
    })
    
    # Comportement au clic
    observeEvent(input$map_shape_click, {
      p <-  input$map_shape_click
      if(!is.null(p$id)){
        updateSelectInput(session, "chx1", choices =c("Votre EPCI" = "", unique(comepci$IDlgepci)), selected = comepci$IDlgepci[comepci$epci2018 == p$id])
        updateSelectInput(session, "chx2", choices =c("Votre EPCI" = "", unique(comepci$IDlgepci)), selected = comepci$IDlgepci[comepci$epci2018 == p$id])
      }
    })
    
    
    ################
    ## GRAPHIQUES ##
    ################
    
    #############
    # Graphique 1
    output$graph1 <- renderPlotly({
  
      if(input$switchval == T){
        df1 <- filter(data_gr1, epci2018 == curr_epci()) %>%
          mutate(Indicateur_s = factor(Indicateur_s, levels = rev(meta$Indicateur_s)))
      }
      else{
        df1 <- filter(data_gr1, epci2018 == curr_epci() & SEXE == "Femmes") %>%
          mutate(Indicateur_s = factor(Indicateur_s, levels = rev(meta$Indicateur_s)))
      }
      
      # Définition de l'amplitude du graphique
      ampinit <- max(df1$valeur_b100, na.rm = T)-min(df1$valeur_b100, na.rm = T)
      
      if(min(df1$valeur_b100) > (100-(ampinit*0.3))){ # si le minimum est supérieur à 100 - 30% de l'amplitude initiale
        vmin <- 100-(ampinit*0.3) # on définit la borne minimum comme étant égale à 100 - 30% de l'amplitude
        amp <- max(df1$valeur_b100) - vmin # on recalcule l'amplitude
      }
      else{
        vmin <- min(df1$valeur_b100)
        amp <- max(df1$valeur_b100)-vmin
      }
      if(max(df1$valeur_b100) < (100+(amp*0.3))){ # si le maximum est inférieur à 100 + 30% de l'amplitude mise à jour
        vmax <- 100+(amp*0.3) # on définit la borne maximum comme étant égale à 100 + 30% de l'amplitude
        amp <- vmax - vmin # on recalcule l'amplitude
        lim <- c(vmin, vmax) # on définit les bornes du graphique
      }
      else{
        vmax <- max(df1$valeur_b100)
        amp <- vmax - vmin # on recalcule l'amplitude
        lim <- c(vmin, vmax) # on définit les bornes du graphique
      }
      
      # Graphique
      g1 <- ggplot(df1, aes(x = Indicateur_s, y = valeur_b100, text = paste0(Indicateur, " : ", round(valeur,1), " ", Unite, ",\ncontre ", round(valeur_france,1), " ", Unite, " en France"))) +
        geom_point(aes(col = SEXE), size = 3) + 
        geom_segment(aes(x=0.5, xend=6, y=100, yend=100), linetype = "dotted", colour = "#787a78", size = 0.2) + 
        scale_colour_manual(limits = c("Femmes", "Hommes"), values = c("#6e2150", "#ff715b")) +
        coord_flip() +
        labs(y = "Base 100 en référence à la valeur nationale\nobservée pour les femmes", x = "") +
        annotate("rect", xmin = 5.8, xmax = 6.4, ymin = 100-(amp*0.05), ymax = 100+(amp*0.05), fill = "#F9F9F9") +
        annotate("text", x = 6, y = 100-(amp*0.045), label = "France", size = 3, colour = "#333232") +
        ylim(lim) +
        theme(plot.background = element_rect(fill = "#F9F9F9"),
              panel.background = element_rect(fill = "#F9F9F9"),
              axis.ticks=element_blank(),
              axis.text.x = element_text(size = 6),
              axis.title.x=element_text(size = 8),
              panel.grid.major.y = element_line(colour = "#e3e3e3", size = 1, lineend = "round"))
  
      ggplotly(g1, tooltip = c("text")) %>%
        config(displayModeBar = F) %>%
        style(textposition = "right",
              hoverlabel = list(bgcolor = "#333232",
                                bordercolor = "#333232",
                                font = list(color = "white"))) %>%
        layout(legend = list(
          orientation = "h",
          xanchor = "center",
          y = -0.5,
          x = 0.5,
          bgcolor = "#F9F9F9"))
    })
    
    #############
    # Graphique 2
    
    output$graph2 <- renderPlotly({
      
      df2 <- filter(data_gr2, epci2018 == curr_epci()) %>%
        mutate(Indicateur_s = factor(Indicateur_s, levels = rev(meta$Indicateur_s)))
      
      # Définition de l'amplitude du graphique
      ampinit <- max(df2$valeur_b100, na.rm = T)-min(df2$valeur_b100, na.rm = T)
      
      if(min(df2$valeur_b100) > (100-(ampinit*0.3))){ # si le minimum est supérieur à 100 - 30% de l'amplitude initiale
        vmin <- 100-(ampinit*0.3) # on définit la borne minimum comme étant égale à 100 - 30% de l'amplitude
        amp <- max(df2$valeur_b100) - vmin # on recalcule l'amplitude
      }
      else{
        vmin <- min(df2$valeur_b100)
        amp <- max(df2$valeur_b100)-vmin
      }
      if(max(df2$valeur_b100) < (100+(amp*0.3))){ # si le maximum est inférieur à 100 + 30% de l'amplitude mise à jour
        vmax <- 100+(amp*0.3) # on définit la borne maximum comme étant égale à 100 + 30% de l'amplitude
        amp <- vmax - vmin # on recalcule l'amplitude
        lim <- c(vmin, vmax) # on définit les bornes du graphique
      }
      else{
        vmax <- max(df2$valeur_b100)
        amp <- vmax - vmin # on recalcule l'amplitude
        lim <- c(vmin, vmax) # on définit les bornes du graphique
      }
      
      # Graphique
      g2 <- ggplot(df2, aes(x = Indicateur_s, y = valeur_b100, text = paste0(Indicateur, " : ", round(valeur,1), " ", Unite, ",\ncontre ", round(valeur_france,1), " ", Unite, " en France"))) +
        geom_point(col = "#6e2150", size = 3) + 
        geom_segment(aes(x=0.5, xend=9, y=100, yend=100), linetype = "dotted", colour = "#787a78", size = 0.2) + 
        coord_flip() +
        labs(y = "Base 100 en référence à la valeur nationale", x = "") +
        annotate("rect", xmin = 8.8, xmax = 9.4, ymin = 100-(amp*0.05), ymax = 100+(amp*0.05), fill = "#F9F9F9") +
        annotate("text", x = 9, y = 100-(amp*0.045), label = "France", size = 3, colour = "#333232") +
        ylim(lim) +
        theme(plot.background = element_rect(fill = "#F9F9F9"),
              panel.background = element_rect(fill = "#F9F9F9"),
              axis.ticks=element_blank(),
              axis.text.x = element_text(size = 6),
              axis.title.x=element_text(size = 8),
              panel.grid.major.y = element_line(colour = "#e3e3e3", size = 1, lineend = "round"),
              panel.grid.major.x = element_blank())
      
      ggplotly(g2, tooltip = c("text")) %>%
        config(displayModeBar = F) %>%
        style(textposition = "right",
          hoverlabel = list(bgcolor = "#333232",
                            bordercolor = "#333232",
                            font = list(color = "white")))
    })
    
    ##################
    ## COMMENTAIRES ##
    ##################
    
    output$comgr1 <- renderUI({
      
      df1 <- filter(data_gr1, epci2018 == curr_epci() & SEXE == "Femmes") %>%
        mutate(Indicateur_s = factor(Indicateur_s, levels = rev(meta$Indicateur_s)))
      
      dfIneg <- filter(data_gr1, epci2018 == curr_epci()) %>%
        mutate(Indicateur_s = factor(Indicateur_s, levels = rev(meta$Indicateur_s)))
      
      # Indicateur > 120
      indic_sup <- df1$Indicateur[df1$valeur_b100 > 120]
      if(length(indic_sup) > 1){
            listis <- gsub("\r\n", " ", indic_sup[1])
            for(i in indic_sup[2:length(indic_sup)]){
              new <- gsub("\r\n", " ", i)
              listis <- paste0(listis, ", ", new)
            }
      }
      if(length(indic_sup) == 1){
        listis <- gsub("\r\n", " ", indic_sup[1])
      }
      
      # Inégalités F-H
      inegfh <- unique(df1$SUM_Diff_FH)
      if(inegfh <190){
        txtineg <- "Les <strong>inégalités entre les femmes et les hommes</strong> sont <strong>globalement moins marquées</strong> dans le territoire sélectionné qu'à l'échelle nationale. "
      }
      if(inegfh > 190){
        txtineg <- "Les <strong>inégalités entre les femmes et les hommes</strong> sont <strong>globalement plus marquées</strong> dans le territoire sélectionné qu'à l'échelle nationale. "
      }
      
      # Choix du titre
      if(unique(df1$SUM_DIFF) > 100){
        if(length(indic_sup) == 5 |length(indic_sup) == 4){
          titre <- "Un accès à l'emploi des femmes fortement dégradé"
        }
        else if(length(indic_sup) == 3){
          titre <- "Un accès à l'emploi des femmes dégradé"
        }
        else if(length(indic_sup) == 1 | length(indic_sup) == 2){
          titre <- "Un accès à l'emploi des femmes plutôt favorable"
        }
        else if(length(indic_sup) == 0){
          titre <- "Un accès à l'emploi des femmes particulièrement favorable"
        }
      }
      else{
        titre <- "Un accès à l'emploi des femmes proche de la situation nationale"
      }
      
      # CAS 1 : TOUT SUP 100
      if(length(indic_sup) == 5){
        HTML(
        paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
              <p>Dans l’EPCI sélectionnée, tous les indicateurs d’accès à l’emploi des femmes excèdent largement la valeur française, traduisant une situation des femmes face à l'emploi très fortement défavorable.
               L’indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df1$Indicateur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]), "</span> est celui qui se distingue le plus avec une valeur de ",
               round(df1$valeur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], 1),  df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]," contre ", round(df1$valeur_france[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)],1), df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], " en France.<br/><br/>", txtineg,"
               L'indicateur <span style = 'background-color : #efc9cf ; '>", tolower(df1$Indicateur[df1$diffFH == max(df1$diffFH)]),"</span> est celui pour lequel les inégalités sont le plus marquées entre les femmes et les hommes
               avec une valeur de ", round(dfIneg$valeur[dfIneg$SEXE == "Femmes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les femmes contre ", round(dfIneg$valeur[dfIneg$SEXE == "Hommes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les hommes.
               </p>"))
      }
      # CAS 2 : TOUT INF 100
      else if(length(indic_sup) == 0){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
              <p>Dans l’EPCI sélectionnée, aucun indicateur n'excède largement la valeur française.
               L’indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df1$Indicateur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]), "</span> apparaît néanmoins comme le <strong>principal point faible</strong> de l’accès à l’emploi des femmes dans ce territoire avec une valeur de ",
                 round(df1$valeur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], 1),  df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], ".<br/><br/>", txtineg,"
               L'indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df1$Indicateur[df1$diffFH == max(df1$diffFH)]),"</span> est celui pour lequel les inégalités sont le plus marquées entre les femmes et les hommes
               avec une valeur de ", round(dfIneg$valeur[dfIneg$SEXE == "Femmes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les femmes contre ", round(dfIneg$valeur[dfIneg$SEXE == "Hommes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les hommes.
                 </p>"))
      }
      # CAS 3 : CLASSIQUE
      else if(length(indic_sup) == 1){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
                 <p>Dans l’EPCI sélectionnée, ", length(indic_sup), " indicateur d’accès à l’emploi des femmes excède largement la valeur française : <span style = 'background-color : #efc9cf ;'>",
                 tolower(listis), "</span>. Pour cet indicateur, la <strong>situation</strong> est donc <strong>sensiblement plus dégradée dans le territoire</strong> sélectionné que dans l'ensemble du pays.
                 L'indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df1$Indicateur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]),"</span> apparaît comme le <strong>principal point faible</strong> dans l'accès à l'emploi des femmes dans ce territoire avec une valeur de ",
                 round(df1$valeur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], 1),  df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]," contre ", round(df1$valeur_france[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)],1), df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], " en France.<br/><br/>", txtineg,"
                 L'indicateur <span style = 'background-color : #efc9cf '>", tolower(df1$Indicateur[df1$diffFH == max(df1$diffFH)]),"</span> est celui pour lequel les inégalités sont le plus marquées entre les femmes et les hommes
                 avec une valeur de ", round(dfIneg$valeur[dfIneg$SEXE == "Femmes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les femmes contre ", round(dfIneg$valeur[dfIneg$SEXE == "Hommes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les hommes.
                 </p>"))
      }
      else{
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
                 <p>Dans l’EPCI sélectionnée, ", length(indic_sup), " indicateurs d’accès à l’emploi des femmes excèdent largement la valeur française : <span style = 'background-color : #efc9cf ;'>",
                 tolower(listis), "</span>. Pour ces indicateurs, la <strong>situation</strong> est donc <strong>sensiblement plus dégradée dans le territoire</strong> sélectionné que dans l'ensemble du pays.
                 L'indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df1$Indicateur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]),"</span> apparaît comme le <strong>principal point faible</strong> dans l'accès à l'emploi des femmes dans ce territoire avec une valeur de ",
                 round(df1$valeur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], 1),  df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]," contre ", round(df1$valeur_france[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)],1), df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], " en France.<br/><br/>", txtineg,"
                 L'indicateur <span style = 'background-color : #efc9cf '>", tolower(df1$Indicateur[df1$diffFH == max(df1$diffFH)]),"</span> est celui pour lequel les inégalités sont le plus marquées entre les femmes et les hommes
                 avec une valeur de ", round(dfIneg$valeur[dfIneg$SEXE == "Femmes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les femmes contre ", round(dfIneg$valeur[dfIneg$SEXE == "Hommes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les hommes.
                 </p>"))
      }
    })
    
    output$comgr2 <- renderUI({
      
      df2 <-  filter(data_gr2, epci2018 == curr_epci()) %>%
        mutate(Indicateur_s = factor(Indicateur_s, levels = rev(meta$Indicateur_s)))
      
      # Indicateur > 120
      indic_sup <- df2$Indicateur[df2$valeur_b100 > 120]
      if(length(indic_sup) > 1){
        listis <- gsub("\r\n", " ", indic_sup[1])
        for(i in indic_sup[2:length(indic_sup)]){
          new <- gsub("\r\n", " ", i)
          listis <- paste0(listis, ", ", new)
        }
      }
      if(length(indic_sup) == 1){
        listis <- gsub("\r\n", " ", indic_sup[1])
      }
      
      # Choix du titre
      if(length(indic_sup) >= 0 & length(indic_sup) < 3){
        titre <- "Un relativement faible nombre de freins potentiels à l'accès à l'emploi des femmes"
      }
      else if(length(indic_sup) >= 3 & length(indic_sup) < 5){
        titre <- "Un certain nombre de freins potentiels à l'accès à l'emploi des femmes"
      }
      else if(length(indic_sup) >= 5){
        titre <- "Un très grand nombre de freins potentiels à l'accès à l'emploi des femmes"
      }
      
      # CAS 1 : TOUT SUP 100
      if(length(indic_sup) == 8){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
                 <p>Dans l’EPCI sélectionnée, tous les indicateurs de freins potentiels à l'accès à l'emploi des femmes ont une valeur largement supérieure à la valeur observée en France. Les femmes y rencontrent donc <strong>un très grand nombre d'obstacles dans leur parcours vers l'emploi</strong> comparativement à la situation française.<br/><br/>
                 L’indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df2$Indicateur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]), "</pan> constitue <strong>le frein potentiel le plus marquant</strong> avec une valeur de ",
                 round(df2$valeur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], 1),  df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]," contre ", round(df2$valeur_france[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)],1), df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], " en France.",
                 "</p>"))
      }
      # CAS 2 : TOUT INF 100
      else if(length(indic_sup) == 0){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
                 <p>Dans l’EPCI sélectionnée, aucun indicateur de frein potentiels à l'accès à l'emploi des femmes n'excède largement la valeur observée en France.<br/><br/>
                 L’indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df2$Indicateur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]), "</span constitue néanmoins <strong>le principal frein potentiel</strong> pour l’accès à l’emploi des femmes dans ce territoire avec une valeur de ",
                 round(df2$valeur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], 1),  df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)],
                 "</p>"))
      }
      # CAS 3 : CLASSIQUE
      else if(length(indic_sup) == 1){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
                 <p>Dans l’EPCI sélectionnée, ", length(indic_sup), " indicateur de freins potentiels à l’accès à l’emploi des femmes a une valeur largement supérieure à la valeur observée en France : <span style = 'background-color : #efc9cf ;'>",
                 tolower(listis), "</span>. Ce <strong>frein potentiel</strong> à l'accès à l'emploi des femmes est donc <strong>sensiblement plus marqué dans le territoire</strong> sélectionné que dans l'ensemble du pays.<br/><br/>
                 L'indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df2$Indicateur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]),"</span> apparaît comme <strong>le principal frein potentiel</strong> pour l'accès à l'emploi des femmes dans ce territoire avec une valeur de ",
                 round(df2$valeur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], 1),  df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]," contre ", round(df2$valeur_france[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)],1), df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], " en France.",
                 "</p>")
        )
      }
      else{
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
                 <p>Dans l’EPCI sélectionnée, ", length(indic_sup), " indicateurs de freins potentiels à l’accès à l’emploi des femmes ont une valeur largement supérieure à la valeur observée en France : <span style = 'background-color : #efc9cf ;'>",
                 tolower(listis), "</span>. Ces <strong>freins potentiels</strong> à l'accès à l'emploi des femmes sont donc <strong>sensiblement plus marqués dans le territoire</strong> sélectionné que dans l'ensemble du pays.<br/><br/>
                 L'indicateur <span style = 'background-color : #efc9cf ;'>", tolower(df2$Indicateur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]),"</span> apparaît comme <strong>le principal frein potentiel</strong> pour l'accès à l'emploi des femmes dans ce territoire avec une valeur de ",
                 round(df2$valeur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], 1),  df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]," contre ", round(df2$valeur_france[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)],1), df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], " en France.",
                 "</p>")
        )
      }
      
      })
  
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)

