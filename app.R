## Application de diagnostic territorial sur l'accès à l'emploi des femmes SOFIE
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
library(shinyalert)

# Chargement des métadonnées
meta <- read_xlsx("data/meta.xlsx", sheet = "Selection2019", range = "B1:J15")
liens <- read.csv2("data/liens.csv")

# Chargement des données
data_gr1 <- read.csv2("data/data_gr1.csv", encoding = "UTF-8")
data_gr2 <- read.csv2("data/data_gr2.csv", encoding = "UTF-8")
typo <- read.csv2("data/typo.csv", sep= ",", encoding = "UTF-8")
comepci <- read.csv2("data/comepci.csv", sep = ",", encoding = "UTF-8") %>%
  filter(dep != '976') %>%
  mutate(IDlgepci = paste0(epci2018, " - ", libepci)) %>%
  mutate(IDlgdep = paste0(dep, " - ", libdep)) %>%
  mutate(epci2018 = as.character(epci2018))
dep_epci_simp <- read.csv2("data/dep_epci_simp.csv", encoding = "UTF-8")


# Chargement des fonds de cartes
epci_gjs <- geojson_read("geo/epci2018_WGS84.json", what = "sp")
crcl <- geojson_read("geo/crclDROM.json", what = "sp")
cap <- geojson_read("geo/capitales.json", what = "sp")

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
    
    useShinyalert(),
    
    # Lien css
    includeCSS("style.css"),
    tags$head(
      tags$style(HTML("
              @import url('//fonts.googleapis.com/css?family=Bree+Serif');
              @import url('//fonts.googleapis.com/css?family=Ubuntu');
              @import url('https://use.fontawesome.com/releases/v5.7.2/css/all.css');
            ")),
      tags$link(href= "www/favicon.ico"),
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
                 selected = HTML("<i class = 'glyphicon glyphicon-home' title = 'Accueil'></i>"),
                 tabPanel(class = "active", HTML("<i class = 'glyphicon glyphicon-home' title = 'Accueil'></i>"), value = "tab1",
                            fluidRow(id = "bodyhome"),
                            fluidRow(
                              absolutePanel(id = "logoPanel", top = 20, right = 0,
                                img(class = "logo", src = 'logo_adcf.png', height = "50px"),
                                img(class = "logo", src = 'logo_cget.png', height = "50px"),
                                img(class = "logo", src = 'logo_OT.png', height = "50px")),
                                     column(6, align = "center", id = "col1",
                                            tags$div(id = "txtintro",
                                              HTML(
                                                "Au-delà des questions d’inégalités salariales, <span style = 'background-color : #955d7f;'>les femmes et les hommes n’ont pas un accès équivalent à l’emploi</span>. 
                                                 Plus souvent exposées à de l’emploi précaire ou à temps partiel, <span style = 'background-color : #955d7f;'>les femmes sont confrontées à différents freins directs</span> (formation, non mixité de l’offre d’emploi…) <span style = 'background-color : #955d7f;'>et indirects</span> (mobilité, garde d’enfants, situation familiale...) dans leur parcours vers l’emploi et ce d'autant plus qu'elles assument toujours la majeure partie du travail domestique. <span style = 'background-color : #955d7f;'>Ces difficultés diffèrent selon les territoires</span> et sont souvent majorées dans les territoires fragiles.<br/><br/>
                                                 <span style = 'font-size : 1.2em ;'>SOFIE vous aide à établir votre propre bilan de l’accès des femmes à l’emploi dans votre territoire et vous fournit des clés pour vous saisir de la question dans les politiques locales.</span><br/><br/>"
                                                )
                                            ),
                                            selectInput("chxdep1",
                                                        label = NULL,
                                                        choices = c("Votre département" = "", levels(factor(unique(comepci$IDlgdep, levels = sort(comepci$IDlgdep))))),
                                                        multiple = FALSE),
                                            selectInput("chx1", label = NULL,
                                                        choices = c("Votre intercommunalité" = "", unique(comepci$IDlgepci)),
                                                        multiple = FALSE)
                                            ),
                                     column(6,
                                            withSpinner(leafletOutput("mapI"), color = "#ff8a78", type = 7),
                                            actionButton("go", "Commencer l'exploration >>"))
                            )
                          ),
                 tabPanel(HTML("<i class = 'glyphicon glyphicon-dashboard' title = 'Tableau de bord'></i>"), value = "tab2",
                          fluidRow(id = "row1",
                            column(6,
                                   absolutePanel(class = "panarr",
                                                 div(class = "panarrtxt", p("dans le détail...")),
                                                 tags$a(div(class = "arrow"), href="#row2")),
                                   h2("L'accès à l'emploi des femmes\ndans les EPCI français"),
                                   absolutePanel(id = "recherche", top = 200, left = 25,
                                                 selectInput("chxdep2",
                                                             label = NULL,
                                                             choices = c("Votre département" = "", levels(factor(unique(comepci$IDlgdep, levels = sort(comepci$IDlgdep))))),
                                                             multiple = FALSE),
                                                 selectInput("chx2",
                                                             label = NULL,
                                                             choices = c("Votre EPCI" = "", unique(comepci$IDlgepci)),
                                                             multiple = FALSE,
                                                             selectize = T)),
                                   withSpinner(leafletOutput("map"), color = "#ff8a78", type = 7),
                                   checkboxInput("displgd", "Afficher la légende")),
                            column(6, class = "v2",
                                   uiOutput("disp_libepci"),
                                     column(1),
                                     column(10, class = "v2txt",
                                            br(),
                                            uiOutput("comtypo")
                                   ))),
                          fluidRow(id = "row2",
                                   column(6,
                                          absolutePanel(class = "panarr",
                                                        div(class = "panarrtxt", p("voir les freins")),
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
                 tabPanel(HTML("<i class = 'glyphicon glyphicon-book' title = 'Les indicateurs'></i>"), value = "tab3",
                          fluidRow(id = "navigIndic",
                              column(12,
                                     h2("Les indicateurs"),
                                     do.call(navlistPanel, c(id='navIndic', lapply(1:14, function(i) {
                                       tabPanel(title=meta$Indicateur_s[i],
                                                fluidRow(
                                                  column(6, 
                                                         p(class = 'ttindic', meta$Indicateur[i]),
                                                         br(),
                                                         p(class = "srcindic", paste0("Source : ", meta$Source[i])),
                                                         p(class = "uniteindic", paste0("Unité : ", meta$Unite[i])),
                                                         br(),
                                                         HTML(paste0("<p class = 'descindic'>", meta$Description[i], "</p>"))),
                                                  column(6, 
                                                         uiOutput(outputId = paste0("crtch", i))
                                                         )
                                                ))
                                     })))
                                     ))),
                 tabPanel(HTML("<i class = 'glyphicon glyphicon-question-sign' title = 'Informations'></i>"), value = "tab4",
                          fluidRow(
                            column(6,
                                   h2("Informations"),
                                   br(),
                                   HTML("<p class = 'ssttinfo'>Contexte</p>
                                        <br/>
                                        <p class = 'bloc'>En référence aux orientations gouvernementales en matière d’égalité femmes-hommes, le CGET a fait de la lutte contre les inégalités sexuées et territoriales une de ses priorités transversales. Il s’agit à la fois de <strong>mesurer et objectiver ces inégalités sexuées dans les territoires fragiles</strong> afin de mieux les combattre et de soutenir le développement de multiples actions au sein des politiques contractuelles et partenariales dont il assure le pilotage (contrats de plan État-Région, fonds européens structurels et d’investissement, contrats de ville…) ou dans le cadre des politiques portées par les différents acteurs publics et privés.
                                        <br/>
                                        <br/>
                                        <strong>Les travaux conduits par l’Observatoire des territoires du CGET ont permis de mieux connaître la situation des femmes et des hommes des territoires ruraux</strong> (pris sous l’angle des communes très peu denses et peu denses) et ont en particulier mis en évidence des difficultés majorées d’accès à l’emploi des femmes de ces territoires.
                                        </br>
                                        <br/>
                                        Une étude pilotée par le CGET a démontré en 2018 que si les freins directs (formation, métiers, secteurs d’activité, marché de l’emploi) et indirects (conditions de travail, situation familiale, modes de garde, mobilité…) à l’accès à l’emploi des femmes ne sont pas spécifiques aux territoires peu denses et isolés, leur caractère rural a un effet amplificateur sur ces difficultés, ce qui est également le cas des quartiers prioritaires de la politique de la ville.
                                        <br/>
                                        <br/>
                                        L’application SOFIE s’inscrit dans le prolongement de ces travaux et est diffusée à l’occasion de la Journée du 8 mars 2019 parallèlement à un guide réalisé par le CGET en lien avec le Service des droits des femmes et de l’égalité entre les femmes et les hommes (SDFE) et la Délégation générale à l’emploi et à la formation professionnelle (DGEFP), visant à « Favoriser l'accès à l'emploi des femmes dans les territoires ruraux ».
                                        </p>
                                        <br/>
                                        <p class = 'ssttinfo'>Objectifs</p>
                                        <br/>
                                        <p class = 'bloc'>
                                        SOFIE a pour objectif d’<strong>aider à la réalisation de diagnostics territorialisés</strong> de l’accès à l’emploi des femmes en dressant au niveau des intercommunalités (en comparaison avec la France entière et les autres intercommunalités), <strong>un portrait de la situation des femmes face à l’emploi et des principaux freins et leviers pouvant l’améliorer</strong>. Cette application vise à fournir des clés pour se saisir de la question dans les politiques locales et partenariales conduites à l’échelle des intercommunalités (notamment les contrats de ruralité et les contrats de ville) en s'appuyant sur les outils et bonnes pratiques présentés dans le guide susmentionné.
                                        <br/>
                                        <br/>
                                        Cette application complète utilement le <a href = 'http://www.observatoire-des-territoires.gouv.fr/observatoire-des-territoires/fr/les-indicateurs-de-l-galit-femmes-hommes' target = '_blank'>kit de données sexuées mis en ligne par l’Observatoire des territoires</a> sur son espace cartographique le 8 mars 2018, comportant une soixantaine d'indicateurs sexués permettant d’établir un constat territorialisé des inégalités sexuées en France, ce dans différentes thématiques et à différentes échelles territoriales (<a href = 'http://carto.observatoire-des-territoires.gouv.fr/'  target = '_blank'>http://carto.observatoire-des-territoires.gouv.fr/</a> taper le mot-clé « égalité » dans le menu de recherche d’indicateurs).
                                        <br/>
                                        <br/>
                                        Ces indicateurs non exhaustifs pourront être complétés, notamment par d’autres données qualitatives définies localement en lien avec les différents acteurs concernés et auprès des bénéficiaires elles-mêmes.
                                        </p>
                                        <br/>
                                        <p class = 'ssttinfo'>Documentation</p>
                                        <br/>
                                        <ul id ='doc'>
                                          <li>Guide <a href = 'https://www.cget.gouv.fr/ressources/publications/favoriser-l-acces-a-l-emploi-des-femmes-dans-les-territoires-ruraux' target = '_blank'>« Favoriser l’accès à l’emploi des femmes dans les territoires ruraux : outils et bonnes pratiques »</a> (CGET, SDFE, DGEFP, 8 mars 2019)</li>
                                          <li>Etude relative aux freins et aux leviers pour l’accès des femmes à l’emploi dans les territoires ruraux (étude des cabinets Geste et Perfegal pour le compte du CGET, janvier 2018) : le rapport intégral de l’étude, sa synthèse et les cinq monographies sont disponibles <a href = 'https://www.cget.gouv.fr/ressources/publications/etude-relative-aux-freins-et-aux-leviers-pour-l-acces-des-femmes-a-l-emploi-dans-les-territoires-ruraux' target = '_blank'>en ligne</a></li>
                                          <li>Notes du CGET diffusées à l’occasion des journées des droits des femmes du 8 mars <a href = 'https://www.cget.gouv.fr/ressources/infographies/mieux-connaitre-pour-mieux-lutter-contre-les-inegalites-sexuees-dans-les-territoires-fragiles' target = '_blank'>2016</a> et <a href = 'https://www.cget.gouv.fr/actualites/dans-les-communes-tres-peu-denses-un-acces-inegal-a-l-emploi' target = '_blank'>2017</a></li>
                                          <li>«Les femmes des quartiers prioritaires : éléments démographiques et situation sur le marché du travail», dans le <a href = 'http://www.onpv.fr/uploads/media_items/rapport-onpv-2015.original.pdf' target = '_blank'>Rapport 2015 de l'Observatoire National de la Politique de la Ville</a> (p. 67)</li>
                                          <li>Rapport EGALITER, <a href = 'http://www.haut-conseil-egalite.gouv.fr/stereotypes-et-roles-sociaux/travaux-du-hcefh/article/rapport-egaliter-combattre' target = '_blank'>« Combattre maintenant les inégalités sexuées, sociales et territoriales dans les quartiers de la politique de la ville et dans les territoires ruraux fragilisés »</a>, Haut-conseil à l’égalité entre les femmes et les hommes (juin 2014)</li>
                                          <li>Vers l’égalité réelle entre les femmes et les hommes : chiffres clés-<a href = 'https://www.egalite-femmes-hommes.gouv.fr/publications/droits-des-femmes/egalite-entre-les-femmes-et-les-hommes/vers-legalite-reelle-entre-les-femmes-et-les-hommes-chiffres-cles-edition-2018/' target = '_blank'>édition 2018</a> et <a href = 'https://www.egalite-femmes-hommes.gouv.fr/publications/droits-des-femmes/egalite-entre-les-femmes-et-les-hommes/vers-legalite-reelle-entre-les-femmes-et-les-hommes-chiffres-cles-edition-2017/' target = '_blank'>2017</a> (secrétariat d’Etat chargé de l’égalité entre les femmes et les hommes)</li>
                                          <li>Travaux du centre de ressources Hubertine Auclert : <a href = 'https://www.centre-hubertine-auclert.fr' target = '_blank'>https://www.centre-hubertine-auclert.fr</a></li>
                                        </ul>
                                        <br/>
                                        <br/>"))
                                   )))
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    shinyalert(
      imageUrl = "logo_OT.png",
      imageWidth = 200,
      html = TRUE,
      text = "<p style = 'font-size : 1em ;'>Attention, cette application n'est pas optimisée pour les petits écrans type smartphones ou tablettes<br/><br/>
              Bonne navigation !</p>",
      confirmButtonCol = "#5b937c"
      )
    
    ## REACTIVE VALUES ##

    # Stockage du code de l'EPCI et du code département
    values <- reactiveValues("code_epci"= vector())
    curr_dep <- reactiveVal(value = 0)
    
    # Définition de l'emprise des cartes  
    box <- reactive({

      if(nchar(values$code_epci) == 9){
        react3 <- st_bbox(subset(epci_gjs, codgeo == values$code_epci)) %>%
          as.vector()
      }
      else{
        react3 <- st_bbox(epci_gjs) %>%
          as.vector()
      }
      return(react3)
    })
    

    
    ## REACTIVE FUNCTIONS ##
    ## Fonctionnement des menus déroulants
    
    # Choix département
    observeEvent(input$chxdep1, {

      if(curr_dep() != input$chxdep1){
          print("Je suis ici")
          curr_dep(input$chxdep1)
          updateSelectInput(session = session, inputId = "chx1", label = NULL,
                            choices = c("Votre EPCI" = "", unique(comepci$IDlgepci[which(comepci$IDlgdep == curr_dep())])))
          updateSelectInput(session = session, inputId = "chxdep2", label = NULL, selected = curr_dep())

      }
      else{
        updateSelectInput(session = session, inputId = "chxdep2", label = NULL, selected = curr_dep())
        updateSelectInput(session = session, inputId = "chx2", label = NULL,
                          choices = c("Votre EPCI" = "", unique(comepci$IDlgepci[which(comepci$IDlgdep == curr_dep())])))
      }
    })

    observeEvent(input$chxdep2, {

      if(curr_dep() != input$chxdep2){
        print("Je suis là")
        curr_dep(input$chxdep2)
        updateSelectInput(session = session, inputId = "chx2", label = NULL,
                          choices = c("Votre EPCI" = "", unique(comepci$IDlgepci[which(comepci$IDlgdep == curr_dep())])))
      }
    })

    # Choix EPCI
    observeEvent(input$chx1, {

        values$code_epci <- substr(input$chx1, 1, 9)
        updateSelectInput(session = session, inputId = "chx2", selected = input$chx1)

    })
    observeEvent(input$chx2, {
      values$code_epci <- substr(input$chx2, 1, 9)
      updateSelectInput(session = session, inputId = "chx2", selected = input$chx2)
    })
    
    
    # Affichage du nom de l'EPCI dans les volets de droite
    output$disp_libepci <- renderUI({
      lib <- unique(comepci$libepci[which(comepci$epci2018 == values$code_epci)])
      lib <- lib[!is.na(lib)]
      p(class = "ttv", lib)
    })
    
    output$disp_libepci1 <- renderUI({
      lib <- unique(comepci$libepci[which(comepci$epci2018 == values$code_epci)])
      lib <- lib[!is.na(lib)]
      p(class = "ttv", lib)
    })
    
    output$disp_libepci2 <- renderUI({
      lib <- unique(comepci$libepci[which(comepci$epci2018 == values$code_epci)])
      lib <- lib[!is.na(lib)]
      p(class = "ttv", lib)
    })
    
    ## FONCTIONNEMENT DES BOUTONS
    # Réinitialisation au clic sur "HOME"
    observe({
      if(!is.null(input$tabset)){
        if(input$tabset == "tab1"){
          updateSelectInput(session = session, inputId = "chxdep1", choices = c("Votre département" = "", levels(factor(unique(comepci$IDlgdep, levels = sort(comepci$IDlgdep))))))
          updateSelectInput(session = session, inputId = "chx1", choices = c("Votre EPCI" = "", unique(comepci$IDlgepci)))
          updateSelectInput(session = session, inputId = "chxdep2", choices = c("Votre département" = "", levels(factor(unique(comepci$IDlgdep, levels = sort(comepci$IDlgdep))))))
          updateSelectInput(session = session, inputId = "chx2", choices = c("Votre EPCI" = "", unique(comepci$IDlgepci)))
          
          values$code_epci <- 0
          curr_dep(0)
        }
      }
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
    
    # Bouton pour afficher ou désafficher la légende
    observeEvent(input$displgd, {
      if(input$displgd == T){
        layercarto <- sp::merge(epci_gjs, typo, by.x = "codgeo", by.y = "epci")
        
        # Couleurs
        coolors <- c("#6e2150", "#dc3778", "#f9dbbd", "#f86838", "#cfc133", "#a64936")
        bins <- factor(c("Un bon accès à l'emploi des femmes et un faible niveau d'inégalités femmes-hommes",
                         "Un bon accès à l'emploi des femmes mais certains freins demeurent",
                         "Un accès à l'emploi des femmes moyen et un faible niveau d'inégalités femmes-hommes",
                         "Un accès à l'emploi dégradé associé à de nombreux freins et à de fortes inégalités femmes-hommes",
                         "Un accès à l'emploi dégradé associé à des freins d'ordre principalement familiaux et à de fortes inégalités femmes-hommes",
                         "De très fortes difficultés d'accès à l'emploi pour les femmes mais aussi pour les hommes"),
                       levels = c("Un bon accès à l'emploi des femmes et un faible niveau d'inégalités femmes-hommes",
                                  "Un bon accès à l'emploi des femmes mais certains freins demeurent",
                                  "Un accès à l'emploi des femmes moyen et un faible niveau d'inégalités femmes-hommes",
                                  "Un accès à l'emploi dégradé associé à de nombreux freins et à de fortes inégalités femmes-hommes",
                                  "Un accès à l'emploi dégradé associé à des freins d'ordre principalement familiaux et à de fortes inégalités femmes-hommes",
                                  "De très fortes difficultés d'accès à l'emploi pour les femmes mais aussi pour les hommes"))
        factpal <- colorFactor(coolors, bins)
        
        leafletProxy("map") %>%
          addLegend(data = layercarto, pal = factpal, values = bins, opacity = 1, title = NULL, position = "bottomleft")
      }
      else{
        leafletProxy("map") %>%
          clearControls()
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

      if(length(values$code_epci) != 0){

        # Infobulles
        labels <- sprintf(
          "<strong>%s</strong>",
          epci_gjs$libgeo
        ) %>% lapply(htmltools::HTML)

        if(nchar(values$code_epci) != 9){
        # Carte
        leaflet() %>%
          addPolygons(data = crcl,
                      fillColor = 'transparent',
                      color = "#8f8f8f",
                      opacity = 1,
                      weight = 1) %>%
          fitBounds(box()[1] - .3, box()[2] - .3, box()[3] + .3, box()[4] + .3) %>%
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
        else{
            # Carte
            leaflet() %>%
              addPolygons(data = crcl,
                          fillColor = 'transparent',
                          color = "#8f8f8f",
                          opacity = 1,
                          weight = 1) %>%
              fitBounds(box()[1]-0.3, box()[2]-0.3, box()[3]+0.3, box()[4]+0.3) %>%
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
              addPolygons(data = subset(epci_gjs, codgeo == values$code_epci),
                          fillColor = 'transparent',
                          color = "#333232",
                          opacity = 1,
                          weight = 3)
        }
      }
    })

    # Comportement au clic
    observeEvent(input$mapI_shape_click, {
      p <-  input$mapI_shape_click
      if(!is.null(p$id)){

        dep <- dep_epci_simp$IDlgdep[which(dep_epci_simp$epci2018 == p$id)]
        dep <- dep[!is.na(dep)]
        
        curr_dep(dep)

        updateSelectInput(session, "chxdep1", selected = dep)

        res <- p$id
        res <- unique(comepci$IDlgepci[which(substr(comepci$IDlgepci, 1, 9) == p$id)])
        res <- res[!is.na(res)]

        updateSelectInput(session, "chx1",
                          choices = c("Votre EPCI" = "", unique(comepci$IDlgepci[which(comepci$IDlgdep == curr_dep())])), selected = res)

      }
    })
    
    
    
    ############
    # Carte typo
    output$map <- renderLeaflet({
      
      if(!is.null(input$chx2)){
        
        # Jointure
        layercarto <- sp::merge(epci_gjs, typo, by.x = "codgeo", by.y = "epci")
        
        # Couleurs
        coolors <- c("#6e2150", "#dc3778", "#f9dbbd", "#f86838", "#cfc133", "#a64936")
        bins <- factor(c("Un bon accès à l'emploi des femmes et un faible niveau d'inégalités femmes-hommes",
                          "Un bon accès à l'emploi des femmes mais certains freins demeurent",
                          "Un accès à l'emploi des femmes moyen et un faible niveau d'inégalités femmes-hommes",
                          "Un accès à l'emploi dégradé associé à de nombreux freins et à de fortes inégalités femmes-hommes",
                          "Un accès à l'emploi dégradé associé à des freins d'ordre principalement familiaux et à de fortes inégalités femmes-hommes",
                          "De très fortes difficultés d'accès à l'emploi pour les femmes mais aussi pour les hommes"),
                       levels = c("Un bon accès à l'emploi des femmes et un faible niveau d'inégalités femmes-hommes",
                                  "Un bon accès à l'emploi des femmes mais certains freins demeurent",
                                  "Un accès à l'emploi des femmes moyen et un faible niveau d'inégalités femmes-hommes",
                                  "Un accès à l'emploi dégradé associé à de nombreux freins et à de fortes inégalités femmes-hommes",
                                  "Un accès à l'emploi dégradé associé à des freins d'ordre principalement familiaux et à de fortes inégalités femmes-hommes",
                                  "De très fortes difficultés d'accès à l'emploi pour les femmes mais aussi pour les hommes"))
        factpal <- colorFactor(coolors, bins)
        
        # Définition de l'emprise en fonction du choix de l'EPCI
        box <- st_bbox(subset(epci_gjs, codgeo == values$code_epci)) %>%
          as.vector()
        
        # Infobulles
        labels <- sprintf(
          "<strong>%s</strong>",
          epci_gjs$libgeo
        ) %>% lapply(htmltools::HTML)
        
        # Carte
        leaflet() %>%
          fitBounds(box()[1] - .3, box()[2] - .3, box()[3] + .3, box()[4] + .3) %>%
          addPolygons(data = crcl,
                      fillColor = 'transparent',
                      color = "#8f8f8f",
                      opacity = 1,
                      weight = 1) %>%
          addPolygons(data = layercarto,
                      layerId = ~codgeo,
                      fillColor = ~factpal(layercarto[["lib"]]),
                      weight = 0.5,
                      color = "white",
                      fillOpacity = 1,
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
          addPolygons(data = subset(epci_gjs, codgeo == values$code_epci),
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

        dep <- dep_epci_simp$IDlgdep[which(dep_epci_simp$epci2018 == p$id)]
        curr_dep(dep)

      updateSelectInput(session, "chxdep2", selected = dep)      
      updateSelectInput(session, "chx2",
                        choices = comepci$IDlgepci[which(comepci$IDlgdep == curr_dep())],
                        selected = comepci$IDlgepci[which(comepci$epci2018 == p$id)])
      }
    })
    
    
    ################
    ## GRAPHIQUES ##
    ################
    
    #############
    # Graphique 1
    output$graph1 <- renderPlotly({
  
      if(input$switchval == T){
        df1 <- filter(data_gr1, epci2018 == values$code_epci) %>%
          mutate(Indicateur_s = factor(Indicateur_s, levels = rev(meta$Indicateur_s)))
      }
      else{
        df1 <- filter(data_gr1, epci2018 == values$code_epci & SEXE == "Femmes") %>%
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
        scale_colour_manual(limits = c("Femmes", "Hommes"), values = c("#6e2150", "#af859f")) +
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
              panel.grid.major.y = element_line(colour = "#e3e3e3", size = 1, lineend = "round"),
              panel.grid.major.x = element_blank())
  
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
      
      df2 <- filter(data_gr2, epci2018 == values$code_epci) %>%
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
    
    output$comtypo <- renderUI({
      
      df <- filter(typo, epci == values$code_epci)
      
      titre <- df$lib
      if(df$clust == "2"){
        
        com <- "
        <p class = 'nb'>Attention : toute typologie résultant d'une simplification de la réalité, la description générale de chaque classe peut parfois entrer en contradiction avec certaines données détaillées présentées par la suite.</p><br/>
        <p>Ce type de territoire caractérise 15% des EPCI français (hors Mayotte) et regroupe 46% de la population.<br/><br/>
        Ces EPCI, localisés principalement au coeur des plus grandes agglomérations, se caractérisent par un bon accès à l'emploi des femmes et par un faible niveau d'inégalités entre les femmes et les hommes sur le plan de l'accès à l'emploi.<br/><br/>
        Des freins potentiels existent : familles monoparentales, familles nombreuses et longueur des trajets domicile-travail mais ceux-ci semblent peu peser sur l'accès à l'emploi des femmes.</p><br/>"
        }
      if(df$clust == "1"){
        com <- "
        <p class = 'nb'>Attention : toute typologie résultant d'une simplification de la réalité, la description générale de chaque classe peut parfois entrer en contradiction avec certaines données détaillées présentées par la suite.</p><br/>
        <p>Ce type de territoire caractérise 19% des EPCI français (hors Mayotte) et regroupe 10% de la population.<br/><br/>
        Les EPCI de ce type, principalement localisés à proximité de grandes agglomérations, ont globalement un bon accès à l'emploi avec de très faibles niveaux d'inactivité et de chômage pour les femmes ainsi qu'une faible part de jeunes femmes non insérées.<br/><br/>
        Des freins potentiels à l'emploi des femmes existent néanmoins dans ces territoires : importance des familles nombreuses, moindre mixité de l'offre d'emploi et longueur des trajets domicile-travail se conjuguent et on observe une part plus élevée de femmes à temps partiel dans ces territoires qu'ailleurs.</p><br/>"
      }
      if(df$clust == "3"){
        com <- "
        <p class = 'nb'>Attention : toute typologie résultant d'une simplification de la réalité, la description générale de chaque classe peut parfois entrer en contradiction avec certaines données détaillées présentées par la suite.</p><br/>
        <p>Ce type de territoire caractérise 28% des EPCI français (hors Mayotte) et regroupe 17% de la population.<br/><br/>
        Il se caractérise par un accès à l'emploi des femmes proche de la situation nationale. Dans ces EPCI, les femmes ont donc un taux d'inactivité et de chômage proche de la moyenne nationale et ne sont globalement pas plus concernées par la précarité et le temps partiel. Les inégalités femmes-hommes en termes d'accès à l'emploi y sont au demeurant moins marquées que dans l'ensemble du pays.<br/><br/>
        Les freins potentiels à l'emploi des femmes y sont moins marqués que dans les autres EPCI malgré une proportion globalement plus importante de femmes faiblement diplômées et une moindre accessibilité aux écoles primaires.</p><br/>"
      }
      if(df$clust == "4"){
        com <- "
        <p class = 'nb'>Attention : toute typologie résultant d'une simplification de la réalité, la description générale de chaque classe peut parfois entrer en contradiction avec certaines données détaillées présentées par la suite.</p><br/>
        <p>Ce type de territoire caractérise 22% des EPCI français (hors Mayotte) et regroupe 8% de la population.<br/><br/>
        Les EPCI de ce type, dont beaucoup sont situés dans le nord-est du pays, ont pour point commun un accès à l'emploi des femmes globalement dégradé et surtout, des inégalités femmes-hommes particulièrement marquées sur le plan de l'accès à l'emploi. Ces inégalités se traduisent principalement par une proportion élevée de femmes à temps partiel.<br/><br/>
        Dans ces territoires, les freins potentiels à l'emploi des femmes sont nombreux et sont de plusieurs ordres : éloignement aux établissements scolaires, non-mixité de l'offre d'emploi, faible niveau de formation des femmes et moindre mixité de l'offre de formation au lycée sont autant d'éléments qui peuvent peser sur l'accès à l'emploi des femmes et expliquer les inégalités observées avec les hommes.<br/></p>"
      }
      if(df$clust == "5"){
        com <- "
          <p class = 'nb'>Attention : toute typologie résultant d'une simplification de la réalité, la description générale de chaque classe peut parfois entrer en contradiction avec certaines données détaillées présentées par la suite.</p><br/>
          <p>Ce type de territoire caractérise 15% des EPCI français (hors Mayotte) et regroupe 17% de la population.<br/><br/>
          Les EPCI de ce type, dont beaucoup sont situés le long du littoral méditerranéen, se caractérisent par un accès à l'emploi des femmes dégradé qui se traduit principalement par une part importante de femmes inactives ou au chômage et un taux élevé de jeunes femmes non insérées. Les inégalités femmes-hommes en matière d'emploi y sont également globalement plus élevées qu'à l'échelle nationale.<br/><br/>
          Les freins potentiels qui pèsent sur l'accès à l'emploi des femmes sont principalement d'ordre familiaux puisqu'on observe dans ces territoires une plus forte représentation de familles monoparentales ou nombreuses et une capacité d'accueil des jeunes enfants inférieure à la moyenne nationale.</p><br/>"
      }
      if(df$clust == "6"){
        com <- "
          <p class = 'nb'>Attention : toute typologie résultant d'une simplification de la réalité, la description générale de chaque classe peut parfois entrer en contradiction avec certaines données détaillées présentées par la suite.</p><br/>
          <p>Ce type de territoire caractérise 1% des EPCI français (hors Mayotte) et regroupe 3% de la population.<br/><br/>
          La totalité des EPCI de ce type se trouvent dans les 4 DOM 'historiques' (Martinique, Guadeloupe, Guyane et Réunion). Dans ces EPCI, les femmes ont un accès à l'emploi très dégradé par rapport à celui des femmes de métropole. Le chômage y est par exemple considérablement plus élevé, de même que l'inactivité et la précarité. Les jeunes femmes sont également nombreuses à ne pas être insérées. Cette situation n'est cependant pas exclusive aux femmes et on observe en réalité des inégalités femmes-hommes limitées en matière d'accès à l'emploi.<br/><br/>
          Les freins potentiels à l'emploi des femmes sont principalement d'ordre familiaux (surreprésentation des familles monoparentales et nombreuses, moindres capacités d'accueil pour les jeunes enfants). Les temps de trajet parfois très longs pour se rendre au travail peuvent également peser sur la capacité des femmes à trouver un emploi.</p><br/>"
      }
      
      HTML(paste0("<p class = 'tttxt'>", titre, "</p><br/>", com))
      
    })
    
    output$comgr1 <- renderUI({
      
      df1 <- filter(data_gr1, epci2018 == values$code_epci & SEXE == "Femmes") %>%
        mutate(Indicateur_s = factor(Indicateur_s, levels = rev(meta$Indicateur_s)))
      
      dfIneg <- filter(data_gr1, epci2018 == values$code_epci) %>%
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
      if(inegfh <170){
        txtineg <- "Les <strong>inégalités entre les femmes et les hommes</strong> sont <strong>globalement moins marquées</strong> dans le territoire sélectionné qu'à l'échelle nationale. "
      }
      if(inegfh >= 170 & inegfh < 210){
        txtineg <- "Les <strong>inégalités entre les femmes et les hommes </strong>dans le territoire sélectionné sont <strong>globalement comparables</strong> à celles observées à l'échelle nationale. "
      }
      if(inegfh >= 210){
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
        paste0("<p class = 'tttxt'>", titre, "</p><br/>
              <p>Dans l’EPCI sélectionnée, tous les indicateurs d’accès à l’emploi des femmes excèdent largement la valeur française, traduisant une situation des femmes face à l'emploi très fortement défavorable.
               L’indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df1$Indicateur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]), "</span> est celui qui se distingue le plus avec une valeur de ",
               round(df1$valeur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], 1),  df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]," contre ", round(df1$valeur_france[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)],1), df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], " en France.<br/><br/>", txtineg,"
               L'indicateur <span style = 'background-color : #f2aca0 ; '>", tolower(df1$Indicateur[df1$diffFH == max(df1$diffFH)]),"</span> est celui pour lequel les inégalités sont le plus marquées entre les femmes et les hommes
               avec une valeur de ", round(dfIneg$valeur[dfIneg$SEXE == "Femmes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les femmes contre ", round(dfIneg$valeur[dfIneg$SEXE == "Hommes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les hommes.
               </p>"))
      }
      # CAS 2 : TOUT INF 100
      else if(length(indic_sup) == 0){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/>
              <p>Dans l’EPCI sélectionnée, aucun indicateur n'excède largement la valeur française.
               L’indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df1$Indicateur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]), "</span> apparaît néanmoins comme le <strong>principal point faible</strong> de l’accès à l’emploi des femmes dans ce territoire avec une valeur de ",
                 round(df1$valeur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], 1),  df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], ".<br/><br/>", txtineg,"
               L'indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df1$Indicateur[df1$diffFH == max(df1$diffFH)]),"</span> est celui pour lequel les inégalités sont le plus marquées entre les femmes et les hommes
               avec une valeur de ", round(dfIneg$valeur[dfIneg$SEXE == "Femmes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les femmes contre ", round(dfIneg$valeur[dfIneg$SEXE == "Hommes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les hommes.
                 </p>"))
      }
      # CAS 3 : CLASSIQUE
      else if(length(indic_sup) == 1){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/>
                 <p>Dans l’EPCI sélectionnée, ", length(indic_sup), " indicateur d’accès à l’emploi des femmes excède largement la valeur française : <span style = 'background-color : #f2aca0 ;'>",
                 tolower(listis), "</span>. Pour cet indicateur, la <strong>situation</strong> est donc <strong>sensiblement plus dégradée dans le territoire</strong> sélectionné que dans l'ensemble du pays.
                 L'indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df1$Indicateur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]),"</span> apparaît comme le <strong>principal point faible</strong> dans l'accès à l'emploi des femmes dans ce territoire avec une valeur de ",
                 round(df1$valeur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], 1),  df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]," contre ", round(df1$valeur_france[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)],1), df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], " en France.<br/><br/>", txtineg,"
                 L'indicateur <span style = 'background-color : #f2aca0 '>", tolower(df1$Indicateur[df1$diffFH == max(df1$diffFH)]),"</span> est celui pour lequel les inégalités sont le plus marquées entre les femmes et les hommes
                 avec une valeur de ", round(dfIneg$valeur[dfIneg$SEXE == "Femmes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les femmes contre ", round(dfIneg$valeur[dfIneg$SEXE == "Hommes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les hommes.
                 </p>"))
      }
      else{
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/><br/>
                 <p>Dans l’EPCI sélectionnée, ", length(indic_sup), " indicateurs d’accès à l’emploi des femmes excèdent largement la valeur française : <span style = 'background-color : #f2aca0 ;'>",
                 tolower(listis), "</span>. Pour ces indicateurs, la <strong>situation</strong> est donc <strong>sensiblement plus dégradée dans le territoire</strong> sélectionné que dans l'ensemble du pays.
                 L'indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df1$Indicateur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]),"</span> apparaît comme le <strong>principal point faible</strong> dans l'accès à l'emploi des femmes dans ce territoire avec une valeur de ",
                 round(df1$valeur[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], 1),  df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)]," contre ", round(df1$valeur_france[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)],1), df1$Unite[df1$valeur_b100 == max(df1$valeur_b100, na.rm = T)], " en France.<br/><br/>", txtineg,"
                 L'indicateur <span style = 'background-color : #f2aca0 '>", tolower(df1$Indicateur[df1$diffFH == max(df1$diffFH)]),"</span> est celui pour lequel les inégalités sont le plus marquées entre les femmes et les hommes
                 avec une valeur de ", round(dfIneg$valeur[dfIneg$SEXE == "Femmes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les femmes contre ", round(dfIneg$valeur[dfIneg$SEXE == "Hommes" & dfIneg$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]],1), df1$Unite[df1$SEXE == "Femmes" & df1$Indic == df1$Indic[df1$diffFH == max(df1$diffFH)]]," pour les hommes.
                 </p>"))
      }
    })
    
    output$comgr2 <- renderUI({
      
      df2 <-  filter(data_gr2, epci2018 == values$code_epci) %>%
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
          paste0("<p class = 'tttxt'>", titre, "</p><br/>
                 <p>Dans l’EPCI sélectionnée, tous les indicateurs de freins potentiels à l'accès à l'emploi des femmes ont une valeur largement supérieure à la valeur observée en France. Les femmes y rencontrent donc <strong>un très grand nombre d'obstacles dans leur parcours vers l'emploi</strong> comparativement à la situation française.<br/><br/>
                 L’indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df2$Indicateur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]), "</pan> constitue <strong>le frein potentiel le plus marquant</strong> avec une valeur de ",
                 round(df2$valeur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], 1),  df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]," contre ", round(df2$valeur_france[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)],1), df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], " en France.",
                 "</p>"))
      }
      # CAS 2 : TOUT INF 100
      else if(length(indic_sup) == 0){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/>
                 <p>Dans l’EPCI sélectionnée, aucun indicateur de freins potentiels à l'accès à l'emploi des femmes n'excède largement la valeur observée en France.<br/><br/>
                 L’indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df2$Indicateur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]), "</span> constitue néanmoins <strong>le principal frein potentiel</strong> pour l’accès à l’emploi des femmes dans ce territoire avec une valeur de ",
                 round(df2$valeur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], 1), " ", df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)],
                 "</p>"))
      }
      # CAS 3 : CLASSIQUE
      else if(length(indic_sup) == 1){
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/>
                 <p>Dans l’EPCI sélectionnée, ", length(indic_sup), " indicateur de freins potentiels à l’accès à l’emploi des femmes a une valeur largement supérieure à la valeur observée en France : <span style = 'background-color : #f2aca0 ;'>",
                 tolower(listis), "</span>. Ce <strong>frein potentiel</strong> à l'accès à l'emploi des femmes est donc <strong>sensiblement plus marqué dans le territoire</strong> sélectionné que dans l'ensemble du pays.<br/><br/>
                 L'indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df2$Indicateur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]),"</span> apparaît comme <strong>le principal frein potentiel</strong> pour l'accès à l'emploi des femmes dans ce territoire avec une valeur de ",
                 round(df2$valeur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], 1), " ", df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]," contre ", round(df2$valeur_france[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)],1), " ", df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], " en France.",
                 "</p>")
        )
      }
      else{
        HTML(
          paste0("<p class = 'tttxt'>", titre, "</p><br/>
                 <p>Dans l’EPCI sélectionnée, ", length(indic_sup), " indicateurs de freins potentiels à l’accès à l’emploi des femmes ont une valeur largement supérieure à la valeur observée en France : <span style = 'background-color : #f2aca0 ;'>",
                 tolower(listis), "</span>. Ces <strong>freins potentiels</strong> à l'accès à l'emploi des femmes sont donc <strong>sensiblement plus marqués dans le territoire</strong> sélectionné que dans l'ensemble du pays.<br/><br/>
                 L'indicateur <span style = 'background-color : #f2aca0 ;'>", tolower(df2$Indicateur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]),"</span> apparaît comme <strong>le principal frein potentiel</strong> pour l'accès à l'emploi des femmes dans ce territoire avec une valeur de ",
                 round(df2$valeur[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], 1), " ",  df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)]," contre ", round(df2$valeur_france[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)],1), " ", df2$Unite[df2$valeur_b100 == max(df2$valeur_b100, na.rm = T)], " en France.",
                 "</p>")
        )
      }
    })
      
      ################
      ## LES IMAGES ##
      ################
    
    lapply(1:14, function(i) {
      
      output[[paste0('crtch', i)]] <- renderUI({
        tags$a(tags$img(src =  paste0(meta$ID[i], ".jpg"), width = "100%"), href = liens$link[i], target="_blank")
      })
    })


  }
  

  
  # Run the application
  shinyApp(ui = ui, server = server)


