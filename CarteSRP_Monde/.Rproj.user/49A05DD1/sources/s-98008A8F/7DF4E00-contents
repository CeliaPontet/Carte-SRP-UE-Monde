#UI.R

# On realise le ui.R avec un format Shiny Dashboard.Separe en X grandes parties
shinyUI(
  dashboardPage(
    skin = "yellow",
    
    # -- 1: header
    dashboardHeader(
      title = "SRP",
      tags$li(class = "dropdown")
    ),
    
    # --2: Sidebar
    dashboardSidebar(
      #width = 250,
      sidebarMenu(id="menu", style="font-size: 15px",
                  # different onglet
                  menuItem("Home", tabName = "Home", icon = icon("home")),
                  menuItem("Données", tabName = "Donnees", icon = icon("database")),
                  menuItem("Créez votre graphique", tabName = "Description", icon = icon("bar-chart")),
                  menuItem("Créez votre carte", tabName = "Carte", icon = icon("map-o")),
                  menuItem("Créez votre carte d'évolution", tabName = "Carte_evol", icon = icon("percent")),
                  hr(),
                  #Sélection du critère
                  radioButtons("critere",label = "Critère",choices = data_srp %>% distinct(CRITERE) %>% arrange(CRITERE) %>% pull(CRITERE), selected = "Surface" ),
                  #Sélection de la ou des cultures concernées
                  conditionalPanel(
                    condition = "input.critere=='Surface'",
                    selectInput(
                      inputId = "culture_multiple",
                      label = "Culture(s)",
                      choices = list(
                            Oléagineux = c("Chanvre" , "Colza d'hiver" , "Lin oléagineux" , "Tournesol", "Soja"),
                            Protéagineux = c("Feverole" , "Lentille" , "Lupin" , "Pois chiche" , "Pois protéagineux" , "Soja"),
                            Autres = c("Betterave sucrière" , "Blé dur" , "Blé tendre" , "Maïs grain" , "Orge d'hiver" , "Orge de printemps" ,  "Sorgho grain")),
                      selected = "Colza d'hiver",
                      multiple = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.critere=='Production' || input.critere=='Rendement'",
                    selectInput(
                      inputId = "culture",
                      label = "Culture",
                      choices = list(
                        Oléagineux = c("Chanvre" , "Colza d'hiver" , "Lin oléagineux" , "Tournesol", "Soja"),
                        Protéagineux = c("Feverole" , "Lentille" , "Lupin" , "Pois chiche" , "Pois protéagineux" , "Soja"),
                        Autres = c("Betterave sucrière" , "Blé dur" , "Blé tendre" , "Maïs grain" , "Orge d'hiver" , "Orge de printemps" ,  "Sorgho grain")),
                      selected = "Colza d'hiver",
                      multiple = FALSE)
                  ),
                  #Sélection du niveau des données
                  selectInput("niveau", label = "Niveau", choices = data_srp %>% distinct(NIVEAU) %>% pull(NIVEAU), selected = "Département" ),
                  
                  # Logo + phrase + reseaux sociaux
                  br(),br(),br(),br(),
                  div(align="center",
                      img(src="terresinovia_logo.png", class='img-rounded', style="width:50%"),
                      br(),br(),
                      #HTML("<p>Made with <span style='font-size:100%;color:red;'>&hearts;</span> by CPT </a> </p>"),
                      br()
                  )
      )#SideBarMenu
    ),#dashboardSideBar
    
    # --3: Body
    dashboardBody(
      useShinyjs(),
      tabItems( #TabItems
        # ----------------------
        # SHEET 1 : HOME PAGE
        # ----------------------
        tabItem(tabName = "Home", class = "one",
                #Texte d'acceuil
                helpText("Accueil" , style="color:orange ; font-family: 'times'; font-size:24pt ; font-type:bold",align="left"),
                column(12,align="justify", style="font-family: 'times'; font-size:13pt",
                       fluidRow(
                         "Bienvenue dans le prototype d'outil de création de cartes SRP (Surfaces, Rendements et Productions)",
                         a("Terres Inovia",href="http://www.terresinovia.fr/",target="_blank()"),
                         ". Vous pouvez naviguer dans les onglets ci contre afin de paramétrer les differents graphiques et cartes souhaités."
                       ),
                       br(),
                       fluidRow(
                         "Dans l'onglet ",actionLink("link_to_tabpanel_Donnees", "Données")," les principales caractéristiques des données accessibles sont presentées : chiffres clés, dates de dernière actualisation ..."
                       ),
                       br(),
                       fluidRow(
                         "Dans l'onglet ",actionLink("link_to_tabpanel_Description", "Créez votre graphique"),"vous pourrez créer les graphiques d'évolution du paramètre souhaité, sur une plage d'années et un ensemble d'entités géographiques donnés."
                       ),
                       br(),
                       fluidRow(
                         "Dans les onglets ",actionLink("link_to_tabpanel_Carte", "Créez votre carte"),"et",actionLink("link_to_tabpanel_Carte_Evol", "Créez votre carte d'évolution") ,"vous pouvez créer la carte que vous souhaitez. A vous de choisir le critère a représenter, la ou les culture(s) concernée(s), le niveau de représentation. Vous pourrez également réaliser un zoom sur une zone géographique. Enfin les paramètres de mise en forme ne sont pas figés."
                       )
                )
        ), # TabItem 1

        # ----------------------
        # SHEET 2 : DONNEES
        # ----------------------

        tabItem(tabName = "Donnees",
                helpText("Données" , style="color:orange ; font-family: 'times'; font-size:24pt ; font-type:bold",align="left"),

                #Texte d'acceuil
                # Chiffres cles des donnees SRP
                fluidRow(
                  absolutePanel(top=120,right=0,left=250,width="1400px",draggable=TRUE,cursor="move",
                                fluidRow(
                                  column(9,infoBoxOutput(width=NULL, "infobox_sheet2.1")),
                                  column(3,infoBoxOutput(width=NULL, "infobox_sheet2.3"))
                                ),
                                fluidRow(
                                  column(3,valueBoxOutput(width=NULL, "infobox_sheet2.4")),
                                  column(5,valueBoxOutput(width=NULL, "infobox_sheet2.5")),
                                  column(4,valueBoxOutput(width=NULL, "infobox_sheet2.2"))
                                )
                  )
                )
                ,
                fluidRow(
                  column(12,align="justify", style="font-family: 'times'; font-size:13pt",
                         tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                         tags$p("Les données de cette base SRP (pour Surfaces – Rendements – Productions) proviennent d’Agreste, service de la statistique, de l’évaluation et de la prospective agricole du ministère français de l’agriculture et de l’alimentation. Elles sont mises en forme par Terres Inovia, institut technique de référence des professionnels de la filière des huiles et protéines végétales et de la filière chanvre . Elles concernent les Surfaces (exprimées en ha), Rendements (en q/ha ou t/ha dans les cas du chanvre et de la betterave a sucre) et Productions (en t) des principales espèces de grandes cultures de France métropolitaine. Les dix-sept grandes cultures présentées sont :"),
                         tags$ul(
                           tags$li(tags$b("Oléoprotéagineux"),(" : chanvre , colza d’hiver, féverole, lentille, lin oléagineux, lupin, pois chiche, pois protéagineux, soja et tournesol")),
                           tags$li(tags$b("Autres espèces de grandes cultures"),(" : betterave à sucre, blé dur, blé tendre, orge d’hiver, orge de printemps, maïs grain et sorgho grain"))
                         ),
                         tags$p("Les  données sont actualisées régulièrement :"),
                         tags$ul(
                           tags$li(tags$b("Colza d’hiver, féverole, lupin, pois protéagineux, soja et tournesol"),(" : mensuellement en cours de campagne avec une consolidation des données en novembre de l’année de récolte pour les cultures d’hiver et de printemps (colza d’hiver, féverole, lupin et pois) et en février de l’année suivant la récolte pour les cultures d’été (soja et tournesol).")),
                           tags$li(tags$b("Chanvre, lentille, lin oléagineux et pois chiche"),(" : annuellement à partir des données traitées par Agreste et issues des dossiers de déclaration par exploitation agricole pour l’accès aux aides de la Politique Agricole Commune (PAC).")),
                           tags$li(tags$b("Betterave à sucre, blé dur, blé tendre, orge d’hiver, orge de printemps, maïs grain et sorgho grain"),(" : annuellement à partir des données consolidées d’Agreste."))
                         ),
                         tags$p("Ces données sont exprimées à un grain départemental et sont  rassemblées sur trois niveaux : la région administrative, la région de développement de Terres Inovia (dénommée par la ville où est basé l’ingénieur régional de développement) et la zone de développement Terres Inovia (Centre et Ouest, Nord et Est ainsi que Sud).")
                  )
                ),
                fluidRow(
                  box(title="Données SRP totales", status="primary", solidHeader=TRUE, width=12, collapsible=TRUE,
                      DT::dataTableOutput("df.sheet.2",width="90%"),
                      downloadButton("downloadData.sheet.2", "Téléchager les données (.csv)")
                  )
                )
        ), # TabItem 2

        # ----------------------
        # SHEET 3 : Description
        # ----------------------

        tabItem(tabName = "Description",
                helpText("Description" , style="color:orange ; font-family: 'times'; font-size:24pt ; font-type:bold",align="left"),


                fluidRow(
                  box(title="Paramétrage",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                      column(4,
                             sliderInput("annees.sheet.3", "Plage des années",
                                         min = data_srp %>% summarise(min(ANNEE)) %>% pull(),
                                         max = data_srp %>% summarise(max(ANNEE)) %>% pull(),
                                         value = c(data_srp %>% summarise(min(ANNEE)) %>% pull(), data_srp %>% summarise(max(ANNEE)) %>% pull()),
                                         sep=""
                             )
                      ),
                      column(6,
                             selectInput(inputId = "entites.sheet.3", label = "Sélectionner les entités", choices = NULL, multiple = TRUE)
                              # checkboxInput("National.sheet.3","Ajouter la référence nationale",value=FALSE)
                      )
                  )

                ,




                  fluidRow(
                    column(6,
                           box(title="Graphique & Données",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                               tabBox(width=12,
                                      tabPanel(title="Graphique",
                                               plotOutput("graph.sheet.3", click = "plot3_click"),
                                               # plotlyOutput("graph.sheet.3",width="100%",height="75%"),
                                               # ggiraphOutput("graph.sheet.3"),
                                               hr(),
                                               fluidRow(
                                                 column(1,dropdownButton(circle = TRUE, status = "primary", icon = icon("question"), size = "xs","Pour assurer une excellente qualité, choisir pdf ou tiff.")),
                                                 column(5,radioButtons(inputId="save.type.sheet.3",label="Type de fichier",choices=list("pdf","png","jpeg","tiff"),inline=TRUE))
                                               ),
                                               downloadButton(outputId="down.sheet.3",label="Télécharger le graphique")
                                      ),
                                      tabPanel(title="Données",status = "primary", solidHeader = FALSE,
                                               DT::dataTableOutput("df.sheet.3",width="90%"),
                                               downloadButton("downloadData.sheet.3", "Téléchager les données (.csv)")
                                      )
                               )
                           )),
                    column(6,
                           box(title="Modifier les paramètres graphiques",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                               tabBox(width = 12,


                                      ########### Echelle #############

                                      tabPanel("Echelle",
                                               h3("Echelle"),
                                               radioButtons("y.ajust.sheet.3","Ajuster l'axe Y",
                                                                   choices=c("Ajusté aux données"="ajust","Echelle personnalisée"="perso"),
                                                                   inline = TRUE),
                                               fluidRow(
                                                 column(4,numericInput("ymin.sheet.3","Min",value=data_srp %>% summarise(min(VALUE,na.rm=TRUE)) %>% pull())),
                                                 column(4,numericInput("ymax.sheet.3","Max",value=data_srp %>% summarise(max(VALUE,na.rm=TRUE)) %>% pull()))
                                               ),
                                               hr(),
                                               h3("Habillage texte"),
                                               checkboxInput("ajout.text.sheet.3","Ajouter les valeurs sur le graphique", value = FALSE),
                                               fluidRow(
                                                 column(4,numericInput("taille.text.sheet.3","Taille",min=0,max=20,value=4,step = 0.25)),
                                                 column(4,numericInput("nb.dec.sheet.3","Nombre de décimales",value=0,step=1)),
                                                 column(4,colourInput("col.text.sheet.3","Couleur",value="#665E5E"),
                                                        checkboxInput("col.group.sheet.3","Appliquer les couleurs des entités",value = FALSE))
                                               )
                                      ),


                                      ########### courbes de tendance ###########

                                      tabPanel(title="Tendances",status = "primary", solidHeader = FALSE,
                                               checkboxInput("trends.sheet.3","Afficher les courbes de tendance ?",value = FALSE),
                                               fluidRow(
                                                 column(6,selectInput("method.sheet.3","Type de tendance",choices=c("Lineaire"="lm","Lissage"="loess"), selected="loess")),
                                                 column(6, sliderInput("lwd.trend.sheet.3","Epaisseur courbe de tendance",value=0.5,min=0,max=4,step=0.1))
                                               ),
                                               checkboxInput("se.sheet.3","Afficher les intervalles de confiance",value=FALSE),
                                               fluidRow(column(4,numericInput("level.sheet.3","Niveau de l'intervalle de confiance",value=0.95,min=0,max=1,step=0.01)))

                                               ),

                                      ########### Couleurs #############

                                      tabPanel("Courbes",
                                               h3("Palette de couleur"),
                                               radioButtons("mode.couleur.sheet.3",label=NULL,choices=c("Palette prédéfinie"="Palette", "Palette personnalisée"="Perso"),inline = TRUE),
                                               hr(),
                                               fluidRow(
                                                 column(6,hidden(
                                                   pickerInput(
                                                     inputId = "palette.brewer.sheet.3", label = "Palette de couleur",
                                                     choices = colors_pal$Qualitative, selected = "Set2", width = "100%",
                                                     choicesOpt = list(
                                                       content = sprintf(
                                                         "<div style='width:100%%;padding:5px;border-radius:4px;background:%s;color:%s'>%s</div>",
                                                         unname(background_pals[10:17]), colortext_pals[10:17], names(background_pals[10:17])
                                                       )
                                                     )
                                                   )
                                                   # selectInput("palette.brewer.sheet.3",
                                                   #                           label="Palette de couleur",
                                                   #                           choices=c("Set1","Set2","Set3","Pastel1","Pastel2","Paired","Dark2","Accent"),
                                                   #                           selected="Set2")
                                                                 )),

                                                 column(6,hidden(radioButtons("palette.direction.sheet.3",
                                                                              "Direction de la palette",
                                                                              choices=c("Origine"=1, "Aléatoire"=0))))
                                               ),
                                               fluidRow(verbatimTextOutput("couleurs")),
                                               # fluidRow(
                                               #  uiOutput("CouleursInputs")
                                               # ),
                                               fluidRow(
                                                 textInput("vecteur.couleurs.sheet.3","Vecteur de couleurs",value=""),
                                                 colourInput("couleur.sheet.3","Aide choix des couleurs",value = "#FFF8DC")
                                               ),
                                               fluidRow(
                                                 column(6,
                                                        fluidRow(h5("Voici quelles seront les couleurs appliquées")),
                                                        fluidRow(plotOutput("demo.palette.sheet.3",width = 300,height = 50))
                                                        ),
                                                 column(4,hidden(actionButton("sample.sheet.3","Tirage aléatoire dans la palette")))
                                               ),
                                               hr(),
                                               h3("Les points et les lignes"),
                                               fluidRow(
                                                 column(6,sliderInput(inputId="size.point.sheet.3",label="Taille des points",value=2,min=0,max=10,step=0.5)),
                                                 column(6,selectInput("linetype.sheet.3","Type de ligne",choices=c("aucune"="blank","________"="solid","- - - - - - - - -"="dashed",". . . . . . . . . . ."="dotted","._._._._._."="dotdash","__ __ __ __"="longdash",".__.__.__."="twodash"),selected="solid"))
                                               ),
                                               hr(),
                                               h3("Graphique simple ou multiple"),
                                               checkboxInput("facet_wrap.sheet.3","Séparer les entités géographiques",value=FALSE)
                                      ),


                                      ########### Allure générale ###########

#
                                      tabPanel(title="Allure générale",status = "primary", solidHeader = FALSE,
                                               h3("Grilles"),
                                               tabBox(width = 12,
                                                      tabPanel("Grille principale",
                                                               fluidRow(column(4,numericInput("min.grid.major.sheet.3","De :",value=1990)),
                                                               column(4,numericInput("max.grid.major.sheet.3","à :",value=2020)),
                                                               column(4,numericInput("pas.grid.major.sheet.3","par pas de :",value=5))),
                                                               fluidRow(column(4,sliderInput("taille.grid.major.sheet.3","Taille",value=0.2,min=0,max=5,step=0.1)),
                                                               column(4,colourInput("couleur.grid.major.sheet.3","Couleur",value="#CFCFCF")),
                                                               column(4,selectInput("linetype.grid.major.sheet.3","Type",
                                                                                    choices=c("________"="solid","- - - - - - - - -"="dashed",". . . . . . . . . . ."="dotted","._._._._._."="dotdash","__ __ __ __"="longdash",".__.__.__."="twodash"))))
                                                      ),
                                                      tabPanel("Grille secondaire",
                                                               fluidRow(column(4,numericInput("min.grid.minor.sheet.3","De :",value=1990)),
                                                               column(4,numericInput("max.grid.minor.sheet.3","à :",value=2020)),
                                                               column(4,numericInput("pas.grid.minor.sheet.3","par pas de :",value=1))),
                                                               fluidRow(column(4,sliderInput("taille.grid.minor.sheet.3","Taille",value=0.1,min=0,max=5,step=0.1)),
                                                               column(4,colourInput("couleur.grid.minor.sheet.3","Couleur",value="#DEDEDE")),
                                                               column(4,selectInput("linetype.grid.minor.sheet.3","Type",
                                                                                    choices=c("________"="solid","- - - - - - - - -"="dashed",". . . . . . . . . . ."="dotted","._._._._._."="dotdash","__ __ __ __"="longdash",".__.__.__."="twodash"),
                                                                                    selected="dashed"))))
                                               ),
                                               hr(),
                                               h3("Axes"),
                                               tabBox(width = 12,
                                                      tabPanel("Axe X",
                                                               column(4,sliderInput("taille.axex.sheet.3","Taille",value=0.2,min=0,max=5,step=0.1)),
                                                               column(4,colourInput("couleur.axex.sheet.3","Couleur",value="#CFCFCF"))
                                                               ),
                                                      tabPanel("Axe Y",
                                                               column(4,sliderInput("taille.axey.sheet.3","Taille",value=0.2,min=0,max=5,step=0.1)),
                                                               column(4,colourInput("couleur.axey.sheet.3","Couleur",value="#CFCFCF"))
                                                        )
                                               ),
                                               hr(),
                                               h3("Fond"),
                                               fluidRow(column(3,colourInput("couleur.fond.sheet.3", "Couleur", "white",returnName = TRUE))),
                                               hr(),
                                               h3("Taille du graphique"),
                                               fluidRow(
                                                 column(6,sliderInput("width.sheet.3","Largeur (cm) :",value=23,min=1,max=50,step=0.1)),
                                                 column(6,sliderInput("height.sheet.3","Hauteur (cm) :",value=10,min=1,max=50,step=0.1))
                                               )
                                      ),


                                      ######## Titre #########

                                      tabPanel(title="Titres",status = "primary", solidHeader = FALSE,
                                               h3("Titre"),
                                               textInput(inputId="titre.sheet.3",
                                                           label="Modifier le titre",
                                                           value=NULL),

                                               fluidRow(
                                                 column(3,sliderInput("taille.titre.sheet.3",
                                                                      label="Taille",
                                                                      value=16,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.titre.sheet.3",
                                                                      label="Alignement",
                                                                      value=0,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.titre.sheet.3",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),

                                                 column(4,radioButtons("style.titre.sheet.3",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic")))
                                               ),

                                               hr(),
                                               h3("Sous-titre"),
                                               textInput(inputId="sous.titre.sheet.3",
                                                                  label="Modifier le sous-titre",
                                                                  value=NULL),
                                               fluidRow(
                                                 column(3,sliderInput("taille.sous.titre.sheet.3",
                                                                      label="Taille",
                                                                      value=12,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.sous.titre.sheet.3",
                                                                      label="Alignement",
                                                                      value=0,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.sous.titre.sheet.3",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),

                                                 column(4,radioButtons("style.sous.titre.sheet.3",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic")))
                                               ),
                                               hr(),
                                               h3("Source"),
                                               textInput(inputId="text1.source.sheet.3",
                                                           label="Modifier la source (1ère ligne)",
                                                           value="Source : Terres Inovia et Terres Univia d'après les données d'Agreste"),
                                                 textInput(inputId="text2.source.sheet.3",
                                                           label="Modifier la source (2nde ligne)",
                                                           value="(Ministère de l'Agriculture et de l'Alimentation)"),
                                               fluidRow(
                                                 column(3,sliderInput("taille.source.sheet.3",
                                                                      label="Taille",
                                                                      value=9,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.source.sheet.3",
                                                                      label="Alignement",
                                                                      value=1,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.source.sheet.3",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),

                                                 column(4,radioButtons("style.source.sheet.3",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"),
                                                                       selected = "italic"))
                                               )
                                      ),

                                      ########### Axes ###########

                                      tabPanel("Axes",
                                               h3("Titres des axes"),
                                               tabBox(width = 12,
                                                      tabPanel("Axe X",
                                                               fluidRow(column(4,textInput("xlab.sheet.3", label="", value="Années")),
                                                               column(4,sliderInput("taille.titrex.sheet.3","Taille",value=10,min=0,max=32,step=1)),
                                                               column(4,colourInput("col.titrex.sheet.3", "Couleur", "#665E5E",returnName = FALSE))
                                                                )),
                                                      tabPanel("Axe Y",
                                                               fluidRow(column(4,textInput("ylab.sheet.3","",value=NULL)),
                                                               column(4,sliderInput("taille.titrey.sheet.3","Taille",value=10,min=0,max=32,step=1)),
                                                               column(4,colourInput("col.titrey.sheet.3", "Couleur", "#665E5E",returnName = FALSE)))
                                                 )
                                               ),
                                               hr(),
                                               h3("Texte des axes"),
                                               tabBox(width = 12,
                                                 tabPanel("Axe X",
                                                          fluidRow(column(3,sliderInput("taille.labelx.sheet.3","Taille",value=8,min=0,max=32,step=1)),
                                                                   column(3,sliderInput("orientation.labelx.sheet.3","Orientation",value=0,min=0,max=90,step=1)),
                                                                   column(3,sliderInput("alignement.labelx.sheet.3", label="Alignement", value=0.5,min=0,max=1,step=0.1)),
                                                                   column(3,colourInput("col.axex.sheet.3", "Couleur", "#665E5E",returnName = FALSE)))
                                                 ),
                                                 tabPanel("Axe Y",
                                                          fluidRow(column(3,sliderInput("taille.labely.sheet.3","Taille",value=8,min=0,max=32,step=1)),
                                                                 column(3,colourInput("col.axey.sheet.3", "Couleur", "#665E5E",returnName = FALSE)))
                                                 )
                                               )
                                      ),

                                      ########### Légende ###########

                                      tabPanel("Légende",
                                               h3("Général"),
                                               textInput(inputId="leg.titre.sheet.3",
                                                         label="Titre de la légende",
                                                         value=NULL),
                                               radioButtons("leg.position.sheet.3",
                                                            label="Position",
                                                            choices=c("Droite"="right","Gauche"="left","Haut"="top","Bas"="bottom","Aucune"="none"),inline=TRUE),
                                               hr(),
                                               h3("Titre de la légende"),
                                               fluidRow(
                                                 column(4,sliderInput("leg.titre.size.sheet.3",label="Taille",value=10,min=0,max=32,step=1)),
                                                 column(4,radioButtons("leg.titre.style.sheet.3","Style", choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"))),
                                                 column(4,colourInput("leg.titre.col.sheet.3", "Couleur", "#665E5E",returnName = FALSE))
                                               ),
                                               hr(),
                                               h3("Elements de la légende"),
                                               fluidRow(
                                                 column(4,sliderInput("leg.elements.size.sheet.3",
                                                                      label="Taille",
                                                                      value=8,min=0,max=32,step=1)),
                                                 column(4,radioButtons("leg.elements.style.sheet.3","Style", choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"))),
                                                 column(4,colourInput("leg.elements.col.sheet.3", "Couleur", "#665E5E",returnName = FALSE)))
                                      ),

                                      # ########### Logos TI et TU ###########


                                      tabPanel("Logos",
                                               h3("Logo TI"),
                                               checkboxInput("logo.ti.sheet.3","Afficher le logo Terres Inovia",value=TRUE),
                                               fluidRow(
                                                 column(4,hidden(sliderInput("size.logo.ti.sheet.3",
                                                                             label="Taille",
                                                                             value=0.10,min=0,max=0.5,step=0.01))),
                                                 column(4,hidden(sliderInput("posx.logo.ti.sheet.3",
                                                                             label="Position X",
                                                                             value=NULL,min=NULL,max=NULL,step=1))),
                                                 column(4,hidden(sliderInput("posy.logo.ti.sheet.3",
                                                                             label="Position Y",
                                                                             value=NULL,min=NULL,max=NULL,step=1)))),
                                               hr(),
                                               h3("Logo TU"),
                                               checkboxInput("logo.tu.sheet.3","Afficher le logo Terres Univia",value=TRUE),
                                               fluidRow(
                                                 column(4,hidden(sliderInput("size.logo.tu.sheet.3",
                                                                             label="Taille",
                                                                             value=0.10,min=0,max=0.5,step=0.01))),
                                                 column(4,hidden(sliderInput("posx.logo.tu.sheet.3",
                                                                             label="Position X",
                                                                             value=NULL,min=NULL,max=NULL,step=1))),
                                                 column(4,hidden(sliderInput("posy.logo.tu.sheet.3",
                                                                             label="Position Y",
                                                                             value=NULL,min=NULL,max=NULL,step=1)))))

                               )
                           )
                    )
                  )
                )
        ), # TabItem 3

        # ----------------------
        # SHEET 4 : CARTE
        # ----------------------

        tabItem(tabName = "Carte",
                helpText("Créez votre graphique" , style="color:orange ; font-family: 'times'; font-size:24pt ; font-type:bold",align="left"),

                fluidRow(
                  box(title="Paramétrage",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                      column(1,
                             numericInput("annee.sheet.4",label="Année",value=max(data_srp$ANNEE,na.rm=TRUE),min=min(data_srp$ANNEE,na.rm=TRUE),max=max(data_srp$ANNEE))),

                      column(11,
                             fluidRow(
                               selectInput("choix.entite.sheet.4", "Sélectionnez les entités géographiques à représenter",
                                                  choices = data_srp %>% filter(NIVEAU=="Département") %>% distinct(ENTITE) %>% pull(),
                                                  selected = data_srp %>% filter(NIVEAU=="Département") %>% distinct(ENTITE) %>% pull(),
                                                  multiple =T),
                               # pickerInput(inputId = "choix.entite.sheet.4", label = "Sélectionnez les entités géographiques à représenter",
                               #   choices = data_srp %>% filter(NIVEAU=="Département") %>% distinct(ENTITE) %>% pull(),
                               #   selected = data_srp %>% filter(NIVEAU=="Département") %>% distinct(ENTITE) %>% pull(),
                               #   options = list(`actions-box` = TRUE),multiple = TRUE),
                               # actionButton("valider.entite.sheet.4","Valider"),
                               materialSwitch(inputId = "corse.sheet.4",label = "Corse",value = FALSE,status = "primary"))
                               # switchInput(inputId = "corse.sheet.4", label = "Corse"))
                               # checkboxInput("corse.sheet.4","Afficher la Corse",value=FALSE))
                      )
                  )
                ),


                fluidRow(
                  column(6,
                         box(title="Carte & Données",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                             tabBox(width=12,
                                    tabPanel("Carte",
                                             plotOutput("map.sheet.4",width="100%",height="480px", click = "plot4_click"),
                                             fluidRow(
                                               column(1,dropdownButton(circle = TRUE, status = "primary", icon = icon("question"), size = "xs","Pour assurer une excellente qualité, choisir pdf ou tiff.")),
                                               column(5,radioButtons(inputId="save.type.sheet.4",label="Type de fichier",choices=list("pdf","png","jpeg","tiff"),inline=TRUE))
                                             ),
                                             downloadButton(outputId="down.sheet.4",label="Telecharger la carte")
                                             # plotlyOutput("map.sheet.4",width="100%",height="480px"),
                                             # ggiraphOutput("map.sheet.4",width="100%",height="480px"),
                                    ),
                                    tabPanel("Données",
                                             dataTableOutput("df.sheet.4",width="90%"),
                                             downloadButton("downloadData.sheet.4", "Téléchager les données (.csv)")
                                    )
                             )
                         )
                  ),


                  column(6,
                         fluidRow(
                           box(title="Modifier les paramètres graphiques",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                               tabBox(width=12,



                                      ########### Echelle #############

                                      tabPanel("Echelle",
                                               h3("Type d'échelle"),
                                               radioButtons("type.echelle.sheet.4",
                                                            label=NULL,
                                                            choices=c("Continue","Discrète"),inline = TRUE),
                                               hr(),
                                               h3("Echelle"),
                                               fluidRow(column(3,numericInput("nb.interv.sheet.4","Nombre d'intervalles",min=1,value=5))),
                                               textInput("interv.sheet.4","Indiquer les interavalles souhaités",value=paste(rep(0,5),collapse = "/")),
                                               fluidRow(column(4,actionButton("decoupage.auto.sheet.4","Découpage auto")),
                                                        column(8,tableOutput("effectifs.sheet.4"))),
                                               fluidRow(
                                                 column(4,hidden(radioButtons("leg.ajust.sheet.4","Ajustement de l'échelle", choices=c("Echelle ajustée aux données","Echelle personnalisée"),inline = FALSE))),
                                                 column(6,tableOutput("summary4"))),
                                      
                                               fluidRow(
                                                 column(4,
                                                        fluidRow(numericInput("leg.gamme.min.sheet.4","Minimum",value=NULL)),
                                                        fluidRow(numericInput("leg.gamme.max.sheet.4","Maximum",value=NULL)),
                                                        fluidRow(actionButton("maj.gamme.sheet.4","Valider la gamme"))
                                                 )),
                                               
                                               hr(),
                                               h3("Habillage texte"),
                                               checkboxInput("ajout.text.sheet.4","Ajouter les valeurs sur la carte", value = FALSE),
                                               fluidRow(
                                                 column(4,numericInput("taille.text.sheet.4","Taille",min=0,max=20,value=2.5,step = 0.25)),
                                                 column(4,numericInput("nb.dec.sheet.4","Nombre de décimales",value=0,step=1)),
                                                 column(4,colourInput("col.text.sheet.4","Couleur",value="#665E5E"))
                                               )
                                      ),



                                      ########### Couleurs #############

                                      tabPanel("Couleurs",
                                               h3("Palette"),
                                               radioButtons("mode.couleur.sheet.4",label=NULL,choices=c("Palette prédéfinie"="Palette", "Gradient personnalisé"="Gradient"),inline = TRUE),
                                               hr(),

                                               fluidRow(
                                                 column(6,hidden(pickerInput(
                                                   inputId = "palette.brewer.sheet.4", label = "Palette de couleur",
                                                   choices = colors_pal$Sequential, selected = "GnBu", width = "100%",
                                                   choicesOpt = list(
                                                     content = sprintf(
                                                       "<div style='width:100%%;padding:5px;border-radius:4px;background:%s;color:%s'>%s</div>",
                                                       unname(background_pals[18:35]), colortext_pals[18:35], names(background_pals[18:35])
                                                     )
                                                   )
                                                 ))),
                                                 # column(6,hidden(selectInput("palette.brewer.sheet.4",
                                                 #                             label="Palette de couleur",
                                                 #                             choices=c("BuGn : blanc>VERT"="BuGn","BuPu : blanc>VIOLET"="BuPu","GnBu : vert>BLEU"="GnBu","Greens : Verts"="Greens","Greys : Gris"="Greys","Oranges"="Oranges","OrRd : orange>ROUGE"="OrRd","PuBu : violet>BLEU"="PuBu","PuBuGn : violet>Bleu>VERT"="PuBuGn","PuRd : violet>ROUGE"="PuRd","Purples : Violets"="Purples","RdPu : rouge>VIOLET"="RdPu","Reds : Rouges"="Reds","YlGn : jaune>VERT"="YlGn","YlGnBu : jaune>Vert>BLEU"="YlGnBu","YlOrBr : jaune>Orange>BRUN"="YlOrBr","YlOrRd : jaune>Orange>ROUGE"="YlOrRd")[17:1],
                                                 #                             selected="GnBu"))),
                                                 column(6,hidden(radioButtons("palette.direction.sheet.4",
                                                                              "Direction de la palette",
                                                                              choices=c("Clair=faible / Foncé=élevé"=1, "Clair=élevé / Foncé=faible"=-1))))
                                               ),
                                               fluidRow(
                                                 column(6,colourInput("couleur1.map.sheet.4","Couleur faible",value = "#FFF8DC")),
                                                 column(6,colourInput("couleur2.map.sheet.4","Couleur élevé",value = "#00868B"))
                                               ),
                                               fluidRow(
                                                 column(6,plotOutput("demo.palette.sheet.4",width = 300,height = 50)),
                                                 column(6,actionButton("valider.palette.sheet.4","Valider la palette"))
                                               ),
                                               hr(),
                                               h3("Données manquantes"),
                                               fluidRow(column(4,colourInput("col.na.sheet.4","Couleur des NA",value = "#B8ADAD"))),
                                               hr(),
                                               h3("Contours"),
                                               fluidRow(
                                                 column(4,colourInput("col.contour.sheet.4",
                                                                      "Couleur",value = "#B8ADAD")),
                                                 column(4,sliderInput("lwd.contour.sheet.4",
                                                                      label="Epaisseur",
                                                                      min=0,max=1,value=0.2,step = 0.05))
                                               )

                                      ),



                                      ########### Titre #############

                                      tabPanel("Titre",
                                               h3("Titre"),
                                               textInput(inputId="titre.sheet.4",
                                                           label="Modifier le titre",
                                                           value=NULL),
                                               fluidRow(
                                                 column(3,sliderInput("taille.titre.sheet.4",
                                                                      label="Taille",
                                                                      value=14,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.titre.sheet.4",
                                                                      label="Alignement",
                                                                      value=0,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.titre.sheet.4",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),

                                                 column(4,radioButtons("style.titre.sheet.4",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic")))
                                               ),
                                               hr(),
                                               h3("Sous-titre"),
                                               textInput(inputId="sous.titre.sheet.4",
                                                           label="Modifier le sous-titre",
                                                           value=NULL),
                                               fluidRow(
                                                 column(3,sliderInput("taille.sous.titre.sheet.4",
                                                                      label="Taille",
                                                                      value=11,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.sous.titre.sheet.4",
                                                                      label="Alignement",
                                                                      value=0,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.sous.titre.sheet.4",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),

                                                 column(4,radioButtons("style.sous.titre.sheet.4",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic")))
                                               ),
                                               hr(),
                                               h3("Source"),
                                               textInput(inputId="text1.source.sheet.4",
                                                           label="Modifier la source (1ère ligne)",
                                                           value="Source : Terres Inovia et Terres Univia d'après les données d'Agreste"),
                                                 textInput(inputId="text2.source.sheet.4",
                                                           label="Modifier la source (2nde ligne)",
                                                           value="(Ministère de l'Agriculture et de l'Alimentation)"),
                                               fluidRow(
                                                 column(3,sliderInput("taille.source.sheet.4",
                                                                      label="Taille",
                                                                      value=8,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.source.sheet.4",
                                                                      label="Alignement",
                                                                      value=1,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.source.sheet.4",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),

                                                 column(4,radioButtons("style.source.sheet.4",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"),
                                                                       selected = "italic"))
                                               )

                                      ),

                                      ########### Légende ###########

                                      tabPanel("Légende",
                                               h3("Général"),
                                               textInput(inputId="leg.titre.sheet.4",
                                                         label="Titre de la légende",
                                                         value=NULL),
                                               radioButtons("leg.position.sheet.4",
                                                            label="Position",
                                                            choices=c("Droite"="right","Gauche"="left","Haut"="top","Bas"="bottom","Aucune"="none"),inline=TRUE),
                                               hr(),
                                               h3("Titre de la légende"),
                                               fluidRow(
                                                 column(4,sliderInput("leg.titre.size.sheet.4",label="Taille",value=10,min=0,max=32,step=1)),
                                                 column(4,radioButtons("leg.titre.style.sheet.4","Style", choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"))),
                                                 column(4,colourInput("leg.titre.col.sheet.4", "Couleur", "#665E5E",returnName = FALSE))
                                               ),
                                               hr(),
                                               h3("Chiffres de la légende"),
                                               fluidRow(
                                                 column(4,sliderInput("leg.chiffre.size.sheet.4",
                                                                      label="Taille",
                                                                      value=8,min=0,max=32,step=1)),
                                                 column(4,radioButtons("leg.chiffre.style.sheet.4","Style", choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"))),
                                                 column(4,colourInput("leg.chiffre.col.sheet.4", "Couleur", "#665E5E",returnName = FALSE)))
                                      ),

                                      ########### Logos TI et TU ###########


                                      tabPanel("Logos",
                                               h3("Logo TI"),
                                               checkboxInput("logo.ti.sheet.4","Afficher le logo Terres Inovia",value=TRUE),
                                               fluidRow(
                                                 column(4,hidden(sliderInput("size.logo.ti.sheet.4",
                                                                             label="Taille",
                                                                             value=0.15,min=0,max=0.5,step=0.01))),
                                                 column(4,hidden(sliderInput("posx.logo.ti.sheet.4",
                                                                             label="Position X",
                                                                             value=900310,min=90000,max=1300000,step=1))),
                                                 column(4,hidden(sliderInput("posy.logo.ti.sheet.4",
                                                                             label="Position Y",
                                                                             value=7200660,min=6000000,max=7500000,step=1)))),
                                               hr(),
                                               h3("Logo TU"),
                                               checkboxInput("logo.tu.sheet.4","Afficher le logo Terres Univia",value=TRUE),
                                               fluidRow(
                                                 column(4,hidden(sliderInput("size.logo.tu.sheet.4",
                                                                             label="Taille",
                                                                             value=0.15,min=0,max=0.5,step=0.01))),
                                                 column(4,hidden(sliderInput("posx.logo.tu.sheet.4",
                                                                             label="Position X",
                                                                             value=1080310,min=90000,max=1300000,step=1))),
                                                 column(4,hidden(sliderInput("posy.logo.tu.sheet.4",
                                                                             label="Position Y",
                                                                             value=7200660,min=6000000,max=7500000,step=1))))),

                                      ############### Autre ###########

                                      tabPanel("Autres",
                                               h3("Flèche Nord (en cours de développement)")
                                               # checkboxInput("north.arrow.sheet.4","Afficher la flèche nord",value = FALSE),
                                               # fluidRow(
                                               #   column(2,numericInput("type.north.arrow.sheet.4",
                                               #                         label="Type de flèche nord",
                                               #                         min=1,max=18,value=3)),
                                               #   column(3,sliderInput("taille.north.arrow.sheet.4","Taille",min=0,max=1,value=0.1)),
                                               #   column(3,radioButtons("location.north.arrow.sheet.4",
                                               #                         "Position",
                                               #                         choices=c("Bas-gauche"="bottomleft","Bas-droite"="bottomright","Haut-droite"="topright","Haut-gauche"="topleft")))
                                               # ),
                                               # plotOutput("north.arrow.symbols4",width = 400,height = 290)
                                               )
                               )
                           )
                         )

                  )
                )
        ), # TabItem 4












            # ----------------------
            # SHEET 5 : CARTE Ecart
            # ----------------------


        tabItem(tabName = "Carte_evol",
                helpText("Créez votre graphique d'évolution" , style="color:orange ; font-family: 'times'; font-size:24pt ; font-type:bold",align="left"),
                fluidRow(
                  box(title="Paramétrage",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                      column(2,
                             fluidRow(
                               numericInput("annee1.sheet.5",label="Année 1",value=max(data_srp$ANNEE,na.rm=TRUE)-1,min=min(data_srp$ANNEE,na.rm=TRUE),max=max(data_srp$ANNEE)),
                               numericInput("annee2.sheet.5",label="Année 2",value=max(data_srp$ANNEE,na.rm=TRUE),min=min(data_srp$ANNEE,na.rm=TRUE),max=max(data_srp$ANNEE)),
                               # radioButtons("type.evolution.sheet.5","Représenter l'évolution :",choices=c("En pourcentage"="pourc","Brute"="brute"))
                               radioGroupButtons(
                                 inputId = "type.evolution.sheet.5",
                                 label = "Représenter l'évolution :", 
                                 choices = c("En pourcentage"="pourc","Brute"="brute"),
                                 status = "primary"
                               ))
                      ),
                      column(10,
                             selectInput("choix.entite.sheet.5", "Sélectionnez les entités géographiques à représenter",
                                         choices = data_srp %>% filter(NIVEAU=="Département") %>% distinct(ENTITE) %>% pull(),
                                         selected = data_srp %>% filter(NIVEAU=="Département") %>% distinct(ENTITE) %>% pull(),
                                         multiple =T),
                             # actionButton("valider.entite.sheet.5","Valider"),
                             materialSwitch(inputId = "corse.sheet.5",label = "Corse",value = FALSE,status = "primary"))
                             # checkboxInput("corse.sheet.5","Afficher la Corse",value=FALSE))
                  )
                ),
                fluidRow(
                  column(6,
                         box(title="Carte & Données",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                             tabBox(width=12,
                                    tabPanel("Carte",
                                             column(12,plotOutput("map.sheet.5",width="100%",height="480px", click = "plot5_click")),
                                             fluidRow(
                                               column(1,dropdownButton(circle = TRUE, status = "primary", icon = icon("question"), size = "xs","Pour assurer une excellente qualité, choisir pdf ou tiff.")),
                                               column(5,radioButtons(inputId="save.type.sheet.5",label="Type de fichier",choices=list("pdf","png","jpeg","tiff"),inline=TRUE))
                                             ),

                                             downloadButton(outputId="down.sheet.5",label="Telecharger la carte")
                                             # plotlyOutput("map.sheet.5",width="100%",height="480px"),
                                             # ggiraphOutput("map.sheet.5",width="100%",height="480px"),
                                    ),
                                    tabPanel("Données",
                                             dataTableOutput("df.sheet.5",width="60%"),
                                             downloadButton("downloadData.sheet.5", "Téléchager les données (.csv)")
                                    )
                             )
                         )
                  ),
                  column(6,
                         fluidRow(
                           box(title="Modifier les paramètres graphiques",status="primary",solidHeader=TRUE,width=12,collapsible=TRUE,
                               tabBox(width=12,



                                      ########### Echelle #############

                                      tabPanel("Echelle",
                                               h3("Type d'échelle"),
                                               radioButtons("type.echelle.sheet.5",
                                                            label=NULL,
                                                            choices=c("Continue","Discrète"),inline = TRUE),
                                               hr(),
                                               h3("Echelle"),
                                               fluidRow(column(2,numericInput("nb.interv.sheet.5","Nombre d'intervalles",min=1,value=5))),
                                               textInput("interv.sheet.5","Indiquer les interavalles souhaités",value=paste(rep(0,5),collapse = "/")),
                                               fluidRow(column(4,actionButton("decoupage.auto.sheet.5","Découpage auto")),
                                                        column(8,tableOutput("effectifs.sheet.5"))),
                                               fluidRow(
                                                 column(4,hidden(radioButtons("leg.ajust.sheet.5","Ajustement de l'échelle", choices=c("Echelle ajustée aux données","Echelle personnalisée"),inline = TRUE))),
                                                 column(6,tableOutput("summary5"))),
                                               fluidRow(
                                                 column(4,numericInput("leg.gamme.min.sheet.5","Minimum",value=NULL)),
                                                 column(4,numericInput("leg.gamme.max.sheet.5","Maximum",value=NULL)),
                                                 column(4,actionButton("maj.gamme.sheet.5","Valider la gamme"))
                                               ),
                                               hr(),
                                               h3("Habillage texte"),
                                               checkboxInput("ajout.text.sheet.5","Ajouter les valeurs sur la carte", value = FALSE),
                                               fluidRow(
                                                 column(4,numericInput("taille.text.sheet.5","Taille",min=0,max=20,value=2.5,step = 0.25)),
                                                 column(4,numericInput("nb.dec.sheet.5","Nombre de décimales",value=0,step=1)),
                                                 column(4,colourInput("col.text.sheet.5","Couleur",value="#665E5E"))
                                               )
                                      ),
                                      
                                      ########### Couleurs #############

                                      tabPanel("Couleurs",
                                               h3("Palette"),
                                               radioButtons("mode.couleur.sheet.5",label=NULL,choices=c("Palette prédéfinie"="Palette", "Gradient personnalisé"="Gradient"),inline = TRUE),
                                               hr(),

                                               fluidRow(
                                                 column(6,hidden(
                                                   pickerInput(
                                                     inputId = "palette.brewer.sheet.5", label = "Palette de couleur",
                                                     choices = colors_pal[c(1,3)], selected = "RdYlGn", width = "100%",
                                                     choicesOpt = list(
                                                       content = sprintf(
                                                         "<div style='width:100%%;padding:5px;border-radius:4px;background:%s;color:%s'>%s</div>",
                                                         unname(background_pals[c(1:9,18:35)]), colortext_pals[c(1:9,18:35)], names(background_pals[c(1:9,18:35)])
                                                       )
                                                     )
                                                   )
                                                   # selectInput("palette.brewer.sheet.5",
                                                   #                           label="Palette de couleur",
                                                   #                           choices=c("BuGn : blanc>VERT"="BuGn","BuPu : blanc>VIOLET"="BuPu","GnBu : vert>BLEU"="GnBu","Greens : Verts"="Greens","Greys : Gris"="Greys","Oranges"="Oranges","OrRd : orange>ROUGE"="OrRd","PuBu : violet>BLEU"="PuBu","PuBuGn : violet>Bleu>VERT"="PuBuGn","PuRd : violet>ROUGE"="PuRd","Purples : Violets"="Purples","RdPu : rouge>VIOLET"="RdPu","Reds : Rouges"="Reds","YlGn : jaune>VERT"="YlGn","YlGnBu : jaune>Vert>BLEU"="YlGnBu","YlOrBr : jaune>Orange>BRUN"="YlOrBr","YlOrRd : jaune>Orange>ROUGE"="YlOrRd","BrBG : BRUN>blanc>VERT"="BrBG","PiYG : ROSE>jaune>VERT"="PiYG","PRGn : VIOLET>blanc>VERT"="PRGn","PuOr : ORANGE>blanc>VIOLET"="PuOr","RdBu : ROUGE>blanc>BLEU"="RdBu","RdGy : ROUGE>blanc>GRIS"="RdGy","RdYlBu : ROUGE>jaune>BLEU"="RdYlBu","RdYlGn : ROUGE>jaune>VERT"="RdYlGn","Spectral : ROUGE>jaune>vert>BLEU"="Spectral")[26:18],
                                                   #                           selected="RdYlGn")
                                                   )),
                                                 column(6,hidden(radioButtons("palette.direction.sheet.5",
                                                                              "Direction de la palette",
                                                                              choices=c("Origine"=1, "Inverse"=-1))))
                                               ),
                                               fluidRow(
                                                 column(4,colourInput("couleur1.map.sheet.5","Couleur négative",value = "#CD3333")),
                                                 column(4,colourInput("couleur2.map.sheet.5","Couleur nulle",value = "#FFF8DC")),
                                                 column(4,colourInput("couleur3.map.sheet.5","Couleur positive",value = "#00868B"))
                                               ),
                                               fluidRow(
                                                 column(6,plotOutput("demo.palette.sheet.5",width = 300,height = 50)),
                                                 column(6,actionButton("valider.palette.sheet.5","Valider la palette"))
                                               ),
                                               hr(),
                                               h3("Données manquantes"),
                                               fluidRow(column(4,colourInput("col.na.sheet.5","Couleur des NA",value = "#B8ADAD"))),
                                               hr(),
                                               h3("Contours"),
                                               fluidRow(
                                                 column(4,colourInput("col.contour.sheet.5",
                                                                      "Couleur",value = "#B8ADAD")),
                                                 column(4,sliderInput("lwd.contour.sheet.5",
                                                                      label="Epaisseur",
                                                                      min=0,max=1,value=0.2,step = 0.05))
                                               )

                                      ),


                                      ############ Titre #############
                                      tabPanel("Titre",
                                               h3("Titre"),
                                                 textInput(inputId="titre.sheet.5",
                                                           label="Titre",
                                                           value=NULL),
                                               fluidRow(
                                                 column(3,sliderInput("taille.titre.sheet.5",
                                                                      label="Taille",
                                                                      value=14,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.titre.sheet.5",
                                                                      label="Alignement",
                                                                      value=0,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.titre.sheet.5",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),

                                                 column(4,radioButtons("style.titre.sheet.5",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic")))
                                               ),
                                               hr(),
                                               h3("Sous-titre"),
                                                 textInput(inputId="sous.titre.sheet.5",
                                                           label="Sous-titre",
                                                           value=NULL),
                                               fluidRow(
                                                 column(3,sliderInput("taille.sous.titre.sheet.5",
                                                                      label="Taille",
                                                                      value=11,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.sous.titre.sheet.5",
                                                                      label="Alignement",
                                                                      value=0,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.sous.titre.sheet.5",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),

                                                 column(4,radioButtons("style.sous.titre.sheet.5",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic")))
                                               ),
                                               hr(),
                                               h3("Source"),
                                                 textInput(inputId="text1.source.sheet.5",
                                                           label="Modifier la source (1ère ligne)",
                                                           value="Source : Terres Inovia et Terres Univia d'après les données d'Agreste"),
                                                 textInput(inputId="text2.source.sheet.5",
                                                           label="Modifier la source (2nde ligne)",
                                                           value="(Ministère de l'Agriculture et de l'Alimentation)"),
                                               fluidRow(
                                                 column(3,sliderInput("taille.source.sheet.5",
                                                                      label="Taille",
                                                                      value=8,min=0,max=32,step=1)),
                                                 column(3,sliderInput("alignement.source.sheet.5",
                                                                      label="Alignement",
                                                                      value=1,min=0,max=1,step=0.1)),
                                                 column(2,colourInput("couleur.source.sheet.5",
                                                                      "Couleur",
                                                                      "#665E5E",returnName = TRUE)),
                                                 column(4,radioButtons("style.source.sheet.5",
                                                                       "Style",
                                                                       choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"),
                                                                       selected = "italic"))
                                               )
                                      ),


                                      tabPanel("Légende",
                                               h3("Général"),
                                               textInput(inputId="leg.titre.sheet.5",
                                                         label="Titre de la légende",
                                                         value=NULL),
                                               radioButtons("leg.position.sheet.5",
                                                            label="Position",
                                                            choices=c("Droite"="right","Gauche"="left","Haut"="top","bas"="bottom","Aucune"="none"),inline=TRUE),
                                               hr(),
                                               h3("Titre de la légende"),

                                               fluidRow(
                                                 column(4,sliderInput("leg.titre.size.sheet.5",label="Taille",value=10,min=0,max=32,step=1)),
                                                 column(4,radioButtons("leg.titre.style.sheet.5","Style", choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"))),
                                                 column(4,colourInput("leg.titre.col.sheet.5", "Couleur", "#665E5E",returnName = FALSE))
                                               ),
                                               hr(),
                                               h3("Chiffres de la légende"),
                                               fluidRow(
                                                 column(4,sliderInput("leg.chiffre.size.sheet.5",
                                                                      label="Taille",
                                                                      value=8,min=0,max=32,step=1)),
                                                 column(4,radioButtons("leg.chiffre.style.sheet.5","Style", choices=c("Normal"="plain", "Italique"="italic", "Gras"="bold", "Gras Italique"="bold.italic"))),
                                                 column(4,colourInput("leg.chiffre.col.sheet.5", "Couleur", "#665E5E",returnName = FALSE)))
                                      ),
                                      ########### Logos TI et météo France ###########


                                      tabPanel("Logos",
                                               h3("Logo TI"),
                                               checkboxInput("logo.ti.sheet.5","Afficher le logo Terres Inovia",value=TRUE),
                                               fluidRow(
                                                 column(4,hidden(sliderInput("size.logo.ti.sheet.5",
                                                                             label="Taille",
                                                                             value=0.15,min=0,max=1,step=0.01))),
                                                 column(4,hidden(numericInput("posx.logo.ti.sheet.5",
                                                                             label="Position X",
                                                                             value=900310,min=90000,max=1300000,step=1))),
                                                 column(4,hidden(numericInput("posy.logo.ti.sheet.5",
                                                                             label="Position Y",
                                                                             value=7200660,min=6000000,max=7500000,step=1)))),
                                               actionButton("LogoTIclick5","Positionner le logo à l'endroit du dernier click"),
                                               verbatimTextOutput("info"),
                                               hr(),
                                               h3("Logo TU"),
                                               checkboxInput("logo.tu.sheet.5","Afficher le logo Terres Univia",value=TRUE),
                                               fluidRow(
                                                 column(4,hidden(sliderInput("size.logo.tu.sheet.5",
                                                                             label="Taille",
                                                                             value=0.15,min=0,max=1,step=0.01))),
                                                 column(4,hidden(sliderInput("posx.logo.tu.sheet.5",
                                                                             label="Position X",
                                                                             value=1080310,min=90000,max=1300000,step=1))),
                                                 column(4,hidden(sliderInput("posy.logo.tu.sheet.5",
                                                                             label="Position Y",
                                                                             value=7200660,min=6000000,max=7500000,step=1))))
                                      ),

                                      tabPanel("Autres",
                                               h3("Flèche Nord (en cours de développement)")
                                               # checkboxInput("north.arrow.sheet.5","Afficher la flèche nord",value = FALSE),
                                               # fluidRow(
                                               #   column(2,numericInput("type.north.arrow.sheet.5",
                                               #                         label="Type de flèche nord",
                                               #                         min=1,max=18,value=3)),
                                               #   column(3,sliderInput("taille.north.arrow.sheet.5","Taille",min=0,max=1,value=0.1)),
                                               #   column(3,radioButtons("location.north.arrow.sheet.5",
                                               #                         "Position",
                                               #                         choices=c("Bas-gauche"="bottomleft","Bas-droite"="bottomright","Haut-droite"="topright","Haut-gauche"="topleft")))
                                               # ),
                                               # plotOutput("north.arrow.symbols5",width = 400,height = 290)
                                      )


                               ) # Tabbox
                           ) # box()
                         ) # fluidRow
                  ) # column
                ) # fluidRow
        ) # TabItem 5 (5)
      ) # TabItems (4)
    ) # dashboardBody (3)
  ) # dashboardPage (2)
) # Shiny UI (1)