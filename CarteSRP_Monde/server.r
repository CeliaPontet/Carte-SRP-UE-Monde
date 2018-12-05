# server.R


shinyServer(function(input, output,session) {

  #-----------------------------------------------------------------------------
  # --- Sidebar
  #-----------------------------------------------------------------------------
  
  data <- reactive({
    data_ue %>% 
      filter(CULTURE == input$culture) %>% 
      filter(CRITERE == input$critere) %>% 
      filter(UE == input$niveau)
  })
  
  
  #-----------------------------------------------------------------------------
  # --- Menu principal
  #-----------------------------------------------------------------------------
  
  # Lien vers tab Donnees
  observeEvent(input$link_to_tabpanel_Donnees, {
    updateNavbarPage(session, "menu", "Donnees")
  })
  # Lien vers tab Desciption
  observeEvent(input$link_to_tabpanel_Description, {
    updateNavbarPage(session, "menu", "Description")
  })
  # Lien vers tab Carte
  observeEvent(input$link_to_tabpanel_Carte, {
    updateNavbarPage(session, "menu", "Carte")
  })
  # Lien vers tab Carte Ecart
  observeEvent(input$link_to_tabpanel_Carte_Evol, {
    updateNavbarPage(session, "menu", "Carte_evol")
  })
  
  
  #-----------------------------------------------------------------------------
  # --- Données
  #-----------------------------------------------------------------------------
  
  output$df.sheet.2<-DT::renderDataTable(data_ue %>% filter(UE=="UE à 28") %>% select(-UE), filter = 'top', server = TRUE, rownames = FALSE,
                                         options = list(autoWidth = TRUE), selection = 'none') # v3.4.1 argument ne fonctionne pas, editable = FALSE
  
  output$downloadData.sheet.2 <- downloadHandler(
    filename = function() {paste("SRP_UE_Monde-", Sys.Date(), ".csv", sep="")},
    content = function(file) {write.csv2(data_ue %>% filter(UE=="UE à 28") %>% select(-UE), file, row.names = FALSE)}
  )
  
  
  
  #-----------------------------------------------------------------------------
  # --- Graphique évolution
  #-----------------------------------------------------------------------------
  
  #Mise à jour des pays selon le niveau choisi
  observeEvent(input$niveau,{
    shiny::updateSelectInput(session = session,
                             inputId = "pays.sheet.3",
                             choices= data() %>% distinct(PAYS) %>% pull(),
                             selected = data() %>% group_by(PAYS) %>% summarise(sum(VALUE,na.rm=TRUE)) %>% top_n(5) %>% distinct(PAYS) %>% pull())})
  
  #Vecteur de couleur perso en fonction du nombre d'entités
  observeEvent(input$pays.sheet.3,{
    updateTextInput(session = session,
                    inputId = "vecteur.couleurs.sheet.3",
                    value = paste(couleurs[1:length(input$pays.sheet.3)],collapse='-'))
  })
  
  
  #Création de data.sheet.3 à partir des
  data.sheet.3 <- reactive({
    data.sheet.3 <- data() %>%
      filter(ANNEE>=input$annees.sheet.3[1] & ANNEE<=input$annees.sheet.3[2]) %>%
      filter(PAYS %in% input$pays.sheet.3) %>%
      mutate(TOOLTIP=paste(PAYS,"\n",ANNEE,"\n",VALUE,UNIT,sep=""))
  })
  
  #Mise à jour du titre
  observeEvent(data(),{
    updateTextInput(session = session,
                    inputId = "titre.sheet.3",
                    value = paste("Evolution des ",input$critere,"s - ",input$niveau, sep=""))
  })
  
  observeEvent({
    input$annees.sheet.3
    input$culture},{
      updateTextInput(session = session,
                      inputId = "sous.titre.sheet.3",
                      value = paste(input$culture," - ",input$annees.sheet.3[1]," à ",input$annees.sheet.3[2],sep=""))
    })
  
  observeEvent(data(),{
    updateNumericInput(session=session,
                       inputId = "ymin.sheet.3",
                       value = data() %>% summarise(min(VALUE,na.rm=TRUE)) %>% pull())
  })
  observeEvent(data(),{
    updateSliderInput(session=session,
                      inputId = "ymax.sheet.3",
                      value = data() %>% summarise(max(VALUE,na.rm=TRUE)) %>% pull())
  })
  

  lim3 <- reactive({
    p <- data.sheet.3() %>%
      ggplot() +
      aes(x=ANNEE,y=VALUE)
    lim_y <- ggplot_build(p)$layout$panel_params[[1]]$y.range
    lim_x <- ggplot_build(p)$layout$panel_params[[1]]$x.range
    lim3 <- c(lim_x,lim_y)
  })
  
  
  observeEvent(input$y.ajust.sheet.3 == "perso",{
    shinyjs::toggle("ymin.sheet.3")
    shinyjs::toggle("ymax.sheet.3")
  })
  
  #Apparition des paramètres si texte souhaité
  observe({
    shinyjs::toggle("taille.text.sheet.3", input$ajout.text.sheet.3 == TRUE)
    shinyjs::toggle("nb.dec.sheet.3", input$ajout.text.sheet.3 == TRUE)
    shinyjs::toggle("col.text.sheet.3", input$ajout.text.sheet.3 == TRUE)
    shinyjs::toggle("col.group.sheet.3", input$ajout.text.sheet.3 == TRUE)
  })
  
  
  observeEvent(input$mode.couleur.sheet.3 == "Palette",{
    shinyjs::toggle("palette.brewer.sheet.3")
    shinyjs::toggle("palette.direction.sheet.3")
    shinyjs::toggle("CouleursInputs")
    shinyjs::toggle("sample.sheet.3")
  })
  observeEvent(input$mode.couleur.sheet.3 == "Perso",{
    shinyjs::toggle("vecteur.couleurs.sheet.3")
    shinyjs::toggle("couleur.sheet.3")
  })
  
  #Options logo si demandés
  observe({
    shinyjs::toggle("size.logo.ti.sheet.3", input$logo.ti.sheet.3 == TRUE)
    shinyjs::toggle("posx.logo.ti.sheet.3", input$logo.ti.sheet.3 == TRUE)
    shinyjs::toggle("posy.logo.ti.sheet.3", input$logo.ti.sheet.3 == TRUE)
  })
  observe({
    shinyjs::toggle("size.logo.tu.sheet.3", input$logo.tu.sheet.3 == TRUE)
    shinyjs::toggle("posx.logo.tu.sheet.3", input$logo.tu.sheet.3 == TRUE)
    shinyjs::toggle("posy.logo.tu.sheet.3", input$logo.tu.sheet.3 == TRUE)
  })
  
  
  
  observe({
    shinyjs::toggle("method.sheet.3", input$trends.sheet.3 == TRUE)
    shinyjs::toggle("lwd.trend.sheet.3", input$logo.tu.sheet.3 == TRUE)
    shinyjs::toggle("se.sheet.3", input$logo.tu.sheet.3 == TRUE)
    shinyjs::toggle("level.sheet.3", input$logo.tu.sheet.3 == TRUE)
  })
  
  
  
  col <- reactive({
    input$sample.sheet.3
    
    if (input$mode.couleur.sheet.3 == "Palette"){
      if (length(input$pays.sheet.3)<=brewer.pal.info[input$palette.brewer.sheet.3,]$maxcolors){
        if (input$palette.direction.sheet.3==1){
          col <- colorRampPalette(brewer.pal(brewer.pal.info[input$palette.brewer.sheet.3,]$maxcolors, input$palette.brewer.sheet.3))(brewer.pal.info[input$palette.brewer.sheet.3,]$maxcolors)[1:length(input$pays.sheet.3)]
        } else {
          col <- colorRampPalette(brewer.pal(brewer.pal.info[input$palette.brewer.sheet.3,]$maxcolors, input$palette.brewer.sheet.3))(brewer.pal.info[input$palette.brewer.sheet.3,]$maxcolors)[sample(1:brewer.pal.info[input$palette.brewer.sheet.3,]$maxcolors,length(input$pays.sheet.3))]
        }
      } else {
        if (input$palette.direction.sheet.3==1){
          col <- colorRampPalette(brewer.pal(brewer.pal.info[input$palette.brewer.sheet.3,]$maxcolors, input$palette.brewer.sheet.3))(length(input$pays.sheet.3))
        } else {
          col <- colorRampPalette(brewer.pal(brewer.pal.info[input$palette.brewer.sheet.3,]$maxcolors, input$palette.brewer.sheet.3))(length(input$pays.sheet.3))[sample(1:length(input$pays.sheet.3),length(input$pays.sheet.3))]
        }
      }
      
    } else {
      col <- colorRampPalette(unlist(strsplit(input$vecteur.couleurs.sheet.3, split="-")))(length(input$pays.sheet.3))
    }
  })
  
  output$demo.palette.sheet.3 <- renderPlot({
    par(mar=c(0,0,0,0))
    plot(x=0.5:(length(input$pays.sheet.3)+0.5),y=rep(0.5,length(input$pays.sheet.3)+1), type= "n", xlab = "", ylab = "",xaxt='n',yaxt='n',frame.plot=FALSE)
    rect(xleft=0.5:(length(input$pays.sheet.3)-0.5),
         ybottom=rep(0,length(input$pays.sheet.3)),
         xright=1.5:(length(input$pays.sheet.3)+1),
         ytop=rep(1,length(input$pays.sheet.3)),
         col=col(),lwd=0,ylim=c(0,1),border = NA)
  })
  
  
  #Mise à jour du titre de la légende
  observeEvent(data.sheet.3(),{
    updateSliderInput(session = session,
                      inputId = "posx.logo.ti.sheet.3",
                      value = (lim3()[2]+0.12*(lim3()[2]-lim3()[1])-(lim3()[2]-lim3()[1])*input$size.logo.ti.sheet.3),
                      min = lim3()[1]-0.3*(lim3()[2]-lim3()[1]),
                      max = lim3()[2]+0.3*(lim3()[2]-lim3()[1]))
  })
  # (data_srp %>% summarise(min(ANNEE)) %>% pull())-0.2*(data_srp %>% summarise(max(ANNEE))-data_srp %>% summarise(min(ANNEE))) %>% pull()
  observeEvent(data.sheet.3(),{
    updateSliderInput(session = session,
                      inputId = "posx.logo.tu.sheet.3",
                      value = lim3()[2]+0.12*(lim3()[2]-lim3()[1]),
                      min = lim3()[1]-0.3*(lim3()[2]-lim3()[1]),
                      max = lim3()[2]+0.3*(lim3()[2]-lim3()[1]))
  })
  observeEvent(data.sheet.3(),{
    updateSliderInput(session = session,
                      inputId = "posy.logo.ti.sheet.3",
                      value = lim3()[4]+0.12*(lim3()[4]-lim3()[3]),
                      min = lim3()[3]-0.3*(lim3()[4]-lim3()[3]),
                      max = lim3()[4]+0.3*(lim3()[4]-lim3()[3]))
  })
  observeEvent(data.sheet.3(),{
    updateSliderInput(session = session,
                      inputId = "posy.logo.tu.sheet.3",
                      value = lim3()[4]+0.12*(lim3()[4]-lim3()[3]),
                      min = lim3()[3]-0.3*(lim3()[4]-lim3()[3]),
                      max = lim3()[4]+0.3*(lim3()[4]-lim3()[3]))
  })
  
  observeEvent(input$critere,{
    updateTextInput(session = session,
                    inputId = "ylab.sheet.3",
                    value = glue("{input$critere} ({data() %>% distinct(UNIT) %>% pull()})"))
  })
  
  
  
  PlotOutputSheet3 <- function(){
    # input$valider.palette.sheet.3
    
    p <- data.sheet.3() %>%
      ggplot() +
      aes(x=ANNEE,y=VALUE,group=PAYS,colour=PAYS,tooltip=VALUE)+
      xlab("Années")+
      ylab(input$critere)+
      scale_x_continuous(breaks=seq(input$min.grid.major.sheet.3,input$max.grid.major.sheet.3,input$pas.grid.major.sheet.3),
                         minor_breaks = seq(input$min.grid.minor.sheet.3,input$max.grid.minor.sheet.3,input$pas.grid.minor.sheet.3))+
      geom_point(size=input$size.point.sheet.3)+
      labs(color=input$leg.titre.sheet.3,
           title = input$titre.sheet.3,
           subtitle = input$sous.titre.sheet.3,
           caption=glue("{input$text1.source.sheet.3} \n {input$text2.source.sheet.3}"))+
      ylab(input$ylab.sheet.3)+
      xlab(input$xlab.sheet.3)+
      theme(plot.title = element_text(size=input$taille.titre.sheet.3,
                                      hjust=input$alignement.titre.sheet.3,
                                      face=input$style.titre.sheet.3,
                                      color=input$couleur.titre.sheet.3))+
      theme(plot.subtitle=element_text(size=input$taille.sous.titre.sheet.3,
                                       hjust=input$alignement.sous.titre.sheet.3,
                                       face=input$style.sous.titre.sheet.3,
                                       color=input$couleur.sous.titre.sheet.3))+
      theme(plot.caption = element_text(size=input$taille.source.sheet.3,
                                        hjust=input$alignement.source.sheet.3,
                                        face=input$style.source.sheet.3,
                                        color=input$couleur.source.sheet.3))+
      theme(legend.title = element_text(colour=input$leg.titre.col.sheet.3,
                                        face=input$leg.titre.style.sheet.3,
                                        size=input$leg.titre.size.sheet.3))+
      theme(legend.text=element_text(colour=input$leg.elements.col.sheet.3,
                                     face=input$leg.elements.style.sheet.3,
                                     size=input$leg.elements.size.sheet.3))+
      theme(legend.position=input$leg.position.sheet.3)+#  Pivot des labels des années
      theme(axis.text.x=element_text(size=input$taille.labelx.sheet.3,angle=input$orientation.labelx.sheet.3,hjust=input$alignement.labelx.sheet.3, colour=input$col.axex.sheet.3))+
      theme(axis.text.y=element_text(size=input$taille.labely.sheet.3,colour=input$col.axey.sheet.3))+
      theme(axis.title.x =  element_text(size=input$taille.titrex.sheet.3 ,colour = input$col.titrex.sheet.3))+
      theme(axis.title.y =  element_text(size=input$taille.titrey.sheet.3 ,colour = input$col.titrey.sheet.3))+
      theme(panel.background = element_rect(fill = input$couleur.fond.sheet.3),
            panel.grid.major = element_line(colour = input$couleur.grid.major.sheet.3, size=input$taille.grid.major.sheet.3,linetype = input$linetype.grid.major.sheet.3),
            panel.grid.minor = element_line(colour = input$couleur.grid.minor.sheet.3,
                                            size=input$taille.grid.minor.sheet.3,
                                            linetype = input$linetype.grid.minor.sheet.3),
            panel.border = element_blank(),
            axis.line.x = element_line(colour = input$couleur.axex.sheet.3,
                                       size=input$taille.axex.sheet.3,
                                       lineend = "butt"),
            axis.line.y = element_line(colour = input$couleur.axey.sheet.3,
                                       size=input$taille.axey.sheet.3))



    #Ajustement de l'axe y à la demande
    if (input$y.ajust.sheet.3=="perso"){
      p <- p +
        ylim(low = input$ymin.sheet.3, high = input$ymax.sheet.3)
    }

    #Ajout du texte si souhaité
    if (input$ajout.text.sheet.3){
      if (input$col.group.sheet.3){
        p <- p +
          geom_text(aes(label=round(VALUE,digits=input$nb.dec.sheet.3)), size=input$taille.text.sheet.3)
      } else {
        p <- p +
          geom_text(aes(label=round(VALUE,digits=input$nb.dec.sheet.3)), size=input$taille.text.sheet.3, col=input$col.text.sheet.3)

      }
    }

    p <- p +
      scale_colour_manual(values = col())


    #On récupère les limites pour les logos
    xmax.ti <- input$posx.logo.ti.sheet.3
    ymax.ti <- input$posy.logo.ti.sheet.3
    xmin.ti <- xmax.ti - (lim3()[2]-lim3()[1])*input$size.logo.ti.sheet.3
    ymin.ti <- ymax.ti - (lim3()[4]-lim3()[3])*input$size.logo.ti.sheet.3

    xmax.tu <- input$posx.logo.tu.sheet.3
    ymax.tu <- input$posy.logo.tu.sheet.3
    xmin.tu <- xmax.tu - (lim3()[2]-lim3()[1])*input$size.logo.tu.sheet.3
    ymin.tu <- ymax.tu - (lim3()[4]-lim3()[3])*input$size.logo.tu.sheet.3

    if (input$logo.ti.sheet.3){
      p <- p +
        annotation_custom(rasterGrob(ti), xmin.ti, xmax.ti, ymin.ti, ymax.ti)
    }
    if (input$logo.tu.sheet.3){
      p <- p +
        annotation_custom(rasterGrob(tu), xmin.tu, xmax.tu, ymin.tu, ymax.tu)
    }

    #Séparation des entités
    if(input$facet_wrap.sheet.3==TRUE){p<-p+facet_wrap(~PAYS)}

    #Type de ligne
    if(input$linetype.sheet.3!="blank"){p<-p+geom_line(linetype=input$linetype.sheet.3)}

    if (input$trends.sheet.3){
      if (input$se.sheet.3==FALSE){
        p<-p+geom_smooth(method=input$method.sheet.3,se=input$se.sheet.3,lwd=input$lwd.trend.sheet.3)
      } else if (input$se.sheet.3==TRUE){
        p<-p+geom_smooth(method=input$method.sheet.3,se=input$se.sheet.3,level=input$level.sheet.3,lwd=input$lwd.trend.sheet.3)
      }
    }

    p <- ggplot_gtable(ggplot_build(p))
    p$layout$clip[p$layout$name=="panel"] <- "off"
    grid.draw(p)
  }
  
  #Graphique
  output$graph.sheet.3 <-  renderPlot({
    # validate(need(data.sheet.3() %>% summarise(!all(is.na(VALUE))) %>% pull(),"Pas de données à représenter."))
    # validate(need(!inherits(try(colorRampPalette(unlist(strsplit(c(input$vecteur.couleurs.sheet.3), split="-")))(length(input$pays.sheet.3)),silent = TRUE),"try-error"),"Les couleurs personnalisées ne sont pas correctement renseignées. RAPPEL : Donner autant de couleurs que d'entitée, séparées par un '-'"))
    # 
    # ggplotly(PlotOutputSheet3(),width=96*input$width.sheet.3/2.54,height=96*input$height.sheet.3/2.54)
    # ggplotly(PlotOutputSheet3(),width=input$width.sheet.3,height=input$height.sheet.3,unit="cm")
    print(PlotOutputSheet3())
    # ggiraph(code=print(p))
  })
  
  #Affichage des données
  output$df.sheet.3 <- DT::renderDataTable(data.sheet.3(), filter = 'top', server = TRUE, rownames = FALSE,
                                           options = list(autoWidth = TRUE), selection = 'none') # v3.4.1 argument ne fonctionne pas, editable = FALSE
  
  #Téléchargement des données
  output$downloadData.sheet.3 <- downloadHandler(
    filename = function() {
      paste(input$critere,"-",input$culture,"-",input$niveau,"-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {write.csv2(data.sheet.3(), file, row.names = FALSE)}
  )
  
  #Téléchargement du graphique
  output$down.sheet.3 = downloadHandler(
    filename = function() {paste("Plot-",input$culture,"-",input$critere,"-",input$annees.sheet.3[1],"à",input$annees.sheet.3[2],"-",Sys.Date(),".",input$save.type.sheet.3,sep="")},
    content = function(file) {
      ggsave(file,plot = PlotOutputSheet3(),dpi=600,width=input$width.sheet.3,height=input$height.sheet.3,unit="cm")
    }
  )
  
  
  
  
  
  #-----------------------------------------------------------------------------
  # --- SHEET 4 : Carto 1 année
  #-----------------------------------------------------------------------------

  reac4 <- reactiveValues(redraw = TRUE, ent4 = isolate(input$choix.entite.sheet.4))

  # If any inputs are changed, set the redraw parameter to FALSE
  observe({
    input$choix.entite.sheet.4
    reac4$redraw <- FALSE
  })

  observe({
    invalidateLater(100, session)
    # input$valider.entite.sheet.4
    if (isolate(reac4$redraw)) {
      reac4$ent4 <- input$choix.entite.sheet.4
    } else {
      isolate(reac4$redraw <- TRUE)
    }
  })

  
  
  #Création de data.sheet.4 à partir des
  data.sheet.4 <- reactive({
    data() %>%
      filter(ANNEE %in% input$annee.sheet.4,PAYS %in% reac4$ent4)
  })

  # value_lim <- reactive({
  #   req(data.sheet.4())
  #   if (is.finite(data.sheet.4() %>% summarise(min(VALUE,na.rm=TRUE)) %>% pull()) & is.finite(data.sheet.4() %>% summarise(max(VALUE,na.rm=TRUE)) %>% pull())){
  #     c(data.sheet.4() %>% summarise(min(VALUE,na.rm=TRUE)) %>% pull(),data.sheet.4() %>% summarise(max(VALUE,na.rm=TRUE)) %>% pull())
  #   } else {
  #     value_lim <- c(0,input$nb.dec.sheet.4+1)
  #   }
  # })
  # 
  # # Mise à jour des interavlles
  # observeEvent({
  #   input$decoupage.auto.sheet.4
  #   data.sheet.4()
  #   input$nb.interv.sheet.4},{
  #     updateTextInput(session = session,
  #                     inputId = "interv.sheet.4",
  #                     value = paste(seq(from=value_lim()[1],to=value_lim()[2],length.out=input$nb.interv.sheet.4+1),collapse = "/"))
  #   })
  # 
  # JoinLevel <- reactive({
  #   if (input$niveau=="Département"){
  #     JoinLevel <- "NOM_DEPT"
  #   } else if (input$niveau=="Région (anciennes)"){
  #     JoinLevel <- "NOM_ANC_REG"
  #   } else if (input$niveau=="Région"){
  #     JoinLevel <- "NOM_REG"
  #   } else if (input$niveau=="ZONE IRD"){
  #     JoinLevel <- "ZONE_IRD"
  #   } else {
  #     JoinLevel <- "ZONE_TI"
  #   }
  # })
  # 
  # 
  # fond4 <- reactive({
  #   if (input$corse.sheet.4){
  #     if (input$niveau=="Département"){
  #       fond4 <- fond_dept
  #     } else if (input$niveau=="Région (anciennes)"){
  #       fond4 <- fond_anc_reg
  #     } else if (input$niveau=="ZONE IRD"){
  #       fond4 <- fond_ird
  #     } else if (input$niveau=="Région"){
  #       fond4 <- fond_region
  #     } else {
  #       fond4 <- fond_ti
  #     }
  #   } else {
  #     if (input$niveau=="Département"){
  #       fond4 <- fond_dept_sanscorse
  #     } else if (input$niveau=="Région (anciennes)"){
  #       fond4<-fond_anc_reg_sanscorse
  #     } else if (input$niveau=="ZONE IRD"){
  #       fond4 <- fond_ird_sanscorse
  #     } else if (input$niveau=="Région"){
  #       fond4 <- fond_region_sanscorse
  #     } else {
  #       fond4 <- fond_ti_sanscorse
  #     }
  #   }
  # })
  # 
  # LIMITS <- reactive({
  #   if (sum(data.sheet.4()$VALUE,na.rm=TRUE)>0){
  #     as.numeric(unlist(strsplit(input$interv.sheet.4, split="/")))
  #   }
  # })
  # 
  # 
  # eff.sheet.4 <- reactive({
  #   if(input$type.echelle.sheet.4=="Discrète"){
  #     LIMITS <- LIMITS()
  #     LIM <- NULL
  #     while (length(LIMITS)>1){
  #       LIM <- c(LIM,glue("{round(LIMITS[1],2)} à {round(LIMITS[2],2)} {data() %>% distinct(UNIT) %>% pull()}"))
  #       LIMITS <- LIMITS[-1]
  #     }
  #     data.sheet.4() %>%
  #       mutate(VALUE_CUTED = cut(VALUE,breaks=LIMITS(),include.lowest = TRUE,labels = LIM)) %>%
  #       rename(Intervalles = "VALUE_CUTED") %>%
  #       count(Intervalles)
  #   }
  # })
  # 
  # 
  # #Table des effectifs
  # output$effectifs.sheet.4 <- renderTable({
  #   eff.sheet.4()
  # })
  # 
  # #Mise à jour des choix de la sélection
  # observeEvent(input$niveau,{
  #   shiny::updateSelectInput(session = session,
  #                            inputId = "choix.entite.sheet.4",
  #                            choices = data() %>% distinct(ENTITE) %>% pull(),
  #                            selected = data() %>% distinct(ENTITE) %>% pull())
  # })
  # 
  # #Mise à jour des bornes selon les choix des critères généraux
  # observeEvent(input$culture,{
  #   updateSliderInput(session = session,
  #                     inputId = "annee.sheet.4",
  #                     min = data() %>%  summarise(min(ANNEE)) %>% pull(),
  #                     max = data() %>%  summarise(max(ANNEE)) %>% pull(),
  #                     value = data() %>%  summarise(max(ANNEE)) %>% pull())
  # })
  # 
  # #Mise à jour du titre et sous-titre proposés
  # observeEvent(data(),{
  #   updateTextInput(session = session,
  #                   inputId = "titre.sheet.4",
  #                   value = glue("Carte des {input$critere}s"))
  # })
  # observeEvent(data.sheet.4(),{
  #   updateTextInput(session = session,
  #                   inputId = "sous.titre.sheet.4",
  #                   value = glue("{paste(culture(),collapse = '+')} - {input$annee.sheet.4}"))
  # })
  # 
  # #Mise à jour du titre de la légende
  # observeEvent(data(),{
  #   updateTextInput(session = session,
  #                   inputId = "leg.titre.sheet.4",
  #                   value = glue("{input$critere} ({data.sheet.4() %>% distinct(UNIT) %>% pull()})"))
  # })
  # 
  # #Mise à jour des min et max si échelle perso
  # observeEvent(data(),{
  #   updateNumericInput(session = session,
  #                      inputId = "leg.gamme.min.sheet.4",
  #                      value = data.sheet.4() %>% summarise(min(VALUE,na.rm=TRUE)) %>% pull())
  # })
  # observeEvent(data(),{
  #   updateNumericInput(session = session,
  #                      inputId = "leg.gamme.max.sheet.4",
  #                      value = data.sheet.4() %>% summarise(max(VALUE,na.rm=TRUE)) %>% pull())
  # })
  # 
  # observeEvent(input$type.echelle.sheet.4 == "Continue",{
  #   shinyjs::toggle("nb.interv.sheet.4")
  #   shinyjs::toggle("decoupage.auto.sheet.4")
  #   shinyjs::toggle("interv.sheet.4")
  #   shinyjs::toggle("leg.ajust.sheet.4")
  # })
  # 
  # 
  # 
  # #Gestion gamme echelle
  # observeEvent(input$leg.ajust.sheet.4=="Echelle personnalisée",{
  #   shinyjs::toggle("leg.gamme.min.sheet.4")
  #   shinyjs::toggle("leg.gamme.max.sheet.4")
  #   shinyjs::toggle("maj.gamme.sheet.4")
  # })
  # 
  # # Gestion couleurs selon gradient / palette
  # observe({
  #   shinyjs::toggle("couleur1.map.sheet.4", input$mode.couleur.sheet.4 == "Gradient")
  #   shinyjs::toggle("couleur2.map.sheet.4", input$mode.couleur.sheet.4 == "Gradient")
  #   shinyjs::toggle("palette.brewer.sheet.4", input$mode.couleur.sheet.4 == "Palette")
  #   shinyjs::toggle("palette.direction.sheet.4", input$mode.couleur.sheet.4 == "Palette")
  # })
  # 
  # #Type de flêche
  # observe({
  #   shinyjs::toggle("type.north.arrow.sheet.4", input$north.arrow.sheet.4 == TRUE)
  #   shinyjs::toggle("location.north.arrow.sheet.4", input$north.arrow.sheet.4 == TRUE)
  #   shinyjs::toggle("taille.north.arrow.sheet.4", input$north.arrow.sheet.4 == TRUE)
  # })
  # 
  # #Décimales et couleur si texte apposé au graph
  # # observe({
  # #   shinyjs::toggle("nb.dec.sheet.4", input$ajout.text.sheet.4==FALSE )
  # #   shinyjs::toggle("col.text.sheet.4", input$ajout.text.sheet.4==FALSE)
  # # })
  # # observeEvent(input$ajout.text.sheet.4, {
  # #   shinyjs::toggle("nb.dec.sheet.4", input$ajout.text.sheet.4==FALSE )
  # #   shinyjs::toggle("col.text.sheet.4", input$ajout.text.sheet.4==FALSE)
  # # })
  # observe({
  #   shinyjs::toggle("taille.text.sheet.4", input$ajout.text.sheet.4 == TRUE)
  #   shinyjs::toggle("nb.dec.sheet.4", input$ajout.text.sheet.4 == TRUE)
  #   shinyjs::toggle("col.text.sheet.4", input$ajout.text.sheet.4 == TRUE)
  # })
  # 
  # 
  # #Options logo si demandés
  # observe({
  #   shinyjs::toggle("size.logo.ti.sheet.4", input$logo.ti.sheet.4 == TRUE)
  #   shinyjs::toggle("posx.logo.ti.sheet.4", input$logo.ti.sheet.4 == TRUE)
  #   shinyjs::toggle("posy.logo.ti.sheet.4", input$logo.ti.sheet.4 == TRUE)
  # })
  # observe({
  #   shinyjs::toggle("size.logo.tu.sheet.4", input$logo.tu.sheet.4 == TRUE)
  #   shinyjs::toggle("posx.logo.tu.sheet.4", input$logo.tu.sheet.4 == TRUE)
  #   shinyjs::toggle("posy.logo.tu.sheet.4", input$logo.tu.sheet.4 == TRUE)
  # })
  # 
  # 
  # 
  # output$demo.palette.sheet.4 <- renderPlot({
  #   par(mar=c(0,0,0,0))
  #   if (input$mode.couleur.sheet.4 == "Palette" & input$palette.direction.sheet.4==1){
  #     col <- colorRampPalette(brewer.pal(9, input$palette.brewer.sheet.4))(50)[1:50]
  #   } else if (input$mode.couleur.sheet.4 == "Palette" & input$palette.direction.sheet.4==-1){
  #     col <- colorRampPalette(brewer.pal(9, input$palette.brewer.sheet.4))(50)[50:1]
  #   } else {
  #     col <- colorRampPalette(c(input$couleur1.map.sheet.4,input$couleur2.map.sheet.4))(50)
  #   }
  #   plot(x=1:50,y=rep(0.5,50), type= "n", xlab = "", ylab = "",xaxt='n',yaxt='n',frame.plot=FALSE)
  #   rect(0.5:49.5,rep(0,50),1.5:50.5,rep(1,50),col=col,lwd=0,ylim=c(0,1),border = NA)
  # })
  # 
  # #Gamme des flêches nord
  # output$north.arrow.symbols4 <- renderPlot({
  #   if (input$north.arrow.sheet.4) {
  #     par (mar=c(0,0,0,0))
  #     # northSymbols()
  #   }
  # })
  # 
  # 
  # output$summary4 <- renderTable({
  #   data.sheet.4() %>% summarise(Min = min(VALUE,na.rm=TRUE),
  #                                q25=quantile(VALUE, probs = 0.25,na.rm=TRUE),
  #                                Median = median(VALUE,na.rm=TRUE),
  #                                Mean = mean(VALUE,na.rm=TRUE),
  #                                q75 = quantile(VALUE, probs = 0.75,na.rm=TRUE),
  #                                Max = max(VALUE,na.rm=TRUE))
  # })
  # 
  # 
  
  data_map <- reactive({
    data <- data.sheet.4() %>% 
      full_join(ref,by="PAYS") %>% 
      select(-PAYS) %>% 
      filter(!is.na(CULTURE),!is.na(ANNEE),!is.na(name)) %>% 
      complete(name,ANNEE,CULTURE)
  })
  
  MapOutputSheet4 <- reactive({

        ne_countries(scale=110, returnclass = "sf") %>% 
      st_transform("+proj=merc") %>% 
      full_join(data_map(),by="name") %>%
      ggplot()+
      geom_sf(aes(fill=VALUE))+
      coord_sf(xlim=c(-3000000, 4000000),ylim=c(4000000,11000000))
    
  #   input$valider.palette.sheet.4
  #   input$maj.gamme.sheet.4
  #   
  #   lim4 <- fond4() %>% 
  #     filter_(paste(JoinLevel(),"%in% reac4$ent4")) %>% 
  #     st_bbox()
  #   
  #   #On récupère les limites pour les logos
  #   xmin.ti <- input$posx.logo.ti.sheet.4
  #   xmax.ti <- xmin.ti + (lim4[3]-lim4[1])*input$size.logo.ti.sheet.4
  #   ymin.ti <- input$posy.logo.ti.sheet.4
  #   ymax.ti <- ymin.ti + (xmax.ti-xmin.ti)*(499/1153)
  #   #On récupère les limites pour les logos
  #   xmin.tu <- input$posx.logo.tu.sheet.4
  #   xmax.tu <- xmin.tu + (lim4[3]-lim4[1])*input$size.logo.tu.sheet.4
  #   ymin.tu <- input$posy.logo.tu.sheet.4
  #   ymax.tu <- ymin.tu + (xmax.tu-xmin.tu)*(499/1153)
  #   
  #   
  #   #Si on est en échelle Discrète
  #   #Echelle ajustée : dépend du nb d'intervalles demandés
  #   LIMITS <- LIMITS()
  #   LIM <- NULL
  #   while (length(LIMITS)>1){
  #     LIM <- c(LIM,glue("{round(LIMITS[1],2)} à {round(LIMITS[2],2)} {data() %>% distinct(UNIT) %>% pull()}"))
  #     LIMITS <- LIMITS[-1]
  #   }
  #   
  #   #Partie fixe
  #   g <- data.sheet.4() %>%
  #     mutate(VALUE_CUTED = cut(VALUE,breaks=LIMITS(),include.lowest = TRUE,labels = LIM)) %>%
  #     left_join(fond4(),by=c("ENTITE"=JoinLevel())) %>%
  #     ggplot()
  #   
  #   if (input$type.echelle.sheet.4=="Continue"){
  #     VAR <- "VALUE"
  #   } else {
  #     VAR <- "VALUE_CUTED"
  #   }
  #   
  #   g <- g +
  #     geom_sf(aes_string(fill=VAR), colour = input$col.contour.sheet.4, lwd=input$lwd.contour.sheet.4)
  #   
  #   #Ajustement du scale_fill/scale_color selon :
  #   # type d'échelle : continue ou discret
  #   # type de palette : brewer ou perso
  #   # la gamme : ajustée ou perso
  #   
  #   
  #   #Si on est en échelle Continue
  #   if (input$type.echelle.sheet.4=="Continue"){
  #     if (isolate(input$mode.couleur.sheet.4)=="Gradient"){
  #       #Palette Perso
  #       if (input$leg.ajust.sheet.4=="Echelle personnalisée"){
  #         g <- g+scale_fill_gradientn(colours=c(isolate(input$couleur1.map.sheet.4),isolate(input$couleur2.map.sheet.4)),
  #                                     limits=c(isolate(input$leg.gamme.min.sheet.4),isolate(input$leg.gamme.max.sheet.4)),
  #                                     na.value = input$col.na.sheet.4)
  #       } else {
  #         g <- g+scale_fill_gradientn(colours=c(isolate(input$couleur1.map.sheet.4),isolate(input$couleur2.map.sheet.4)),
  #                                     na.value = input$col.na.sheet.4)
  #       }
  #       
  #       #Palette Brewer
  #     } else {
  #       if (input$leg.ajust.sheet.4=="Echelle personnalisée"){
  #         g <- g+scale_fill_distiller(palette=isolate(input$palette.brewer.sheet.4),
  #                                     direction = isolate(input$palette.direction.sheet.4),
  #                                     limits=c(isolate(input$leg.gamme.min.sheet.4),isolate(input$leg.gamme.max.sheet.4)),
  #                                     na.value = input$col.na.sheet.4)
  #       }else {
  #         g <- g+scale_fill_distiller(palette=isolate(input$palette.brewer.sheet.4),
  #                                     direction = isolate(input$palette.direction.sheet.4),
  #                                     na.value = input$col.na.sheet.4)
  #       }
  #     }
  #   } else {
  #     
  #     if (isolate(input$mode.couleur.sheet.4)=="Gradient"){
  #       g <- g +
  #         scale_fill_manual(values=colorRampPalette(c(isolate(input$couleur1.map.sheet.4),isolate(input$couleur2.map.sheet.4)))(input$nb.interv.sheet.4),
  #                           guide = guide_legend(reverse=TRUE),
  #                           na.value = input$col.na.sheet.4)
  #     } else {
  #       g <- g+
  #         scale_fill_brewer(palette=isolate(input$palette.brewer.sheet.4), direction = isolate(input$palette.direction.sheet.4),
  #                           guide = guide_legend(reverse=TRUE),
  #                           na.value = input$col.na.sheet.4)
  #     }
  #     
  #   }
  #   
  #   g <- g +
  #     coord_sf(datum=NA)+#Supprime les graduations
  #     theme(panel.background=element_rect(fill="white"))+#Fond blanc au lieu de gris
  #     labs(fill=input$leg.titre.sheet.4,title = input$titre.sheet.4, subtitle = input$sous.titre.sheet.4, caption=glue("{input$text1.source.sheet.4} \n {input$text2.source.sheet.4}"))+
  #     theme(plot.title = element_text(size=input$taille.titre.sheet.4, hjust=input$alignement.titre.sheet.4, face=input$style.titre.sheet.4, color=input$couleur.titre.sheet.4))+
  #     theme(plot.subtitle=element_text(size=input$taille.sous.titre.sheet.4, hjust=input$alignement.sous.titre.sheet.4, face=input$style.sous.titre.sheet.4, color=input$couleur.sous.titre.sheet.4))+
  #     theme(plot.caption = element_text(size=input$taille.source.sheet.4, hjust=input$alignement.source.sheet.4, face=input$style.source.sheet.4, color=input$couleur.source.sheet.4))+
  #     theme(legend.title = element_text(colour=input$leg.titre.col.sheet.4, face=input$leg.titre.style.sheet.4, size=input$leg.titre.size.sheet.4))+
  #     theme(legend.text=element_text(colour=input$leg.chiffre.col.sheet.4, face=input$leg.chiffre.style.sheet.4, size=input$leg.chiffre.size.sheet.4))+
  #     theme(legend.position=input$leg.position.sheet.4)+
  #     theme(plot.margin=margin(5,5,30,5))
  #   
  #   if (input$ajout.text.sheet.4){
  #     g <- g + geom_text(aes(label=round(VALUE,digits = input$nb.dec.sheet.4),x=LON,y=LAT),col=input$col.text.sheet.4,size=input$taille.text.sheet.4)+ xlab("")+ylab("")
  #   }
  #   
  #   # if (input$north.arrow.sheet.4){
  #   #   g <- g + north(fond4(),symbol = input$type.north.arrow.sheet.4,location=input$location.north.arrow.sheet.4,scale=input$taille.north.arrow.sheet.4)
  #   # }
  #   #
  #   if (input$logo.ti.sheet.4){
  #     g <- g +
  #       annotation_custom(rasterGrob(ti), xmin.ti, xmax.ti, ymin.ti, ymax.ti)
  #   }
  #   if (input$logo.tu.sheet.4){
  #     g <- g +
  #       annotation_custom(rasterGrob(tu), xmin.tu, xmax.tu, ymin.tu, ymax.tu)
  #   }
  #   
  #   g <- ggplot_gtable(ggplot_build(g))
  #   g$layout$clip[g$layout$name=="panel"] <- "off"
  #   grid.draw(g)
  #   
  #   g
  })
  
  
  

  #Carte
  output$map.sheet.4 <- renderPlot({
    # input$valider.palette.sheet.4
    # # validate(need(data.sheet.4() %>% summarise(sum(VALUE)) %>% pull() != 0,"Pas de données disponibles pour les critères choisis"))
    # # validate(need(MapOutputSheet4(), "Carte non disponible"))
    # validate(need(length(input$choix.entite.sheet.4)>0,"Choisir les entités géographiques à représenter"))
    # validate(need(length(unique(LIMITS()))==input$nb.interv.sheet.4+1,message = "Il existe un problème dans les intervalles renseignés ci-dessous : pas assez de valeurs seuils ou valeurs seuils identiques. N'oubliez pas qu'il faut renseigner un nombre de valeurs seuil différentes suffisant (nombre d'intervalles souhaités +1). Ces valeurs seuils doivent être séparées par des /. Vous pouvez y insérer des espaces pour plus de visisbilité"))
    # validate(need(all(sort(LIMITS())==LIMITS()),message = "Il existe un problème dans les intervalles renseignés ci-dessous : n'oubliez pas qu'il faut renseigner les valeurs seuils rangées dans l'ordre croissant."))
    # validate(need(input$niveau!="National","Une carte des données nationales n'a pas vraiment de sens."))
    # validate(need(!(input$type.echelle.sheet.4=="Discrète" & input$nb.interv.sheet.4>9 & isolate(input$mode.couleur.sheet.4)=="Palette"),"Avec une palette prédéfinie, vous ne pouvez pas contruire plus de 9 classes."))
    # # validate(need(nrow(eff.sheet.4())>=input$nb.interv.sheet.4,"Le découpage actuel n'aboutit pas au nombre d'intervalles souhaités. Veuillez personnaliser vos intervalles et assurez-vous qu'aucun d'entre eux n'est vide."))
    MapOutputSheet4()
  })

  # #Téléchargement de la carte
  # output$down.sheet.4 = downloadHandler(
  #   filename = function() {paste("Carte","-",paste(culture(),collapse = '+'),"-",input$critere,"-",input$annee.sheet.4,"-",Sys.Date(),".",input$save.type.sheet.4,sep="")},
  #   content = function(file) {
  #     ggsave(file,plot = MapOutputSheet4(),dpi=600)
  #   }
  # )

  #Affichage des données
  output$df.sheet.4 <- DT::renderDataTable(data_map(),filter = 'top', server = TRUE, rownames = FALSE,
                                           options = list(autoWidth = TRUE), selection = 'none') # v3.4.1 argument ne fonctionne pas , editable = FALSE

  #Téléchargement des données
  output$downloadData.sheet.4 <- downloadHandler(
    filename = function() {paste("Carte-",input$critere,"-",paste(culture(),collapse = '+'),"-",input$niveau,"-", Sys.Date(), ".csv", sep="")},
    content = function(file) {write.csv2(data.sheet.4(), file, row.names = FALSE)}
  )
  
  
  
})
