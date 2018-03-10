#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


options( java.parameters = "-Xmx32g" )

.lib<- c("shiny","lubridate","jsonlite","dplyr", "stringr", "tidyr","ggplot2", "plotly", "DT","reshape2","visNetwork","ggalt","knitr",
         "grid","gridExtra", "igraph","shinyjs", "shinyBS", "shinythemes", "shinycssloaders", "shinyWidgets", "shinyWidgets", "shinythemes",
         "Hmisc","openair", "visNetwork")

.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)



# polen_meteo_all$Populus
# polen_meteo_all$Morus
# polen_meteo_all$Quercus
# polen_meteo_all$Fraxinus
# polen_meteo_all$Ligustrum
# polen_meteo_all$Ulmus  
# polen_meteo_all$Platanus
# polen_meteo_all$Pinus
# polen_meteo_all$Olea
# polen_meteo_all$Cupressaceae.Taxaceae
# polen_meteo_all$Casuarina


polen_meteo_all <- readRDS("polen_meteo_all_current_norm.rds")
polen_meteo_all_orig <- readRDS("polen_meteo_all_current.rds")

pollution <- readRDS("pollution_orig.rds")
pollution_norm <- readRDS("pollution_norm.rds")

pm <- readRDS("pm_orig.rds")
pm_norm <- readRDS("pm_norm.rds")


polen_meteo_all <- polen_meteo_all[!is.na(polen_meteo_all$Fecha),]
polen_meteo_all$Pred <- "Hist"
polen_meteo_all[which(polen_meteo_all$Fecha >= today()),"Pred"] <- "Pred"

polen_meteo_all_orig <- polen_meteo_all_orig[!is.na(polen_meteo_all_orig$Fecha),]
polen_meteo_all_orig$Pred <- "Hist"
polen_meteo_all_orig[which(polen_meteo_all_orig$Fecha >= today()),"Pred"] <- "Pred"


pollution <- readRDS("pollution_orig.rds")
pollution_norm <- readRDS("pollution_norm.rds")


pollution_norm <- pollution_norm[!is.na(pollution_norm$Fecha),]
pollution_norm$Pred <- "Hist"
pollution_norm[which(pollution_norm$Fecha >= today()),"Pred"] <- "Pred"

pollution <- pollution[!is.na(pollution$Fecha),]
pollution$Pred <- "Hist"
pollution[which(pollution$Fecha >= today()),"Pred"] <- "Pred"



pm <- readRDS("pm_orig.rds")
pm_norm <- readRDS("pm_norm.rds")

pm_norm <- pm_norm[!is.na(pm_norm$Fecha),]
pm_norm$Pred <- "Hist"
pm_norm[which(pm_norm$Fecha >= today()),"Pred"] <- "Pred"

pm <- pm[!is.na(pm$Fecha),]
pm$Pred <- "Hist"
pm[which(pm$Fecha >= today()),"Pred"] <- "Pred"


vlc <- c("Populus", "Morus", "Quercus", "Fraxinus", "Ligustrum", "Ulmus", "Platanus", "Pinus", 
         "Olea", "Cupressaceae.Taxaceae", "Casuarina")

estacionalidades <- read.csv("Estacionalidad_Polen.csv")
estacionalidades <- estacionalidades[,c("Mes",colnames(estacionalidades)[colnames(estacionalidades)%in%vlc])]
n<- colnames(estacionalidades[,-1])
Nodes <- data.frame(id = 1:length(n), label = n)


AllergensGlobal <- "Platanus"
colA <- "darkseagreen3"
colB <- "deepskyblue3"

data.range <- NA


#polen_meteo_all$Fecha >= today()
#range(polen_meteo_all$Fecha, na.rm = T)
#sum(is.na(polen_meteo_all$Fecha))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  observeEvent(input$TypeParticle,{
    if(input$TypeParticle == "Pollen"){
      observeEvent(input$Pollens,{
        for (i in 1:length(input$Pollens)){
          dat <- polen_meteo_all_orig
          max <- max(dat[input$Pollens])
          level <- filter(dat, Fecha == today())[input$Pollens]
          
          if (level <= max/5){
            showNotification(paste("The level for ", input$Pollens[i], "is : ", level, " - NO ALERT", sep = ""), type = "message")
            #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
          }else{
            if(level <= 2*(max/5)){
              showNotification(paste("The level for ", input$Pollens[i], "is : ", level, " - MEDIUM ALERT",sep = ""), type = "warning")
              #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
            }else{
              showNotification(paste("The level for ", input$Pollens[i], "is : ", level," - ALERT", sep = ""), type = "error")
              #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
            }
          }
          
        }
      })

    }else{
      if(input$TypeParticle == "Air Pollution"){
        #showNotification(paste("This are no Alerts for ", input$Pollutions, " today :)", sep = ""), type = "message")
        observeEvent(input$Pollutions,{
          for (i in 1:length(input$Pollutions)){
            #showNotification(paste("There are no ALERTS for ", input$Pollutions[i], " today :)", sep = ""), type = "message")
            dat <- pollution
            max <- max(dat[input$Pollutions])
            level <- filter(dat, Fecha == today())[input$Pollutions]
            
            if (level <= max/5){
              showNotification(paste("The level for ", input$Pollutions[i], "is : ", level, " - NO ALERT", sep = ""), type = "message")
              #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
            }else{
              if(level <= 2*(max/5)){
                showNotification(paste("The level for ", input$Pollutions[i], "is : ", level, " - MEDIUM ALERT",sep = ""), type = "warning")
                #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
              }else{
                showNotification(paste("The level for ", input$Pollutions[i], "is : ", level," - ALERT", sep = ""), type = "error")
                #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
              }
            }
            
          }
        })

      }else{
        #showNotification(paste("This are no Alerts for ", input$Others, " today :)", sep = ""), type = "message")
        observeEvent(input$Others,{
          for (i in 1:length(input$Others)){
            #showNotification(paste("There are no ALERTS for ", input$Others[i], " today :)", sep = ""), type = "message")
            dat <- pm
            max <- max(dat[input$Others])
            level <- filter(dat, Fecha == today())[input$Others]
            
            if (level <= max/5){
              showNotification(paste("The level for ", input$Others[i], "is : ", level, " - NO ALERT", sep = ""), type = "message")
              #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
            }else{
              if(level <= 2*(max/5)){
                showNotification(paste("The level for ", input$Others[i], "is : ", level, " - MEDIUM ALERT",sep = ""), type = "warning")
                #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
              }else{
                showNotification(paste("The level for ", input$Others[i], "is : ", level," - ALERT", sep = ""), type = "error")
                #showNotification(paste("There are no ALERTS for ", input$Pollens[i], " today :)", sep = ""), type = "message")
              }
            }
            
          }
        })

      }
    }
  })
  
  
 
    
    
   
  
  
  
   
  output$Aller.Temp <- renderPlotly({
    
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }
    
    AllergensGlobal <<- inputAllergens
    print(inputAllergens)
    inputDataRange <- input$DataRange
    print(inputDataRange)
    plotAller(inputDataRange, inputAllergens, dat, dat.norm, "Temp")
  })

  output$Aller.Prec <- renderPlotly({
    
    
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }    #print(inputAllergens)
    inputDataRange <- input$DataRange
    #print(inputDataRange)
    plotAller(inputDataRange, inputAllergens,  dat, dat.norm, "Prec")
  })
  
  output$Aller.Cloud <- renderPlotly({
    
    
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }    #print(inputAllergens)
    inputDataRange <- input$DataRange
    #print(inputDataRange)
    plotAller(inputDataRange, inputAllergens, dat, dat.norm, "Cloud")
  })
  
  output$Aller.Wind <- renderPlotly({
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }    #print(inputAllergens)
    inputDataRange <- input$DataRange
    #print(inputDataRange)
    plotAller(inputDataRange, inputAllergens, dat, dat.norm, "Speed")
  })
  
  output$Aller.Sun <- renderPlotly({
    
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }    #print(inputAllergens)
    inputDataRange <- input$DataRange
    #print(inputDataRange)
    plotAller(inputDataRange, inputAllergens, dat, dat.norm, "Sun")
  })
  
  output$Aller.Press <- renderPlotly({
    
    
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }    #print(inputAllergens)
    inputDataRange <- input$DataRange
    #print(inputDataRange)
    plotAller(inputDataRange, inputAllergens, dat, dat.norm, "Press")
  })
  
  output$Aller.windRose <- renderPlot({
    
    
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }    #print(inputAllergens)
    inputDataRange <- input$DataRange
    #print(inputDataRange)
    plotAller(inputDataRange, inputAllergens, dat, dat.norm, "windRose")
  })
  
  output$Aller.pollRose <- renderPlot({
    
    
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }    #print(inputAllergens)
    inputDataRange <- input$DataRange
   # print(inputDataRange)
    plotAller(inputDataRange, inputAllergens, dat, dat.norm, "pollRose")
  })
  
  
  output$Aller.Calendar <- renderPlot({
    Allergens <- input$Pollens
    
    print("Calendar PLOT")
    data <- polen_meteo_all_orig
    
    polen_meteo_all.2017 <- filter(data, Fecha <= ymd("2017-12-31"),  Fecha >= ymd("2017-01-01"))
    polen_meteo_all.2017.s <- select(polen_meteo_all.2017, Fecha, Wind.direction, Wind.speed,Allergens[1])
    colnames(polen_meteo_all.2017.s)<- c("date","wd","ws", Allergens[1])

    labs <- levels(cut(polen_meteo_all.2017.s[,Allergens[1]], 4))
    bins <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
                  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
    print(bins[,2])
    calendarPlot(polen_meteo_all.2017.s, pollutant = Allergens[1], breaks = c(0,0.01,bins[,2]),
                 labels = c("No", "Very low", "Low", "High", "Very High"), 
                 cols = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
                 statistic = "max", year = 2017, layout = c(4, 3), size = 10)
  })
  
  output$Flan <-  renderVisNetwork({
    
    Allergens <- input$Pollens

    # Detectar las filas (meses) del polen seleccionado
    idsAller <- which(Nodes$label %in% Allergens)
    Allergens <- as.character(Nodes[which(Nodes$label %in% Allergens),"label"])
    
    colNums <- match(c(Allergens),names(estacionalidades))
    polenes_coincidentes_strong <- list()
    polenes_coincidentes_weak <- list()
    coincid <- c()
    for(i in 1:length(Allergens)){
      # Filtrar las filas de la estacionalidad (también quitamos la columna del mes)
      tmp <- estacionalidades[which(estacionalidades[,Allergens[i]] != 0), -1]
      tmp2 <- tmp[colSums(tmp != 0) > 0]
      # Extraer los polenes que coinciden en su epoca de maxima polinizacion
      polenes_coincidentes_strong[[i]] <- tmp2[colSums(tmp2) > 1]
      polenes_coincidentes_weak[[i]] <- tmp2[colSums(tmp2) <= 1]
      coincid <- c(coincid, ncol(tmp2))
    }
    
    # Parte VisNetwork
    # Creacion de los nodos
    # palette <- distinctColorPalette(length(names(polenes_coincidentes)))
    node_id <-  data.frame(id = idsAller,
                           label = Allergens, 
                           icon.color = c("#08A46B"),
                           shape = c("dot"),
                           who = which(Nodes$label %in% Allergens),
                           type = c(rep("0", length(Allergens)))
    )
    nodes <- list()
    for (i in 1:length(Allergens)){
      
      idsS <- which(Nodes$label %in% colnames(polenes_coincidentes_strong[[i]]))
      idsW <- which(Nodes$label %in% colnames(polenes_coincidentes_weak[[i]]))
      ids <- c(idsS, idsW)
      nodes[[i]] <- data.frame(id = ids,
                               # add labels on nodes
                               label = Nodes[ids,"label"],
                               #color = palette[1:length(names(polenes_coincidentes))],
                               icon.color = "#82D5B7",
                               shape = c("dot"),
                               who = node_id$id[i],
                               type = c(rep("2", length(idsS)), rep("1", length(idsW)))
      )
      
    }
    
    nodes_all <- plyr::ldply(nodes, data.frame)
    nodes_all_2 <- rbind(node_id, nodes_all)
    
    edges <- list()
    values <- c("white", "#ffcc00", "#ff6666")
    names(values) <- c("0", "1", "2")
    rels <- c("", "Weak", "Strong")
    names(rels) <- c("0", "1", "2")
    wid <- c(0, 4,4)
    names(wid) <- c("0", "1", "2")
    
    for(i in 1:length(Allergens)){
      
      edges <- data.frame(
        from = nodes_all_2$id,
        to = nodes_all_2$who,
        edges$color <- values[nodes_all_2$type],
        edges$label <- rels[nodes_all_2$type],
        shape = c("dot"),
        width = wid[nodes_all_2$type]) 
      
    }
    
    
    colnames(edges)[3] <- "color"
    colnames(edges)[4] <- "label"
    
    nodes_all_2[which(nodes_all_2$label %in% Allergens),"icon.color"] <- "#08A46B"
    nodupnodes <- unique(nodes_all_2[1:3])
   
    edges <- filter(edges, from != to)
    
    nodupnodes$shape = "icon"
    nodupnodes$icon.face = "Ionicons"
    nodupnodes$icon.code = "f1fd"
    
    print(edges)
    
    
    
    network <- visNetwork(nodupnodes, edges, height = "1000px", width = "100%")
    network %>%
      addIonicons()%>%
      #visIgraphLayout()%>%
      visPhysics(stabilization = FALSE) %>% 
      #visInteraction(navigationButtons = TRUE) %>% 
      visOptions(highlightNearest = TRUE) 
    
  })
  
  
  
  output$detailDT <-  DT::renderDataTable({
    inputType <- input$TypeParticle
    
    if (inputType == "Pollen"){
      inputAllergens <- input$Pollens
      dat <- polen_meteo_all_orig
      dat.norm <- polen_meteo_all
    }else{
      if(inputType == "Air Pollution"){
        inputAllergens <- input$Pollutions
        dat <- pollution
        dat.norm <- pollution_norm
      }else{
        inputAllergens <- input$Others
        dat <- pm
        dat.norm <- pm_norm
      }
    }     
    
    tempDT <- select(data.range, c("Fecha",inputAllergens))
    
    dt <- datatable(tempDT,  extensions = c('Buttons'), filter = 'top', selection = 'multiple', 
                    options = list(pageLength = 10,
                                   dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                   #sDom  = '<"top">lrt<"bottom">ip'
                    )#,
                    # caption = htmltools::tags$caption(
                    #   style = 'caption-side: top; text-align: center; color: black ;',
                    #   htmltools::h3("Particle Levels"))
                    )
      for(i in 1:length(inputAllergens)){
        dt <- dt %>% formatStyle(inputAllergens[i],
                           background = styleColorBar(range(tempDT[,inputAllergens[i]]), 'lightblue'),
                           backgroundSize = '98% 88%',
                           backgroundRepeat = 'no-repeat',
                           backgroundPosition = 'center')
      }
      
      dt
    
    
    # %>% formatStyle(
                      #   'Alert',target = 'row',
                      #   backgroundColor = styleEqual(c("No","Very Low","High","Very High"), 
                      #                                c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"))
                      # )
   

  })
  
  

  output$textPollen <- renderUI({
    #install.packages("readtext")
    library(readtext)
    
    Allergens <- input$Pollens
    
    txt <- paste(readLines(paste("./pollen_info/",Allergens[1],"_EN.txt",sep="")), collapse = "")
    paste(txt)
    
    })
  

  
  output$ParticleData.csv <- downloadHandler(
    
    filename = function() {paste("ParticleData.csv", sep='') },
    
    content = function(file) {
      
      data.f <- select(polen_meteo_all_orig, Fecha, AllergensGlobal)
      data.f <- filter(data.f, Fecha <=today())
      write.csv(data.f, file)
    }
  ) 
  
  output$infoPollen <- renderUI({
    
    Allergens <- input$Pollens
    includeHTML(paste("./www/info/",tolower(Allergens[1]),".html",sep=""))
  })
  output$infoPollen2 <- renderUI({
    
    Allergens <- input$Pollens
    includeHTML(paste("./www/info/",tolower(Allergens[1]),"_img.html",sep=""))
  })
  
  
  output$mapPollen <- renderUI({
    
    Allergens <- input$Pollens
    includeHTML(paste("./pollen_info/",tolower(Allergens[1]),".html",sep=""))
  })
  
  output$about <- renderUI({
    Allergens <- input$Pollens
    
    includeHTML("about.html")
  })
  
  output$titleInfo <- renderUI({

        Allergens <- input$Pollens
      HTML(paste("<h1 style=\"color: #428bca;\">",Allergens[1]," Pollen </h1>", sep=""))

  })
  
  
  
  
  # dataNorm <- polen_meteo_all
  # Allergens <- "Platanus"
  # Range <- "Currently"
  
  plotAller <- function(Range,Allergens,data, dataNorm, what){
    
      if(Range == "2 years ago"){
        data.range <<- filter(data, Fecha <= (today() + months(1)), Fecha >= (today() - months(24)))
        data.range.norm <- filter(dataNorm, Fecha <= (today() + months(1)), Fecha >= (today() - months(24)))
        
      }else{
        if(Range == "1 year ago"){
          data.range <<- filter(data, Fecha <= (today() + months(1)), Fecha >= (today() - months(12)))
          data.range.norm <- filter(dataNorm, Fecha <= (today() + months(1)), Fecha >= (today() - months(12)))
          
        }else{
          if(Range == "6 months ago"){
            data.range <<- filter(data, Fecha <= (today() + months(1)), Fecha >= (today() - months(6)))
            data.range.norm <- filter(dataNorm, Fecha <= (today() + months(1)), Fecha >= (today() - months(6)))
            
          }else{
            if(Range == "3 months ago"){
              data.range <<- filter(data, Fecha <= (today() + months(1)), Fecha >= (today() - months(3)))
              data.range.norm <- filter(dataNorm, Fecha <= (today() + months(1)), Fecha >= (today() - months(3)))
              
            }else{
              if(Range == "1 month ago"){
                data.range <<- filter(data, Fecha <= (today() + days(7)), Fecha >= (today() - months(1)))
                data.range.norm <- filter(dataNorm, Fecha <= (today() + days(7)), Fecha >= (today() - months(1)))
                
              }else{
                if(Range == "Currently"){
                  data.range <<- filter(data, Fecha <= (today() + days(3)), Fecha >= (today() - days(x=7)))
                  data.range.norm <- filter(dataNorm, Fecha <= (today() + days(3)), Fecha >= (today() - days(x=7)))
                  
                }else{
                  if(Range == "Next week"){
                    data.range <<- filter(data, Fecha >= (today() - days(3)), Fecha <= (today() + days(x=7)))
                    data.range.norm <- filter(dataNorm, Fecha >= (today() - days(2)), Fecha <= (today() + days(x=7)))
                    
                  }else{
                    if(Range == "Next month"){
                      data.range <<- filter(data, Fecha >= (today() - days(3)), Fecha <= (today() + months(1)))
                      data.range.norm <- filter(dataNorm, Fecha >= today() - days(2), Fecha <= (today() + months(1)))
                      
                    }else{
                      if(Range == "Next 3 months"){
                        data.range <<- filter(data, Fecha >= (today() - days(7)), Fecha <= (today() + months(3)))
                        data.range.norm <- filter(dataNorm, Fecha >= (today() - days(7)), Fecha <= (today() + months(3)))
                        
                      }
                      
                    }
                  }
                }
              }
            }
          }
        } 
        }
        
        

      if(what == "Temp"){
        colNums <- match( c("Fecha", "Pred", "Humidity", "Mean.Temperature","Min.Temperature","Max.Temperature", Allergens),names(data.range.norm))
        data.sel <- select(data.range.norm,  colNums)
        data.sel.melt <- melt(data.sel, id.vars = c("Fecha", "Pred", "Humidity", "Mean.Temperature","Min.Temperature","Max.Temperature"))
        data.sel.melt[is.na(data.sel.melt$Humidity),"Humidity"] <- 0
        
        cutoff.Hist <- data.frame( x1 = min(data.sel.melt$Fecha), x2 = today(), y1 = -0.05, y2 = 1.15)
        cutoff.Pred <- data.frame( x1 = today(), x2= max(data.sel.melt$Fecha), y1 = -0.05, y2 = 1.15)
        
        hoy <- data.frame(x = c(today(),today()), y = c(-0.05,1.15))
        # print(cutoff.Hist)
        # print(data.sel.melt)
        gs.pal <- colorRampPalette(c(colA,colB),bias=.1,space="rgb")
        
        g <- ggplotly(ggplot(data.sel.melt) + 
          geom_rect(data=cutoff.Hist, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=colA, alpha=0.15) + 
          geom_rect(data=cutoff.Pred, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=colB, alpha=0.15) + 
          geom_area(data = filter(data.sel.melt, variable%in%Allergens[1]),aes(x = Fecha,y = Humidity), linetype = "dashed", alpha = 0.4, fill = "khaki1") +            
          geom_ribbon(aes(x = Fecha, ymin =Min.Temperature , ymax = Max.Temperature), fill = "grey70") +
          geom_line(aes(x = Fecha, y = Mean.Temperature), colour = "black") +
          geom_line(aes(x,y),hoy, size = 0.8, linetype ="dotted", colour ="black", alpha = 0.5) + 
            
          # geom_line(aes(x, y), data = cutoff.Hist.l, size = 6,alpha = 0.6, colour = colA) +  
          # geom_line(aes( x, y), cutoff.Pred.l, size = 6,alpha = 0.6, colour = colB) +  
            
          #geom_rect(data=cutoff.Hist, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color=colA, alpha=0.5) + 
          # geom_rect(data=cutoff.Pred, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color=colB, alpha=0.5) + 
          
            
          annotate("text", mean(filter(data.sel.melt, Pred == "Hist")$Fecha), 1.05, hjust = 1, label = "Historical", colour = "black", size = 3)+
          annotate("text", mean(filter(data.sel.melt, Pred == "Pred")$Fecha) , 1.05, hjust = 1, label = "Prediction", colour = "black", size = 3)+
          annotate("text", today() , -0.1, hjust = 1, label = "Today", colour = "black", size = 3)+
            
          #geom_smooth( aes(Fecha, value, colour = variable, span = 0))+
          geom_line( aes(Fecha, value, colour = variable, linetype = as.factor(Pred)))+
          guides(linetype=FALSE)+
          theme_light() + ylab("") + xlab("") + ggtitle("Temperature & Humidity vs. Allergens") +
          scale_fill_manual(values=gs.pal(2)) +
            theme(#legend.position = "bottom",
                  #legend.key.size =  unit(0.1, "in"),
                  # axis.title = element_text(size = 12),
                  # axis.text=element_text(size=7),
                  # axis.text.x = element_text(size = 10,angle = 0, hjust = 1),
                  # axis.text.y = element_text(size = 10, hjust = 1),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  strip.background = element_blank(),
                  strip.text=element_text(vjust=0, colour = "black", face = "bold",hjust = 0)
            ))%>% layout(legend = list(orientation = 'h', x = 0.02, y = -0.080,  
                                       font = list(family = "sans-serif",
                                                   size = 10,
                                                   color = "#000"))) 
        return(g)
        
      }else{
        if(what%in%c("Prec","Cloud","Speed","Sun","Press")){
          
          if (what == "Prec"){
            col <- "Precipitation.amount"
            title <- "Precipitations vs Allergens"
            cA <- "dodgerblue"
          }else{
            if(what == "Cloud"){
              col <- "Cloud.cover"
              title <- "Cloud coverage vs Allergens"
              cA <- "black"
            }else{
              if(what == "Speed"){
                col <- "Wind.speed"
                title <- "Wind vs Allergens"
                cA <- "black"
              }else{
                if(what == "Sun"){
                  col <- "Sunshine.duration"
                  title <- "Sunshine duration vs Allergens"
                  cA <- "gold3"
                }else{
                  if(what == "Press"){
                    col <- "Sea.level.pressure"
                    title <- "Pressure vs Allergens"
                    cA <- "gray"
                }
              }
            }
          }
        }
        
          colNums <- match( c("Fecha", "Pred", Allergens, col),names(data.range.norm))
          data.sel <- select(data.range.norm,  colNums)
          data.sel.melt <- melt(data.sel, id.vars = c("Fecha", "Pred", col))
          
          cutoff.Hist <- data.frame( x1 = min(data.sel.melt$Fecha), x2 = today(), y1 = -0.05, y2 = 1.15)
          cutoff.Pred <- data.frame( x1 = today(), x2= max(data.sel.melt$Fecha), y1 = -0.05, y2 = 1.15)
          
          f2 <- list(
            size = 8
          )
          
          x <- list(
            tickfont = f2
          )
          
          g <- ggplot(data.sel.melt) + 
                          geom_rect(data=cutoff.Hist, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=colA, alpha=0.15) + 
                          geom_rect(data=cutoff.Pred, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=colB, alpha=0.15) 
                          
                          #geom_line(aes_string(x = "Fecha", y = col), colour = cA, size = 0.6) +
                          if(what == "Sun"){
                            g <- g + geom_point(aes_string(x = "Fecha", y = col), shape = 8, colour = cA, fill = "blac", size = 1)
                          }else{
                            if(what == "Press"){
                              g <- g + geom_line(aes_string(x = "Fecha", y = col), colour = cA, size = 0.6) +
                                  geom_point(aes_string(x = "Fecha", y = col), shape = 8, colour = cA, size = 1)
                            }else{
                              if (what == "Prec"){
                                g <- g + geom_bar(aes_string(x = "Fecha", y = col), colour = cA, stat = "identity", size = 1) 

                                
                              }
                            }
                          }
                         
                          
                          # geom_line(aes( x, y), cutoff.Hist, size = 6,alpha = 0.6, colour = colA) +  
                          # geom_line(aes( x, y), cutoff.Pred, size = 6,alpha = 0.6, colour = colB) +  
           g <- g + geom_line(aes(Fecha, value, colour = variable, linetype = as.factor(Pred)),size = 0.6)+
                          annotate("text", mean(filter(data.sel.melt, Pred == "Hist")$Fecha), 1.05, hjust = 1, label = "H", colour = "black", size = 3)+
                          annotate("text", mean(filter(data.sel.melt, Pred == "Pred")$Fecha) , 1.05, hjust = 1, label = "P", colour = "black", size = 3)+
                          
                          theme_light() + ylab("") + xlab("") + ggtitle(title) +
                          theme(#legend.position = "bottom",
                                #legend.key.size =  unit(0.1, "in"),
                                #axis.title = element_text(size = 12),
                                #axis.text=element_text(size=7),
                                #axis.text.x = element_text(size = 10,angle = 0, hjust = 1),
                                #axis.text.y = element_text(size = 10, hjust = 1),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                strip.background = element_blank(),
                                strip.text=element_text(vjust=0, colour = "black", face = "bold",hjust = 0)
                          )

        g <- ggplotly(g) %>% layout(legend = list(orientation = 'h', x = 0.02, y = -0.080,  
                                                      font = list(family = "sans-serif",
                                                                  size = 10,
                                                                  color = "#000")),
                                        xaxis = x) 
          return(g)
          
        }else{
          if(what%in%c("Prec")){
              if (what == "Prec"){
                col <- "Precipitation.amount"
                title <- "Precipitations vs Allergens"
              }
              colNums <- match( c("Fecha", "Pred", Allergens, col),names(data.range.norm))
              data.sel <- select(data.range.norm,  colNums)
              data.sel.melt <- melt(data.sel, id.vars = c("Fecha", "Pred", col))
              
              cutoff.Hist <- data.frame( x1 = min(data.sel.melt$Fecha), x2 = today(), y1 = -0.05, y2 = 1.15)
              cutoff.Pred <- data.frame( x1 = today(), x2= max(data.sel.melt$Fecha), y1 = -0.05, y2 = 1.15)
              
              f2 <- list(
                size = 8
              )
              
              x <- list(
                tickfont = f2
              )
              
              g <- ggplotly(ggplot(data.sel.melt) + 
                              geom_rect(data=cutoff.Hist, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=colA, alpha=0.15) + 
                              geom_rect(data=cutoff.Pred, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=colB, alpha=0.15) + 
                              
                              #geom_line(aes_string(x = "Fecha", y = col), colour = "black", size = 0.6) +
                              geom_bar(aes_string(x = "Fecha", y = col), colour = "dodgerblue", stat = "identity", size = 1) +
                              geom_line(aes(Fecha, value, colour = variable, linetype = as.factor(Pred)),size = 0.6)+
                              
                              # geom_line(aes( x, y), cutoff.Hist, size = 6,alpha = 0.6, colour = colA) +  
                              # geom_line(aes( x, y), cutoff.Pred, size = 6,alpha = 0.6, colour = colB) +  
                              annotate("text", mean(filter(data.sel.melt, Pred == "Hist")$Fecha), 1.05, hjust = 1, label = "H", colour = "black", size = 3)+
                              annotate("text", mean(filter(data.sel.melt, Pred == "Pred")$Fecha) , 1.05, hjust = 1, label = "P", colour = "black", size = 3)+
                              
                              theme_light() + ylab("") + xlab("") + ggtitle(title) +
                              theme(#legend.position = "bottom",
                                #legend.key.size =  unit(0.1, "in"),
                                #axis.title = element_text(size = 12),
                                #axis.text=element_text(size=7),
                                #axis.text.x = element_text(size = 10,angle = 0, hjust = 1),
                                #axis.text.y = element_text(size = 10, hjust = 1),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                strip.background = element_blank(),
                                strip.text=element_text(vjust=0, colour = "black", face = "bold",hjust = 0)
                              )) %>% layout(legend = list(orientation = 'h', x = 0.02, y = -0.080,  
                                                          font = list(family = "sans-serif",
                                                                      size = 10,
                                                                      color = "#000")),
                                            xaxis = x) 
              return(g)
          
          }else{
            if(what%in%c("windRose")){
            #print(Allergens[1])
            data_wind <- select(data.range, Fecha, Wind.direction, Wind.speed, Allergens[1])
            #print(colnames(data_wind))
            #print(data_wind)
            colnames(data_wind)<- c("date","wd","ws", Allergens[1])
            return(windRose(data_wind, key.footer = "knots", breaks = c(0, 10, 20, 40, 80), key.position="right", cols = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")))
            
          }else{
            if(what%in%c("pollRose")){
              #print(Allergens[1])
              poll_wind <- select(data.range, Fecha, Wind.direction, Wind.speed, Allergens[1])
              #print(colnames(poll_wind))
              colnames(poll_wind)<- c("date","wd","ws", Allergens[1])
              return(pollutionRose(poll_wind, pollutant = Allergens[1], breaks = c(0, 20, 40, 60, 80), cols = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"))
              )
              
            }else{
              return(null)
              
            }
            
          }
        }
        }
        }
          
    
    
  }

  
  
})
