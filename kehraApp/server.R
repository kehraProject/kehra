library(leaflet)
library(dplyr)
library(rgdal)
library(googleVis)

shinyServer(function(input, output, session) {
  
  dataset1 <- reactive({df1 <- bbox[bbox$Name == input$location, ]})

  ## Interactive Map ###########################################################

  # Create the map
  output$map <- renderLeaflet({
    df1 <- dataset1()
    leaflet() %>%
      # Base groups #
      # addTiles(group = "OSM (default)") %>%
      addProviderTiles("Esri.NatGeoWorldMap", group = "NatGeoWorldMap") %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Streets") %>%
      setView(lng = df1[,"longmid"], 
              lat = df1[,"latmid"], 
              zoom=df1[,"zoomlevel"]) %>%
      # Overlay groups #
      addPolygons(data=adm0, weight=2, group = "Study Areas") %>%
      addPolygons(data=adm1, weight=2, 
                  group = "Admin Boundaries 1", col="yellow") %>%
      # addPolygons(data=adm2, weight=2, group = "Admin Boundaries 2", col="orange") %>%
      addPolygons(data=AralSea, weight=2, col="gray", group = "Aral Sea (late 1950s)") %>%
      addCircles(lng = nuclearSites$Lon, 
                 lat = nuclearSites$Lat, 
                 fillColor = "yellow", color = "yellow",  
                 weight = 3, radius = 10000, 
                 popup = paste(#"<b>Country:</b>", nuclearSites$Country, "<br>",
                               #"<b>Location:</b>", nuclearSites$Location,"<br>",
                               "<b>Site:</b>", nuclearSites$Site, "<br>",
                               #"<b>Latitude:</b>", nuclearSites$Lat, "<br>",
                               #"<b>Longitude:</b>", nuclearSites$Lon, "<br>",
                               "<b>Description:</b>", nuclearSites$Description),
                 group = "Nuclear sites") %>%
      addCircles(lng = SleepingTowns$Longitude, 
                 lat = SleepingTowns$Latitude, 
                 fillColor = "red", color = "red", 
                 weight = 3, radius = 20000, 
                 popup = paste("<b>Town:</b>", SleepingTowns$Towns, "<br>",
                               "<b>Description:</b>", SleepingTowns$Reason),
                 group = "Sleeping Towns") %>%
      addCircles(lng = mineralResources$Longitude, 
                 lat = mineralResources$Latitude, 
                 fillColor = "brown", color = "brown", 
                 weight = 3, radius = 10000, 
                 popup = paste("<b>Site name:</b>", mineralResources$SITE_NAME,
                               "<br>",
                               "<b>Description:</b>", mineralResources$URL),
                 group = "Mineral Resource Data System") %>%
      addCircles(lng = monitoredCities$LONGITUDE, 
                 lat = monitoredCities$LATITUDE,
                 fillColor = "red", color = "red", weight = 3, 
                 radius = ifelse(monitoredCities$POP_MAX > 1, 
                                 sqrt(abs(monitoredCities$POP_MAX))*30, 0),
                 group = "Populated places") %>%
      addCircles(lng = populatedPlaces$LONGITUDE, 
                 lat = populatedPlaces$LATITUDE,
                 fillColor = "orange", color = "orange", weight = 3, 
                 radius = ifelse(populatedPlaces$POP_MAX > 1, 
                                 sqrt(abs(populatedPlaces$POP_MAX))*30, 0), 
                 popup = paste("<b>",populatedPlaces$NAME, "</b><br>",
                               "Max population:", populatedPlaces$POP_MAX),
                 group = "Populated places") %>%
      # Layers control #
      addLayersControl(
        position = 'bottomleft',
        baseGroups = c("NatGeoWorldMap", "Streets"),
        overlayGroups = c("Study Areas", 
                          "Admin Boundaries 1", # "Admin Boundaries 2", 
                          "Aral Sea (late 1950s)",
                          "Nuclear sites",
                          "Sleeping Towns",
                          "Populated places",
                          "Mineral Resource Data System"),
        #overlayGroups = c("Nuclear sites", "AWS", "Health Risk Spots"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("Nuclear sites", "Populated places" ,
                  "Admin Boundaries 1", #"Admin Boundaries 2"
                  "Aral Sea (late 1950s)", "Sleeping Towns", 
                  "Mineral Resource Data System"
                  ))
  })
  
  ## Data explorer #############################################################
  
  output$plot1 <- renderPlot({
    
    VI <- readRDS("data/VulnerabilityIndex.rds")
    
    plot(VI$VulnerabilityI ~ VI$Ranking, 
         main = "KZ and UK are not amongst the most vulnerable countries to climate change",
         xlab="Ranking", ylab="Vulnerability Index")
    KZ <- VI[VI$Sovereign=="Kazakhstan",]
    points(KZ$VulnerabilityI ~ KZ$Ranking, col="red", pch=20, cex= 1.5)
    UK <- VI[VI$Sovereign=="U.K.",]
    points(UK$VulnerabilityI ~ UK$Ranking, col="blue", pch=20, cex= 1.5)
    legend("topleft", pch = 20, col=c("red","blue"), legend=c("KZ","UK"))
    
  })
  
#   ## Project's timeline ########################################################
#   
#   output$timeline <- renderGvis({
#     
#     datTL <- data.frame(Name=c("Admin", "Data Collection", "Model Building", 
#                                "Model Testing", "Model Debugging", 
#                                "Paper 1", "Paper 2", "Paper 3", 
#                                "Reporting", "Workshop"),
#                         Task=c("Andrew", "Allan", "Claudia",
#                                "Abraham", "Maged",
#                                "Aygul", "Sara", "Kanat", "Yelzhas", "Bella"),
#                         start=as.Date(x=c("2015-05-01", "2015-05-01",
#                                           "2015-10-01", "2015-10-01", 
#                                           "2015-10-01", "2015-10-01", 
#                                           "2015-10-01", "2015-10-01", 
#                                           "2015-10-01", "2015-10-01")),
#                         end=as.Date(x=c("2017-03-31", "2017-03-31", 
#                                         "2017-03-31", "2017-03-31", 
#                                         "2017-03-31", "2017-03-31", 
#                                         "2017-03-31", "2017-03-31", 
#                                         "2017-03-31", "2017-03-31")))
#     
#     gvisTimeline(data=datTL, 
#                  rowlabel="Name",
#                  barlabel="Task",
#                  start="start", 
#                  end="end",
#                  options=list(timeline="{groupByRowLabel:false}",
#                               backgroundColor='#ffd', 
#                               height=7500#,
#                               #colors="['#cbb69d', '#603913', '#c69c6e']"
#                               ))
#     
#   })

})
