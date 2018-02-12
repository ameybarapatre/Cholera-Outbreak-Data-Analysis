library(leaflet)


setwd('c:/users/prakash/desktop/cs 424')
pumps <-read.csv("choleraPumpLocations.csv", header=FALSE)
content<-paste0("<strong>Pump</strong>")

deaths <-read.csv("choleraDeathLocations.csv",header=FALSE)
popups<-sapply(deaths$V1,as.character)
popups <-paste("<strong>Number Of Deaths:</strong>", popups)

leaflet() %>%
  setView(-0.1354223, 51.5135085, zoom = 17) %>%
  addTiles(urlTemplate = "http://ameybarapatre.github.io/map/{z}/{x}/{y}.png",
           options = tileOptions(minZoom = 15, maxZoom = 18, tms = TRUE)) %>%
  addCircles(lng =deaths$V2,
             lat =deaths$V3,
             radius = deaths$V1, 
             color = "red", 
             fillColor = "red",popup=popups,  highlightOptions=highlightOptions(sendToBack=T))%>% addCircles(lng =pumps$V1,lat =pumps$V2,color = "blue", 
               fillColor = "blue" , popup=content, highlightOptions=highlightOptions(sendToBack=T))
                                              