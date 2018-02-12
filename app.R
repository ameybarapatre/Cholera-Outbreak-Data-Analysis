# sample R + Shiny example for CS 424 Spring 2018 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# This is a sample dashboard making use of the evl room temperature data and displaying
# it in a variery of ways to show off some of the different capabilities of R and Shiny
# and the Shiny Dashboard.

#libraries to include
require(plyr)
require(scales)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(reshape2)
library(plotly)

human_numbers <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){
    
    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "K")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"M")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"Bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "Tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}

#' Human versions of large currency numbers - extensible via smbl


human_num   <- function(x){human_numbers(x, smbl = '')} 

#setwd('c:/users/prakash/desktop/cs 424/P1')




naples_data <- read.table(file = "choleraDeaths.tsv", header = TRUE)
naples_data$newDate <- as.Date(naples_data$Date, "%d-%b-%Y")
naples_data$Death <- as.numeric(as.character(naples_data$Death))
naples_data$Attack <- as.numeric(as.character(naples_data$Attack))
naples_data$Cdeaths <- cumsum(naples_data$Death)
naples_data$Cattacks <-cumsum(naples_data$Attack)

naples_data_agesex <- read.table(file = "naplesCholeraAgeSexData.tsv", header = TRUE)
naples_data_agesex $male <- as.numeric(as.character(naples_data_agesex $male))
naples_data_agesex $female <- as.numeric(as.character(naples_data_agesex $female))

uk_data_agesex <- read.csv(file = "UKcensus1851.csv", sep = ",")
uk_data_agesex $male <- as.numeric(as.character(uk_data_agesex$male))
uk_data_agesex $female <- as.numeric(as.character(uk_data_agesex$female))



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Cholera Outbreak : Graph", tabName = "1854g"),
    menuItem("Cholera Outbreak :Table", tabName = "1854t"),
    menuItem("Cholera Outbreak : Map", tabName = "1854m"),
    menuItem("1851 Census :Graph", tabName = "1851b"),
    menuItem("1851 Census :Pie Chart", tabName = "1851p"),
    menuItem("1851 Census : Table", tabName = "1851t"),
    menuItem("1851 Census: Total Pie Chart", tabName = "1851p2"),
    menuItem("Naples: Table", tabName = "naplest"),
    menuItem("Naples : Graph", tabName = "naplesg"),
    menuItem("About", tabName = "about")
  )
)



ui <- dashboardPage(
    dashboardHeader(title = "Project 1"),
    sidebar,
    dashboardBody(
       tabItems(
    tabItem(tabName = "1854g",
    fluidRow(
        box( title = "Attacks and Deaths 1854 London Cholera Outbreak ", solidHeader = TRUE, status = "primary", width = 12,
            plotlyOutput(outputId = "hist2" , height= "50vh")
        )
    )), 
    tabItem(tabName = "1854t",fluidRow(
        box( title = "Table of Attacks and Deaths 1854 London Cholera Outbreak", solidHeader = TRUE, status = "primary", width = 12,
            dataTableOutput("tab1" , height = "20vh")
        )
    )),
     tabItem(tabName = "1854m",
    fluidRow(
        box(title = "Current Map", solidHeader = TRUE, status = "primary", width = 6,
            leafletOutput("jpeg" , height= "40vh")
        ), 
        box(title = "Snow's Map", solidHeader = TRUE, status = "primary", width = 6,
            leafletOutput("leaf" , height = "40vh")
        )
    )),
    tabItem(tabName = "naplesg",
     
   fluidRow(
        box( title = "Naples Age Sex Fatality Numbers Bar Graph", solidHeader = TRUE, status = "primary", width = 12,
            plotOutput("hist1" , height = "50vh")
        )))
     ,
     tabItem(tabName = "naplest",
        box(title = "Naples Age Sex Fatality Numbers Table", solidHeader = TRUE, status = "primary", width = 12,
            dataTableOutput("hist4")
        )),
tabItem(tabName = "1851b",
    
   fluidRow(
         box(title = "UK Census Numbers Bar graph", solidHeader = TRUE, status = "primary", width = 12,
            plotOutput("hist0" , height = "50vh")
        ))),
 tabItem(tabName = "1851p",
   fluidRow(
         box(title = "UK Census Numbers Pie Chart", solidHeader = TRUE, status = "primary", width = 12,
           plotOutput("hist3" , height = "45vh")
        )
    )),
  tabItem(tabName = "1851t",
   fluidRow(
         box(title = "UK Census Numbers Table", solidHeader = TRUE, status = "primary", width = 12,
            dataTableOutput("tab2")
        )
    )),
   tabItem(tabName = "1851p2",
   fluidRow(
         box(title = "UK Census Total Pie Chart", solidHeader = TRUE, status = "primary", width = 12,
            plotOutput("hist5" , height = "45vh")
        )
    )),tabItem(tabName="about" , "This project is created by Amey Barapatre using R Shiny , plotly , ggplot2 , lubridate & leaflet.The data is about the 1854 London cholera outbreak based on data created and compiled by Robin Wilson


    . I also use fatality numbers for men and women in different age groups from cholera in the same time period and the UK census data for 1851"))
))

server <- function(input, output) {

# increase the default font size
theme_set(theme_grey(base_size = 18)) 

# calculate the values one time and re-use them in multiple charts to speed things up



# bar graph UK
output$hist0 <- renderPlot({
   
    data_m <- melt(uk_data_agesex)
    ggplot(data_m, aes(x=age,fill=variable, y=value)) + geom_bar(stat="identity")  + labs(x="Age" ,y ="Fatalities" ,fill="Sex") +scale_y_continuous(labels = human_num)
        
        
})




# bar graph Naples
output$hist1 <- renderPlot({
   
    data_m <- melt(naples_data_agesex)
    ggplot(data_m, aes(x=age,fill=variable, y=value)) + geom_bar(stat="identity") + labs(x="Age" ,y ="Fatalities" ,fill="Sex")
    # + coord_polar("y", start=0) #+ facet_grid(~age)
  #      
})


# 4 lines death and attacks
output$hist2 <- renderPlotly({
  
    p <- ggplot(naples_data, aes(x=(newDate) , okay = newDate)) + labs(x="Time" ,y ="Deaths" ,color = "Colors") +
         geom_line(aes(y=Death , color="Death" ) ) + geom_line(aes(y=Attack , color="Attack"))+
    geom_line(aes(y=Cattacks , color="Total Attacks")) +
  geom_line(aes(y=Cdeaths , color="Total Deaths")) 
  p<-ggplotly(p ,tooltip = c("y","okay"))
    
 })


# pie chart UK
output$hist3 <- renderPlot({
   
    data_m <- melt(uk_data_agesex)
    ggplot(data_m, aes(x="",fill=age, y=value)) + geom_bar(stat="identity")   + coord_polar(theta= "y")  + facet_grid(~variable) + labs(x="Age" ,y ="Fatalities" ,fill="Sex") +scale_y_continuous(labels = human_num)
        
})
 
 # total pie chart UK
output$hist5 <- renderPlot({
   
    data_m <- uk_data_agesex
    df <- data.frame(
  group = c("male", "female"),
  value = c(sum(data_m$male) , sum(data_m$female))
    )

    ggplot(df, aes(x="",fill=group, y=value)) + geom_bar(stat="identity" , width =1)  + coord_polar("y", start=0) + labs(x="" ,y ="Fatalities" ,fill="Sex") +scale_y_continuous(labels = human_num)
        
})


#  Naples Age Sex table
output$hist4 <- DT::renderDataTable(
    DT::datatable({ 
        temperatures <- as.data.frame(naples_data_agesex)
  }, 
  options = list(scrollY="40vh",searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(2, 'desc'))
  ) , colnames = c("Age" = "age" , "Male" = "male" , "Female" = "female" ) 
    )
)


# Naples attack deaths table
output$tab1 <- DT::renderDataTable(
    DT::datatable({ 
    
    temperatures <- as.data.frame(naples_data[, names(naples_data)!="Date"])
  }, 
  options = list( scrollY = "30vh" ,searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(2, 'desc'))
  ) ,colnames  = c("Attacks" = "Attack" ,  "Deaths" = "Death" , "Date" = "newDate" , "Total Deaths" = "Cdeaths" ,"Total Attacks" = "Cattacks") 
    )
)

output$tab2 <- DT::renderDataTable(
    DT::datatable({ 
    temp_data <- uk_data_agesex
    temp_data$Total <- uk_data_agesex$male +uk_data_agesex$female
    temperatures <- as.data.frame(temp_data)
  }, 
  options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(2, 'desc'))
  ) , colnames = c("Age" ="age" , "Male" = "male" , "Female" = "female" , "Total" = "Total")
    )%>%
  formatCurrency('Male',currency = "", interval = 3, mark = "," ,digit= 0)%>%
  formatCurrency('Female',currency = "", interval = 3, mark = ",",digit= 0)%>%
  formatCurrency('Total',currency = "", interval = 3, mark = "," ,digit= 0)


)




# read in a jpeg map of the lab to show the room layout and plot some text on it
output$jpeg <- renderLeaflet({
  pumps <-read.csv("choleraPumpLocations.csv", header=FALSE)
content<-paste0("<strong>Pump</strong>")

deaths <-read.csv("choleraDeathLocations.csv",header=FALSE)
popups<-sapply(deaths$V1,as.character)
popups <-paste("<strong>Number Of Deaths:</strong>", popups)

leaflet() %>%
  setView(-0.1354223, 51.5135085, zoom = 17) %>%
  addTiles() %>%
  addCircles(lng =deaths$V2,
             lat =deaths$V3,
             radius = deaths$V1, 
             color = "red", 
             fillColor = "red",popup=popups,  highlightOptions=highlightOptions(sendToBack=T))%>% addCircles(lng =pumps$V1,lat =pumps$V2,color = "blue", 
               fillColor = "blue" , popup=content, highlightOptions=highlightOptions(sendToBack=T))
                                              
})

# add a leaflet map 
output$leaf <- renderLeaflet({
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
                                              
})


}

shinyApp(ui = ui, server = server)

