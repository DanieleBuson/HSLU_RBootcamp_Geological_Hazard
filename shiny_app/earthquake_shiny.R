earthquake <- read.csv("C:/Users/buson/OneDrive/Desktop/r_project/project_folder/data/usgs_main.csv")

earthquake <- earthquake[!is.na(earthquake$mag),]

earthquake$time <- as.POSIXct(earthquake$time, tz = "", format = "%Y-%m-%d")

days <- format(earthquake$time, "%d")
months <- format(earthquake$time, "%m")


df_geo_vis <- data.frame(months, days, 
                 latitude = earthquake$latitude, 
                 longitude = earthquake$longitude, 
                 depth = earthquake$depth, 
                 mag = earthquake$mag, 
                 rms = earthquake$rms, 
                 place = earthquake$place, 
                 type = earthquake$type,
                 int_mag= as.integer(earthquake$mag))

################################################################################
########################Visualization of Geographical data######################
################################################################################ 

# Please check if you have installed all the libraries needed. (on the readMe.txt file)

# install.packages("shinythemes")

library("ggsn")
library("ggplot2")
library("sf")
library(plotly)
library("rnaturalearth")
library("rnaturalearthdata")

# Getting the shape file of the world 
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
library(shinythemes)

################################################################################
################################ R.Shiny #######################################
################################################################################

# The goal is to have a table and a visualisation with interactive tools 
# so that exploring data can be done in a easier way. 

################################################################################
################################################################################
################################################################################

library(shiny)
col.tmp <- c( "-1" = "purple","0" = "violet", "1" = "blue",
              "2"="skyblue","3"="green","4"="yellow",
              "5"="orange","6"="darkorange","7"="red")
pnt.size <- c( "-1" = .1,"0" = .1, "1" = .1, "2"=.1,"3"=.1,
               "4"=.1,"5"=.2,"6"=.3,"7"=.3)
# Define the Shiny UI
ui <- fluidPage(theme = shinytheme("slate"),

  
  navbarPage("Earthquakes Visualization",
             
             tabPanel("Preview",
                      
                      sidebarLayout(
                        
                        sidebarPanel(width= 2,
                                     helpText("R-Bootcamp course project at HSLU")),
                        
                        mainPanel( titlePanel("Preview"),
                                   fluidRow(
                                     column(10, h4("Geographical Data Visualisation")),
                                     column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("The idea is to first create a table with interactive elements to make data exploration 
                                     simpler. Following that, the visualisation of the events by magnitude and type would be available. 
                                     Obtaining a geographical dataset consisting of world country borders, selection of the country itself
                                      is accomplished and executed in a user-friendly manner.
                                                                                                                       ")),
                                     column(10, h4("Data overview")),
                                     column(10, tableOutput("tablep")),
                                     column(10, h4("Data granularity")),
                                     column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("The granularity of an earthquake dataset in 2022 with columns such as months, days, latitude, longitude, depth, mag, rms, place, and type can significantly affect the accuracy and reliability of any analysis or visualisation performed on it.

                                              The dataset can be made granular by including the exact date and time of each earthquake, allowing for detailed analysis of temporal patterns such as the frequency of earthquakes throughout the year and their possible correlation with other variables.
                                              
                                                 Furthermore, including each earthquake's latitude and longitude provides a granular view of its geographical distribution, allowing for spatial analysis to identify hotspots and potentially vulnerable areas. Besides that, including information on the depth, magnitude, and type of each earthquake can aid in identifying potential causal factors that contribute to seismic activity.")),
                                     column(10, h4("Feature Engineering")),
                                     column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("Some feature engineering was required, such as the aggregation of magnitude data and the addition of only integer values to the dataset. This is due to  magnitude levels ranging from -1 to 1, which include surface seismic events  created by controlled explosions related to underground mining.")),
                                     column(10, h4("Type of Seismic Events")),
                                     column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("Seismic events like earthquakes, quarry blasts, ice quakes, explosions, and chemical explosions differ in a number of ways.")),
                                     column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("Earthquakes: Earthquakes are a type of natural phenomenon that happen when tectonic plates under the crust of the earth move. They frequently cause ground displacement, vibration, and shaking. An earthquake can range in size and intensity from minor tremors to catastrophic quakes.")),
                                     column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("Quarry Blast: When explosives are used to dislodge rocks and minerals during mining or construction operations, a quarry blast results. They result in an abrupt and powerful release of energy that can produce seismic waves, which cause shaking and ground movement.")),
                                     column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("Ice Quake: An ice quake is a type of seismic occurrence that takes place when permafrost, a frozen ground, experiences rapid temperature changes that cause the ice to split and crack. Similar to an earthquake, the resulting seismic waves can cause trembling and noise.")),
                                      column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("Explosion: An explosion is a sudden, violent release of energy that can be brought on by a number of things, including chemical reactions, gas leaks, or even fireworks. They have the ability to produce seismic waves, which can move the ground and cause shaking.")),
                                      column(10,style = "text-align: justify; margin-left: 20px; margin-right: 20px;", p("Chemical Explosion: A chemical reaction can result in the sudden and violent release of energy that results in a chemical explosion. Since chemical reactions take place instead of physical events like gas leaks or fireworks, they differ from regular explosions in this regard.
                                                                                                                         They have the ability to produce seismic waves, which can shake and move the ground.")),
                                     
                                   )
                                   )) ),
             tabPanel("Table",
                      
                      sidebarLayout(
                        
                        sidebarPanel(width= 2,
                                     sliderInput("slider2", "Magnitude:", min = -1, max = 7,width = 200 , value = 1),
                                     selectInput("select2", "Type:",width = 200 , choices = unique(df_geo_vis$type)),
                                     helpText("Help: Select the magnitude and type of earthquakes to visualize on the table")),
                        
                        mainPanel( titlePanel("Earthquakes table"), 
                                   
                                   fluidRow(
                                     #column(6, img(src = "my_photo.jpg")),
                                     column(6, p("Time span, Magnitude are aggregated for this very project."))
                                   ),tableOutput("table")
                                   )) ),
             
             tabPanel("Map",
                      
                      sidebarLayout(
                        
                        sidebarPanel(width= 2,
                                     sliderInput("slider", "Magnitude:", min = -1, max = 7,width = 200 , value = 1),
                                     selectInput("select", "Type:",width = 200 , choices = unique(df_geo_vis$type)),
                                     selectInput("selectName", "Country:",width = 200 , choices = sort(unique(world$name))),
                                     helpText("Help: Select the magnitude and type of earthquakes to visualize on the map.")),
                        
                        mainPanel( titlePanel("Earthquakes by magnitude and type 
                                              "), plotlyOutput("map",width = "125%", height = "800px")))),
             tabPanel("About us",
                      
                      fluidRow(
                      column(6,div(style="text-align: center;",imageOutput("Daniele",width=1000,height=300)),
                      h3("Daniele Buson"), p("Data Scientist and Mathematical Engineer"),
                      p(a("daniele.buson@stud.hslu.ch",href="daniele.buson@stud.hslu.ch"))),
                      
                      column(6,div(style="text-align: center;",imageOutput("Morty",width=1100,height=300)),
                      h3("Morteza Kiani Haftlang"), p("Data Scientist and Electrical Engineer"),
                      p(a("morteza.kianihaftlang@stud.hslu.ch", href = "morteza.kianihaftlang@stud.hslu.ch")))
                               )
                      
                      
                      )) 
             
 ) 

# Define the Shiny server
server <- function(input, output) {
  
  output$map <- renderPlotly({
    
    
    col.pnt <- col.tmp[as.character(input$slider)]
    size.pnt <- pnt.size[as.character(input$slider)] 
    
    filtered.df <- df_geo_vis[df_geo_vis$int_mag==input$slider ,]
    filtered.df2 <- filtered.df[filtered.df$type==input$select,]
    country <- world[world$name ==input$selectName,]
    
    p <-ggplot() + geom_sf(data = world)+ 
      geom_sf(data=country, fill = "brown")+
      
      geom_point(data=filtered.df2, 
                 mapping = aes(x=longitude,
                               y=latitude),
                 color = col.pnt,size=size.pnt*10)+
      
      #scale_colour_gradientn(colors=rev(rainbow(9)))
      scale_color_identity()
    ggplotly(p) })
  
  output$table <- renderTable({
    filtered.df <- df_geo_vis[df_geo_vis$int_mag==input$slider2 ,]
    filtered.df2 <- filtered.df[filtered.df$type==input$select2,]
    })
  
  output$tablep <- renderTable({
    head(df_geo_vis,7)
  })
  
  output$Daniele <- renderImage({ list(src = "C:/Users/buson/OneDrive/Desktop/r_project/project_folder/images/Daniele.jpg",width="25%")}, deleteFile = F)
  output$Morty <- renderImage({ list(src = "C:/Users/buson/OneDrive/Desktop/r_project/project_folder/images/Morty.jpg",width="25%")}, deleteFile = F)
  
}

# Run the Shiny app
shinyApp(ui, server)


