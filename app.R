#loading library
library(shiny)
library(DT)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(dplyr)
#library(raster)
#library(rsconnect)



#loading elevation shape file
countor<-readOGR(
  dsn="C:/Users/aquaya/Dropbox/AquayaLEARN 2018/Brian/Nakuru/www/Elevation Contours",
  layer="25m interval rough contours",encoding = 'UTF-8')


#loading sewer shape file
sewer<-readOGR(
  dsn="C:/Users/aquaya/Dropbox/AquayaLEARN 2018/Brian/Nakuru/www/Sewer map",
  layer="Sewer line WGS84",encoding = 'UTF-8')

 #loading LIAs shape file
 mp<-readOGR(
   dsn="C:/Users/aquaya/Dropbox/AquayaLEARN 2018/Brian/Nakuru/www/LIAs",
   layer="m1",encoding = 'UTF-8')
 
#loading  water treatment plants shape file
 treat<- readOGR(dsn="C:/Users/aquaya/Dropbox/AquayaLEARN 2018/Brian/Nakuru/www/treatment plants",
                 layer="Nk_Sewer_newcrs",encoding = 'UTF-8')
 
 #projecting the coordinates to default CRS
 treat1<-spTransform(treat, CRS("+proj=longlat +datum=WGS84"))


#Remove NAs from AreaType col
mp<-mp[!is.na(mp$AreaTyp),]


#loading nakuru data
nakuru<-read.csv("C:/Users/aquaya/Dropbox/AquayaLEARN 2018/Brian/Nakuru/www/naks/nak3.csv",encoding = 'UTF-8')
colnames(nakuru)[c(1,2,3,5,6,7,8,9,10,11)]=c("Location Name","Sublocation Name","Area Name",
                                           "Populationperkm2",
                           "Percentage Dwellings with PipedWateronPlot",
                           "Percentage Dwellings with WaterSourceOnPlot",
                           "Percentage Dwellings with FlushToilets",
                           "Percentage Dwellings with OtherImproved",
                           "Percentage Dwellings with Unimproved",
                           "Percentage Dwellings with OpenDefecation")



#slicing nakuru data for plotting options
nakuru1<-nakuru[,c("AreaType"),drop=F]
nakuru2<-nakuru[,c(5,7,8,9,10,11)]

#UI layout
ui<-dashboardPage(
  dashboardHeader(title = "NAKURU DATA",titleWidth = 275),
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      menuItem("DATA EXPLORER",tabName = "data",icon = icon("th")),
      menuItem("PLOT EXPLORER",tabName = "plot",icon =icon("bar-chart-o")),
      menuItem("INTERACTIVE MAPPING",tabName = "map",icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        h3("INTERACTING WITH DATA"),
        
        fluidRow(
          
          box(title = "FILTERING",status = "primary",solidHeader = TRUE,
              checkboxGroupInput(inputId = "popdensity", 
                                 label = " Population Per Km2:",
                                 choices = list(
                                   "< 15,000"=1, 
                                   "15,001 - 30,000"=2 , 
                                   ">30,000"=3
                                 ), 
                                 selected =list(1,2,3)
                                 ),
              
              selectInput(inputId = "area", 
                          label = " Area Type:",
                          choices = c(
                            "All",
                            unique(as.character(nakuru$AreaType))
                          ) 
              ),
              
              width = 4),
          
          
          box(title = "OUTPUT",status = "primary",solidHeader = TRUE,
              
              #datatableoutput
              DT::dataTableOutput("table"),
              width = 8
              
          ))),
      
      tabItem(
        tabName = "plot",
        h3("PLOT INTERACTION"),
        fluidRow(
          box(
            title = "AXIS VALUES",status = "primary",solidHeader = TRUE,
            radioButtons(inputId = "status",
                         label = "SELECT X VALUE:",
                         choices = colnames(nakuru1)
                         
            ),
            radioButtons(inputId = "status2",
                         label = "SELECT Y VALUE:",
                         choices = colnames(nakuru2)
                         
                         
            ),width=4
          ),
          box(
            title = "PLOT OUTPUT",status = "primary",solidHeader = TRUE,
            plotOutput("stat",click = "plot_click"),
            verbatimTextOutput("info"),
            width = 8
          ))
         
      ),
      tabItem(
        tabName = "map",
        h3("INTERACTIVE MAP"),
        fluidRow(
          box(
            title = "MAP DISPLAY",status = "primary",solidHeader = TRUE,
            leafletOutput("leaf",height = 600),width = 12
            
          ),
          
          absolutePanel( fixed = TRUE,
                         draggable = TRUE, top = 180, left = "auto", right = 10,
                         width = 330, height = "auto",style="opacity:0.8;background:#ffffff;",
                         
                         h2("MAP EXPLORER"),
                         
                         selectInput(inputId = "pop",
                         label = " POPULATION:",
                         choices = list(
                           "All"=1,
                           "Population Per Km2"=4
                          #"< 15,000"=2
                         # "15,001 - 30,000"=3 ,
                         # ">30,001"=1
                         )
                         ),
                         #Specification of range within an interval 
                         # sliderInput(inputId = "pop", 
                         #             label = "Population Per km2:",
                         #             min = 1, max = 155000,
                         #             value = c(1,15000)),
                         
                         selectInput(inputId = "area1", 
                                     label = " AREA TYPE:",
                                     choices = c(
                                       "All",
                                       unique(as.character(mp$AreaTyp))
                                     ) 
                         ),
                         selectInput(
                           inputId = "pw",
                           label = "WATER TYPE:",
                           choices = c(
                             "All"=1,
                             "Piped Water On Plot"=2,
                             "Water source On Plot"=3
                           )
                         ),
                         
                         selectInput(
                           inputId = "san",
                           label = "SANITATION TYPE:",
                           choices = c(
                             "All"=1,
                             "Flush Toilets"=2,
                             "Other Improved"=3,
                             "UnImproved"=4,
                             "Open Defecation"=5
                           )
                         ),
                          checkboxGroupInput(
                            inputId = "treat",
                            label = "LAYOUT:",
                            choices=c(
                              "Low Income Areas"=1,
                              "Water Treatment Plant"=2,
                              "Sewer Line"=3,
                              "Elevation"=4
                            ),selected = 1
                          )
                         
                         
                         
          )
        )
        
        
        
      )
    )
  )
)

#server function

server<-function(input,output,session){
  
    #reactive function for datatable
    dt<-reactive({
    df<-nakuru
    

#area type function
    if(input$area!="All"){

      df<-df[df$AreaType==input$area,]

    }
    
#population function    
    str(df)
    df1<-df %>% filter(Populationperkm2<=15000)
    df2<-df %>% filter(Populationperkm2>15000&Populationperkm2<=30000)
    df3<-df %>% filter(Populationperkm2>30000)
    temp<-data.frame()

    if("1" %in% input$popdensity){
      temp<-rbind(temp,df1)
    }

    if("2" %in% input$popdensity){
      temp<-rbind(temp,df2)
    }
    if("3" %in% input$popdensity){
      temp<-rbind(temp,df3)
    }
    temp
    
    })
    output$table<-DT::renderDataTable({
    DT::datatable(dt(),options = list(scrollX=TRUE))

    
  })

  
  #plot explorer 
  output$stat<-renderPlot({
        x<-nakuru[,c(input$status,input$status2)]
    plot(x,col = "#75AADB", pch = 19,main=paste0(input$status2,"  vs ",input$status))
    
    })
  
  #average lias info(%)
  output$info <- (renderText({
    paste0("MEDIAN=", round(as.numeric(input$plot_click$y,0)),
           "\n% AREA TYPE OF DWELLINGS:\nPlanned:68%\nUnplanned:18%\nMixed Area:14%",
           "\n% WATER ACCESS TYPE OF DWELLINGS:\nPiped Water On Plot:41%\nWater Source On Plot:43%",
           "\n% SANITATION TYPE OF DWELLINGS:\nFlush Toilets:19%\nUnimproved:46%\nOther Improved:35%\nOpen Defecation:0.3%"
           )
    
    
  }))
  
  
  #INTERACTIVE WEB MAPPING
  
  
  
  
  #reactive function for sanitation type
   fts<-reactive({
     dm<-mp
     if(input$san==2){
       dm[dm$FlshTlt<=25,]
     }
     if(input$san==3)
     {
       dm[dm$OthrImp<=50,]
     }
      if(input$san==4)
      {
         dm[dm$Unmprvd<=50,]
      }
        if(input$san==5)
              {
               dm[dm$OpnDfct<=50,]
              }
      
      return(dm)
    })
  
  
  
  #reactive function for water access type
  pwp<-reactive({
    dm<-mp
    
        if("2" %in% input$pw){
      dm[dm$PpdWtrP<=25,]
    }

    if("3" %in% input$pw){
      dm[dm$WtrSrOP<=25,]
    }

    dm
  })
  
  #reactive function for population per km2 
  ppd<-reactive({
    dt<-mp
    if(input$pop==1){
      dt[dt$PpDnsty<=15000,]
    } else if(input$pop==3){
      dt[dt$PpDnsty>15000&dt$PpDnsty<=30000,]
    } else if(input$pop==4){
      dt[dt$PpDnsty>30000,]
    }
    return(dt)
  })
  
  #reactive function for area type
  fdata<-reactive({
    data<-mp
    if(input$area1!="All"){
      data<-subset(data,AreaTyp %in% input$area1)
      #data<-data[data$AreaType==input$area,]
       
    }
    
    
   return(data)
  })
  
 #Base map(default)
  
  output$leaf<-renderLeaflet({
    
    
    
    leaflet(mp) %>%
      
      #Initializing the map
     # setView(lng=36.092245, lat=-00.292115,zoom=15)%>%
      
      #default map
      #Add default OpenStreetMap map tiles
      addTiles()%>%
      
       # addProviderTiles("Esri.NatGeoWorldMap",group = "default")%>%  
        #addProviderTiles("CartoDB.Positron",group = "custom")%>%
      
      #nakuru lias polygons
      addPolygons(
        data = mp,
        fillColor = "blue",
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                        "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty
                       )
        
      ) 
    
   
    
    
  })
  
  #observe function for area type
  observe({
    #color mapping function for area type
    pal<-colorFactor(rainbow(7),mp$AreaTyp)
    
  leafletProxy("leaf",data=fdata()) %>%
     
      clearMarkers() %>%
      clearControls() %>%
     clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        #data=fdata(),
        fillColor = ~pal(AreaTyp),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                        "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty
        )
        
      )%>%
  addLegend(title = "AreaType", position = "topleft",
           pal = pal, values = ~AreaTyp, opacity = 1)
  })
    
  #observe function for population per km2
  observe({
    
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma",mp$PpDnsty, 15, pretty = TRUE)
    
    leafletProxy("leaf",data=ppd()) %>%

      # clearMarkers() %>%
       clearControls() %>%
       clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal1(PpDnsty),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                        "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty
                       
        )
        
      )%>%
      addLegend(title = "Population Per km2", position = "topleft",
                pal = pal1, values = ~PpDnsty, opacity = 1)

  })
  
  
  
  
  #observe function for water access type
  observe({
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma", mp$PpdWtrP, 5, pretty = TRUE)
    pal2<-colorBin("plasma", mp$WtrSrOP, 5, pretty = TRUE)
    
    md<-leafletProxy("leaf",data=pwp()) %>% clearControls() %>% clearShapes()
      
      # clearMarkers() %>%
    if("1" %in% input$pw){
      md %>%
        
       
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%  
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay lias map
        addPolygons(
        
          fillColor = "blue",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
          )
          
        ) 
    }
    
    #piped water on plot if function
    if("2" %in% input$pw){
      md%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal1(PpdWtrP),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty
                       
        )
        
      )%>%
      addLegend(title = "Piped Water On Plot(%):", position = "topleft",
                pal = pal1, values = ~PpdWtrP, opacity = 1)
    }
    
    #water source on plot if function
    if("3" %in% input$pw){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal2(WtrSrOP),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
                         
          )
          
        )%>%
        addLegend(title = "WaterSource On Plot(%):", position = "topleft",
                  pal = pal2, values = ~WtrSrOP, opacity = 1)
    }
  })
  
  #observe function for sanitation type
  observe({
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma", mp$FlshTlt, 5, pretty = TRUE)
    pal2<-colorBin("plasma", mp$OthrImp, 5, pretty = TRUE)
    pal3 <- colorBin("plasma", mp$Unmprvd, 5, pretty = TRUE)
    pal4<-colorBin("plasma", mp$OpnDfct, 5, pretty = TRUE)
    
    #leafletproxy function
    md<-leafletProxy("leaf",data=fts()) %>% clearControls() %>% clearShapes()
    
    # clearMarkers() %>%
    #nakuru lias if function
    if("1" %in% input$san){
      md %>%
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%  
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay lias map
        addPolygons(
          
          fillColor = "blue",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
          )
          
        ) 
    }
    
    #flash toilets if function
    if("2" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          #data=fdata(),
          fillColor = ~pal1(FlshTlt),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
                         
          )
          
        )%>%
        addLegend(title = "Flush Toilets (%):", position = "topleft",
                  pal = pal1, values = ~FlshTlt, opacity = 1)
    }
    
    #otherimproved if function
    if("3" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal2(OthrImp),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
                         
          )
          
        )%>%
        addLegend(title = "Other Improved (%):", position = "topleft",
                  pal = pal2, values = ~OthrImp, opacity = 1)
    }
    
    #unimproved if function
    if("4" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          #data=fdata(),
          fillColor = ~pal3(Unmprvd),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
                         
          )
          
        )%>%
        addLegend(title = "UnImproved (%):", position = "topleft",
                  pal = pal3, values = ~Unmprvd, opacity = 1)
    }
    
    #open defecation if function
    if("5" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal4(OpnDfct),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
                         
          )
          
        )%>%
        addLegend(title = "Open Defecation (%):", position = "topleft",
                  pal = pal4, values = ~OpnDfct, opacity = 1)
    }
  })
  
  #observe function for water access type
  observe({
    
    #leafletproxy function
    md<-leafletProxy("leaf") %>% clearControls() %>% clearShapes()
    
    
    #nakuru lias if function
    if("1" %in% input$treat){
      md %>%
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%  
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay lias map
        addPolygons(
          data = mp,
          fillColor = "blue",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
          )
          
        ) 
    }
    
    #water treatment plants ovelay map function
    if("2" %in% input$treat){
      md%>%
        addPolygons(
          data = treat1,
          color="green",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~NAME,
          popup = ~paste("<strong>TYPE:</strong>",TYPE,
                         "<br>",
                         "<strong>USAGE:</strong>",USAGE,"m3",
                         "<br>",
                         "<strong>CAPACITY:</strong>",CAPACITY,"m3"


          )
        )
    }
    
    #sewer lines overlay map function
    if("3" %in% input$treat){
      md%>%
        addPolylines(
          weight = 2, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          data=sewer,
          color = "red",
          highlightOptions = highlightOptions(
            weight = 2,
            color = "yellow",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~OWNER,
          popup = ~paste("<strong>OWNER:</strong>",OWNER_2,
                         "<br>",
                         "<strong>YEAR CONSTRUCTED:</strong>",YEAR_CONST,
                         "<br>",
                         "<strong>MATERIAL:</strong>",MATERIAL,
                         "<br>",
                         "<strong>PIPE SIZE:</strong>",PIPE_SIZE,
                         "<br>",
                         "<strong>LENGTH:</strong>",LENGTH_1


          )
        ) 
    
    }
    
    #elevation contours overlay map function
    if("4" %in% input$treat){
      md%>%
        addPolylines(
          weight = 2, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          data=countor,
          color = "brown",
                highlightOptions = highlightOptions(
                  weight = 2,
                  color = "green",
                  fillOpacity = 0.7,
                  bringToFront = TRUE
                ),
                label =~Interval,
                popup = ~paste("<strong>ELEVATION:</strong>",elevation,"m",
                               "<br>",
                               "<strong>INTERVAL:</strong>",Interval
                               )


        ) 
      
    }
        
  })
  
}


shinyApp(ui,server)