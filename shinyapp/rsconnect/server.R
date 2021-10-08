#runApp("/Users/zanzver/Documents/BCU/Year2/DataVisualisation/Assignemnt/Code/shinyapp")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("scales")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("lubridate")
# install.packages("plyr")
# install.packages("tigris")
# install.packages("leaflet")
# install.packages("sp")
# install.packages("ggmap")
# install.packages("maptools")
# install.packages("broom")
# install.packages("httr")
# install.packages("readr")
# install.packages("gganimate")
# install.packages("gapminder")
# install.packages("packcircles")
# install.packages("wordcloud2")
# install.packages("wordcloud")

library(shiny) 
library(shinydashboard)
library(ggplot2)
library(plotly)
require(scales)
library(dplyr)
library(reshape2)
library(lubridate)
library(plyr)
library(tigris)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(readr)
library(gganimate)
library(gapminder)
library(packcircles)
library(wordcloud2) 
library(wordcloud)

#set theme for world cloud
theme_set(theme_bw()) 

#define data path
maindata<- read.csv(file.path("~/NYPD_Arrests_Data__Historic_-2.csv"))


#main server function
server <- function(input, output){
  #plot 1-> presentation of arests over years. User can select date from when to when to view graph
  output$numberOfArrestsOverYears_plot  <- renderPlot({
    #convert from main data to mydata (for saftey)
    mydata <-maindata
    
    #declare inmput type 
    if(input$from == "2006"){
      dsFrom <- "2006-01-01"
    }
    if(input$to == "2006"){
      dsTo <- "2006-01-01"
    }
    if(input$from == "2007"){
      dsFrom <- "2007-01-01"
    }
    if(input$to == "2007"){
      dsTo <- "2007-01-01"
    }
    if(input$from == "2008"){
      dsFrom <- "2008-01-01"
    }
    if(input$to == "2008"){
      dsTo <- "2008-01-01"
    }
    if(input$from == "2009"){
      dsFrom <- "2009-01-01"
    }
    if(input$to == "2009"){
      dsTo <- "2009-01-01"
    }
    if(input$from == "2010"){
      dsFrom <- "2010-01-01"
    }
    if(input$to == "2010"){
      dsTo <- "2010-01-01"
    }
    if(input$from == "2011"){
      dsFrom <- "2011-01-01"
    }
    if(input$to == "2011"){
      dsTo <- "2011-01-01"
    }
    if(input$from == "2012"){
      dsFrom <- "2012-01-01"
    }
    if(input$to == "2012"){
      dsTo <- "2012-01-01"
    }
    if(input$from == "2013"){
      dsFrom <- "2013-01-01"
    }
    if(input$to == "2013"){
      dsTo <- "2013-01-01"
    }
    if(input$from == "2014"){
      dsFrom <- "2014-01-01"
    }
    if(input$to == "2014"){
      dsTo <- "2014-01-01"
    }
    if(input$from == "2015"){
      dsFrom <- "2015-01-01"
    }
    if(input$to == "2015"){
      dsTo <- "2015-01-01"
    }
    if(input$from == "2016"){
      dsFrom <- "2016-01-01"
    }
    if(input$to == "2016"){
      dsTo <- "2016-01-01"
    }
    if(input$from == "2017"){
      dsFrom <- "2017-01-01"
    }
    if(input$to == "2017"){
      dsTo <- "2017-01-01"
    }
    if(input$from == "2018"){
      dsFrom <- "2018-01-01"
    }
    if(input$to == "2018"){
      dsTo <- "2018-01-01"
    }
    
    #convert arrest date, to M/D/Y format 
    mydata$ARREST_DATE <-as.Date(mydata$ARREST_DATE,"%m/%d/%Y")
    
    #filter years that users has selected (from and to)
    mydata<-mydata%>%
      filter(mydata$ARREST_DATE > dsFrom & mydata$ARREST_DATE < dsTo)
    
    #count occurences from date
    dcounter <- as.Date(mydata$ARREST_DATE, format="%m/%d/%Y")
    
    #convert to table, and cut month from dcounter
    tab <- table(cut(dcounter, 'month'))
    
    #set the final count as date (with specific format) and Freqency 
    finalcount<- data.frame(Date=format(as.Date(names(tab)), '%Y-%m-%d'),
                            Frequency=as.vector(tab))

    #create a plot, x is year and y is frequency 
    ggplot(finalcount, aes(x =  as.Date(Date), y = Frequency)) +
      geom_line(color="darkgreen") +
      scale_x_date(date_breaks = "1 years",
                   date_labels = "%Y")+ xlab("Date") + ylab("Frequency")
  })
  
  #plot 2-> show user what is the top arrest case. They can select occurences 5 as min and 8 as max
  output$whatArePeopleArrestedFor_plot  <- renderPlot({
    #convert from main data to mydata (for saftey)
    mydata<-maindata
    
    #get count from table
    data <- as.data.frame(table(mydata$PD_DESC))

    #rename columns
    colnames(data) <- c("group", "value")
    #reorder value in data from highest values to lower
    data<-data[order(-data$value),]

    #declare inmput type 
    if(input$whatArePeopleArrestedForfeatures == "5"){
      data<-head(data,5)
    }
    if(input$whatArePeopleArrestedForfeatures == "6"){
      data<-head(data,6)
    }
    if(input$whatArePeopleArrestedForfeatures == "7"){
      data<-head(data,7)
    }
    if(input$whatArePeopleArrestedForfeatures == "8"){
      data<-head(data,8)
    }
    
    #set circle foundementals 
    packing <- circleProgressiveLayout(data$value, sizetype='area')
    #combine data and packing
    data <- cbind(data, packing)
    #declare number of points in avector
    dat.gg <- circleLayoutVertices(packing, npoints=50)

    #create a plot of circles
    ggplot() +
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +

      geom_text(data = data, aes(x, y, size=value, label = group)) +
      scale_size_continuous(range = c(1,3)) +

      theme_void() +
      theme(legend.position="none") +
      coord_equal()

  })

  #plot 3-> present graph that is based on diversity. They can change between sex, age and race
  #please not that this graphs are as gifs, it takes a min or two to load each graph
  output$diversity_plot <- renderImage({
       #convert from main data to mydata (for saftey)
       mydata<-maindata
       
       #convert a data format of arest date so we only have year at the end
       mydata$ARREST_DATE<-format(as.POSIXct(as.Date(mydata$ARREST_DATE, format = "%m/%d/%Y")),format='%Y')
      
        #create new datasets that might be used in the future, based on users input
       countsSex <- ddply(mydata, .(mydata$ARREST_DATE, mydata$PERP_SEX), nrow)
       countsRace <- ddply(mydata, .(mydata$ARREST_DATE, mydata$PERP_RACE), nrow)
       countsAge <- ddply(mydata, .(mydata$ARREST_DATE, mydata$AGE_GROUP), nrow)

       #remove outliers 
       countsSex<-subset(countsSex, countsSex$V1>20)
       countsRace<-subset(countsRace, countsRace$V1>20)
       countsAge<-subset(countsAge, countsAge$V1>20)

       #declare inmput type 
       if(input$diversityfeatures == "countsSex"){
         swdata<-countsSex
       }
       if(input$diversityfeatures == "countsRace"){
         swdata<-countsRace
       }
       if(input$diversityfeatures == "countsAge"){
         swdata<-countsAge
       }

       #rename second column to "r3", this is due to input type being on second column 
       colnames(swdata)[2] <- "r3"
       #factor the column 
       swdata$r3<-factor(swdata$r3)
    
       #filter data based on year
       data06<-dplyr::filter(swdata, grepl('2006', swdata$`mydata$ARREST_DATE`))
       data07<-dplyr::filter(swdata, grepl('2007', swdata$`mydata$ARREST_DATE`))
       data08<-dplyr::filter(swdata, grepl('2008', swdata$`mydata$ARREST_DATE`))
       data09<-dplyr::filter(swdata, grepl('2009', swdata$`mydata$ARREST_DATE`))
       data10<-dplyr::filter(swdata, grepl('2010', swdata$`mydata$ARREST_DATE`))
       data11<-dplyr::filter(swdata, grepl('2011', swdata$`mydata$ARREST_DATE`))
       data12<-dplyr::filter(swdata, grepl('2012', swdata$`mydata$ARREST_DATE`))
       data13<-dplyr::filter(swdata, grepl('2013', swdata$`mydata$ARREST_DATE`))
       data14<-dplyr::filter(swdata, grepl('2014', swdata$`mydata$ARREST_DATE`))
       data15<-dplyr::filter(swdata, grepl('2015', swdata$`mydata$ARREST_DATE`))
       data16<-dplyr::filter(swdata, grepl('2016', swdata$`mydata$ARREST_DATE`))
       data17<-dplyr::filter(swdata, grepl('2017', swdata$`mydata$ARREST_DATE`))
       data18<-dplyr::filter(swdata, grepl('2018', swdata$`mydata$ARREST_DATE`))

       #rename columns 
       colnames(data06)[2] <- "r2"
       colnames(data07)[2] <- "r2"
       colnames(data08)[2] <- "r2"
       colnames(data09)[2] <- "r2"
       colnames(data10)[2] <- "r2"
       colnames(data11)[2] <- "r2"
       colnames(data12)[2] <- "r2"
       colnames(data13)[2] <- "r2"
       colnames(data14)[2] <- "r2"
       colnames(data15)[2] <- "r2"
       colnames(data16)[2] <- "r2"
       colnames(data17)[2] <- "r2"
       colnames(data18)[2] <- "r2"

       #make 2 basic states and concatenate them:
       d06 <- data.frame(group= data06$`mydata$ARREST_DATE`, values = data06$V1, frame = data06$r2)
       d07 <- data.frame(group= data07$`mydata$ARREST_DATE`, values = data07$V1, frame = data07$r2)
       d08 <- data.frame(group= data08$`mydata$ARREST_DATE`, values = data08$V1, frame = data08$r2)
       d09 <- data.frame(group= data09$`mydata$ARREST_DATE`, values = data09$V1, frame = data09$r2)
       d10 <- data.frame(group= data10$`mydata$ARREST_DATE`, values = data10$V1, frame = data10$r2)
       d11 <- data.frame(group= data11$`mydata$ARREST_DATE`, values = data11$V1, frame = data11$r2)
       d12 <- data.frame(group= data12$`mydata$ARREST_DATE`, values = data12$V1, frame = data12$r2)
       d13 <- data.frame(group= data13$`mydata$ARREST_DATE`, values = data13$V1, frame = data13$r2)
       d14 <- data.frame(group= data14$`mydata$ARREST_DATE`, values = data14$V1, frame = data14$r2)
       d15 <- data.frame(group= data15$`mydata$ARREST_DATE`, values = data15$V1, frame = data15$r2)
       d16 <- data.frame(group= data16$`mydata$ARREST_DATE`, values = data16$V1, frame = data16$r2)
       d17 <- data.frame(group= data17$`mydata$ARREST_DATE`, values = data17$V1, frame = data17$r2)
       d18 <- data.frame(group= data18$`mydata$ARREST_DATE`, values = data18$V1, frame = data18$r2)

       #join dataframes
       data <- rbind(d06, d07,d08,d09,d10,d11,d12,d13,d14,d15,d16,d17,d18)

       #create output file
       outfile <- tempfile(fileext='.gif')

       #now make the animation
       p = ggplot(data, aes(x=group, y=values, fill=group)) +
         geom_bar(stat='identity') +
         theme_bw() +
         labs(title = 'Year: {frame_time}') +
         #gganimate specific bits:
         transition_states(
           frame,
           transition_length = 2,
           state_length = 1
         ) +
         labs(title = '{closest_state}')+
         ease_aes('sine-in-out')
  
       #save file
       anim_save("outfile.gif", animate(p,nframes=200))

       #return a list containing the filename
       list(src = "outfile.gif",
            contentType = 'image/gif'
       )}, deleteFile = TRUE)

  #plot 4-> present graph that presents months over time
  output$timeFfTheYear_plot  <- renderPlot({
    #convert from main data to mydata (for saftey)
    test<-maindata
    
    #convert to date format as M/D/Y
    temp<-as.Date(test$ARREST_DATE, "%m/%d/%Y")
    #get only months
    x<-format(temp, format="%m")
    
    #convert to table
    b <- data.frame(table(x))
    #create data
    data <- data.frame(
      x=month.abb[b$x],
      y=b$Freq 
    )
    
    #plot a graph
    ggplot(data, aes(x = x, y = y)) +
      geom_segment( aes(x=reorder(x, -y), xend=x, y=0, yend=y), color="skyblue") +
      geom_point( color="blue", size=4, alpha=0.4) +
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )+
      scale_y_continuous(labels = comma)+
      xlab("Month") +
      ylab("Frequency")
  })
  
  # #plot 5-> present map. User can select specific arrest types and they can zoom in and out of the map
  output$mymap <- renderLeaflet({
    #convert from main data to mydata (for saftey)
    mydata <-maindata
    
    #filter data based on top arrest cases, but divide 2 from number of occurrences due to efficiency  
    d1<-head(dplyr::filter(mydata, grepl('MARIJUANA, POSSESSION 4 & 5', mydata$PD_DESC)),20000) #422358
    d2<-head(dplyr::filter(mydata, grepl('ASSAULT 3', mydata$PD_DESC)),15000) #376001
    d3<-head(dplyr::filter(mydata, grepl('THEFT OF SERVICES, UNCLASSIFIED', mydata$PD_DESC)),15000) #303359
    d4<-head(dplyr::filter(mydata, grepl('CONTROLLED SUBSTANCE, POSSESSION 7', mydata$PD_DESC)),10000)#278083
    d5<-head(dplyr::filter(mydata, grepl('NY STATE LAWS,UNCLASSIFIED VIOLATION', mydata$PD_DESC)),10000)#225678
    d6<-head(dplyr::filter(mydata, grepl('LARCENY,PETIT FROM OPEN AREAS,UNCLASSIFIED', mydata$PD_DESC)),5000)#188347
    
    #initialize the leaflet map:
    leaflet() %>%
      #set the view of New York
      setView(-74.005974, 40.712776, zoom =11) %>%
      
      addProviderTiles("Esri.WorldImagery")%>%
      #mark groups
      addCircleMarkers(data=d1, lng=~d1$Longitude , lat=~d1$Latitude, radius=6 , color="black",
                       fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="MARIJUANA, POSSESSION 4 & 5 (red)") %>%
      addCircleMarkers(data=d2, lng=~d2$Longitude , lat=~d2$Latitude, radius=6 , color="black",
                       fillColor="blue", stroke = TRUE, fillOpacity = 0.8, group="ASSAULT 3 (blue)") %>%
      addCircleMarkers(data=d3, lng=~d3$Longitude , lat=~d3$Latitude, radius=6 , color="black",
                       fillColor="blueviolet", stroke = TRUE, fillOpacity = 0.8, group="THEFT OF SERVICES, UNCLASSIFIED (violet)") %>%
      addCircleMarkers(data=d4, lng=~d4$Longitude , lat=~d4$Latitude, radius=6 , color="black",
                       fillColor="darkgoldenrod", stroke = TRUE, fillOpacity = 0.8, group="CONTROLLED SUBSTANCE, POSSESSION 7 (gold)") %>%
      addCircleMarkers(data=d5, lng=~d5$Longitude , lat=~d5$Latitude, radius=6 , color="black",
                       fillColor="aquamarine", stroke = TRUE, fillOpacity = 0.8, group="NY STATE LAWS,UNCLASSIFIED VIOLATION (aquamarine)") %>%
      addCircleMarkers(data=d6, lng=~d6$Longitude , lat=~d6$Latitude, radius=6 , color="black",
                       fillColor="deeppink", stroke = TRUE, fillOpacity = 0.8, group="LARCENY,PETIT FROM OPEN AREAS,UNCLASSIFIED (pink)") %>%
      
      #add the control widget, it is closed on start up
      addLayersControl(overlayGroups = c("MARIJUANA, POSSESSION 4 & 5 (red)",
                                         "ASSAULT 3 (blue)",
                                         "THEFT OF SERVICES, UNCLASSIFIED (violet)",
                                         "CONTROLLED SUBSTANCE, POSSESSION 7 (gold)",
                                         "NY STATE LAWS,UNCLASSIFIED VIOLATION (aquamarine)",
                                         "LARCENY,PETIT FROM OPEN AREAS,UNCLASSIFIED (pink)"),
                       options = layersControlOptions(collapsed = TRUE))
  })

  #plot 6-> create a world cloud. User can change if they want to see top law codes or arrest precinct
  output$wordcloud2 <- renderWordcloud2({
    #convert from main data to mydata (for saftey)
    mydata<-maindata
    
    #get specific dataset based on users input
    if(input$jurisdictionResponsibleFeatures == "LAW_CODE"){
       data <- as.data.frame(table(mydata$LAW_CODE))
       }
    if(input$jurisdictionResponsibleFeatures == "ARREST_PRECINCT"){
       data <- as.data.frame(table(mydata$ARREST_PRECINCT))
     }
    
    #display worldcloud
    wordcloud2(data, size=5)
  })
  
}
