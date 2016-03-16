library(shiny)
library(leaflet)
library(ggplot2)
library(magrittr)
library(rgdal)
library(RColorBrewer)
load("diabetes_map.rdata")
load("brfss1112.RData")
load("pop_dem_graphs.rdata")


####################33
##################
######## Fix bins for OFHS income map.
######## Also, change the parameters for factors on maps as needed.
######## Also add the county name for ggcounty data.
#################
#####################

shinyServer(function(input, output, session){
  
  
  output$main11 <- renderLeaflet({
    if(input$choices11=="diabetes"){
      ohio$temp <- ohio$percentDiabetic11
      tempText = "Percent of People <br> Who Are Diabetic"
    }else if(input$choices11 == "afford"){
      ohio$temp <- ohio$percentNotAfford11
      tempText = "Percent of People <br> Who Couldn't <br> Afford Doctor's Visit"
    } else if(input$choices11=="coverage"){
      ohio$temp <- 100-ohio$percentCovered11
      tempText = "Percent of People<br>Who Do Not Have<br>Access to Healthcare"
    } else if(input$choices11=="education"){
      ohio$temp <- ohio$percentMoreThanHighSchool11
      tempText = "Percent of People <br> With at Least <br> Some College"
    } else if(input$choices11=="health"){
      ohio$temp <- ohio$percentUnhealthy11
      tempText = "Percent of People <br> Who Consider <br> Themselves Unhealthy"
    } else {
      ohio$temp <- ohio$`percent<15K11`
      tempText = "Percent of People <br> Who Make Less <br> Than $15K a Year"
    }
    
    countyPal <- colorBin("Blues", domain = ohio$temp, bins = 5)
    
    
    popupInfo <- paste0("<p><b>", ohio$County, "</b>",
                        "<br><em>Percent: </em>",
                        round(ohio$temp, digits = 0),"%</P>")
    
    leaflet(ohio) %>%
      addTiles() %>%
      addPolygons(stroke=F,
                  fillOpacity = .8,
                  smoothFactor=1,
                  color=~countyPal(temp),
                  popup=popupInfo) %>%
      addLegend("bottomright", pal=countyPal, values=~temp, title=tempText, opacity=1, labFormat = labelFormat(suffix="%"))
    
  }) # end of main11
  
  
  output$main12 <- renderLeaflet({
    
    if(input$choices12=="diabetes"){
      ohio$temp <- ohio$percentDiabetic12
      tempText = "Percent of People <br> Who Are Diabetic"
    }else if(input$choices12 == "afford"){
      ohio$temp <- ohio$percentNotAfford12
      tempText = "Percent of People <br> Who Couldn't <br> Afford Doctor's Visit"
    } else if(input$choices12=="coverage"){
      ohio$temp <- 100-ohio$percentCovered12
      tempText = "Percent of People<br>Who Do Not Have<br>Access to Healthcare"
    } else if(input$choices12=="education"){
      ohio$temp <- ohio$percentMoreThanHighSchool12
      tempText = "Percent of People <br> With at Least <br> Some College"
    } else if(input$choices12=="health"){
      ohio$temp <- ohio$percentUnhealthy12
      tempText = "Percent of People <br> Who Consider <br> Themselves Unhealthy"
    } else {
      ohio$temp <- ohio$`percent<15K12`
      tempText = "Percent of People <br> Who Make Less <br> Than $15K a Year"
    }
    
    countyPal <- colorBin("Greens", domain = ohio$temp, bins = 5)
    
    
    popupInfo <- paste0("<p><b>", ohio$County, "</b>",
                        "<br><em>Percent: </em>",
                        round(ohio$temp, digits = 0),"%</P>")
    
    leaflet(ohio) %>%
      addTiles() %>%
      addPolygons(stroke=F,
                  fillOpacity = .8,
                  smoothFactor=1,
                  color=~countyPal(temp),
                  popup=popupInfo) %>%
      addLegend("bottomright", pal=countyPal, values=~temp, title=tempText, opacity=1, labFormat = labelFormat(suffix="%"))
    
  }) # end of main12
  
  
  output$plot11 <- renderPlot({
    if(input$choices11=="diabetes"){
      return()
    }else if(input$choices11 == "afford"){
      return(affordVisit11graph)
    } else if(input$choices11=="coverage"){
      return(insured11graph)
    } else if(input$choices11=="education"){
      return(education11graph)
    } else if(input$choices11=="health"){
      return(health11graph)
    } else{
      return()
    }
  })
  
  
  output$plot12 <- renderPlot({
    if(input$choices12=="diabetes"){
      return()
    }else if(input$choices12 == "afford"){
      return(affordVisit12graph)
    } else if(input$choices12=="coverage"){
      return(insured12graph)
    } else if(input$choices12=="education"){
      return(education12graph)
    } else if(input$choices12=="health"){
      return(health12graph)
    } else{
      return()
    }
  })
  
  
  output$ofhs_left <- renderLeaflet({
    if(input$choicesOFHS!="income" & input$choicesOFHS!="diabetes"){
      if(input$choicesOFHS=="coverage"){
        ggcounty$temp <- ggcounty$PercentUninsured
        tempText = "Percent of People<br>Who Do Not Have<br>Access to Healthcare"
      }  else if(input$choicesOFHS=="education"){
        ggcounty$temp <- ggcounty$PercentAtLeastSomeCollege
        tempText = "Percent of People <br> With at Least <br> Some College"
      } else if(input$choicesOFHS=="race"){
        ggcounty$temp <- ggcounty$PercentMinority
        tempText = "Percent of Racial <br> Minorities"
      }  else if(input$choicesOFHS=="age"){
        ggcounty$temp <- ggcounty$Percent18to35
        tempText = "Percent of People Ages <br> 18 to 34"
      } else {
        ggcounty$temp <- ggcounty$PercentUnderNormal
        tempText = "Percent of People with <br> an Underweight or Normal BMI"
      }
      
      countyPal <- colorBin("Reds", domain = ggcounty$temp, bins = 5)
      
      
      popupInfo <- paste0("<p><b>", ggcounty$NAME, " County</b>",
                          "<br><em>Percent: </em>",
                          round(ggcounty$temp, digits = 0),"%</P>")
      
      leaflet(ggcounty) %>%
        addTiles() %>%
        addPolygons(stroke=F,
                    fillOpacity = .8,
                    smoothFactor=1,
                    color=~countyPal(temp),
                    popup=popupInfo) %>%
        addLegend("bottomright", pal=countyPal, values=~temp, title=tempText, opacity=1, labFormat = labelFormat(suffix="%"))
    } else if(input$choicesOFHS=="income"){
      
      countyPal <- colorBin("Reds", domain = ggcounty$WeightedIncome, c(0, 10000, 15000, 20000, 25000, 35000, 50000, 75000, 600000))
      
      
      popupInfo <- paste0("<p><b>", ggcounty$NAME, " County</b>",
                          "<br><em>Average Household Income: </em>$",
                          round(ggcounty$WeightedIncome, digits = 0),"</P>")
      
      leaflet(ggcounty) %>%
        addTiles() %>%
        addPolygons(stroke=F,
                    fillOpacity = .8,
                    smoothFactor=1,
                    color=~countyPal(WeightedIncome),
                    popup=popupInfo) %>%
        addLegend("bottomright", pal=countyPal, values=~WeightedIncome, title="Average Income", opacity=1, labFormat = labelFormat(prefix="$"))
    } else{
      
      
      countyPal <- colorBin("Reds", domain = ggcounty$percentDiabetes, bins = 5)
      
      
      popupInfo <- paste0("<p><b>", ggcounty$NAME, " County</b>",
                          "<br><em>Percent of Respondents with Diabetes: </em>",
                          round(ggcounty$percentDiabetes, digits = 0),"%",
                          "<br><em>Percent of Diabetics with Type I: </strong>",
                          round(ggcounty$`Percent Type I`, digits = 2), "%", 
                          "<br><em>Percent of Diabetics with Type II: </strong>",
                          round(ggcounty$`Percent Type II`, digits = 2), "%",
                          "<br><em>Percent of Diabetics with Gestational: </strong>",
                          round(ggcounty$`Percent Gestational`, digits = 2), "%</P>")
      
      leaflet(ggcounty) %>%
        addTiles() %>%
        addPolygons(stroke=F,
                    fillOpacity = .8,
                    smoothFactor=1,
                    color=~countyPal(percentDiabetes),
                    popup=popupInfo) %>%
        addLegend("bottomright", pal=countyPal, values=~percentDiabetes, title="Percent of People <br> Who Are Diabetic", opacity=1, labFormat = labelFormat(suffix="%"))
    }
    
  })
  
  
  
  output$plotOFHS <- renderPlot({
    
    if(input$choicesOFHS=="income"){
      return(ggplot(DIncome, aes(x= IncomeLevel, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               xlab("Household Income"))
    } else if(input$choicesOFHS=="coverage"){
      return(ggplot(DCoverage, aes(x= InsuranceType, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               xlab("Type of Insurance"))
    } else if(input$choicesOFHS=="diabetes"){
      return()
    }  else if(input$choicesOFHS=="education"){
      return(ggplot(DEducation, aes(x= LevelOfEducation, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               xlab("Highest Level of Education Achieved"))
    } else if(input$choicesOFHS=="race"){
      return(ggplot(DRace, aes(x= RaceEthnicity, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               xlab("Race or Ethnicity"))
    } else if(input$choicesOFHS=="age"){
      return(ggplot(DAge, aes(x=ageGroup, y=percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               xlab("Age"))
    } else if(input$choicesOFHS=="bmi"){
      return(ggplot(DBMI, aes(x=BMICategory, y=percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               xlab("BMI Category"))
    } else{
      return()
    }
  })
  
  
  output$testgif = renderImage({
    list(src="TieFighters.gif", width=650, height=700)
  }, deleteFile=F)
  
})