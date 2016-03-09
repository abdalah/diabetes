library(shiny)
library(leaflet)
library(ggplot2)
library(magrittr)
library(rgdal)
library(RColorBrewer)
load("diabetes_map.rdata")
load("brfss1112.RData")
load("pop_dem_graphs.rdata")

shinyServer(function(input, output, session){
  
  
  output$main11 <- renderLeaflet({
    if(input$choices11=="diabetes"){
      ohio$temp <- ohio$percentDiabetic11
      tempText = "Percent of People <br> Who Are Diabetic"
    }else if(input$choices11 == "afford"){
      ohio$temp <- ohio$percentNotAfford11
      tempText = "Percent of People <br> Who Couldn't <br> Afford Doctor's Visit"
    } else if(input$choices11=="coverage"){
      ohio$temp <- ohio$percentCovered11
      tempText = "Percent of People <br> Who have Access <br> to Healthcare"
    } else if(input$choices11=="education"){
      ohio$temp <- ohio$percentMoreThanHighSchool11
      tempText = "Percent of People <br> With at Least <br> Some College"
    } else if(input$choices11=="exercise"){
      ohio$temp <- ohio$percentExercise11
      tempText = "Percent of People <br> Who Exercise"
    } else if(input$choices11=="health"){
      ohio$temp <- ohio$percentUnhealthy11
      tempText = "Percent of People <br> Who Consider <br> Themselves Unhealthy"
    } else if(input$choices11=="income"){
      ohio$temp <- ohio$`percent<15K11`
      tempText = "Percent of People <br> Who Make Less <br> Than $15K a Year"
    } else if(input$choices11=="race"){
      ohio$temp <- ohio$percentMinority11
      tempText = "Percent of Racial <br> Minorities"
    } else{
      ohio$temp <- ohio$percentFemale11
      tempText = "Percent of People <br> Are Female"
    }
    
    countyPal <- colorBin("Blues", domain = ohio$temp, bins = 5)
    
    
    popupInfo <- paste0("<p><b>County: ", ohio$County, "</b>",
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
  
  
  output$minus111 <- renderLeaflet({
    if(input$choices11!="diabetes"){
      if(input$choices11 == "afford"){
        ohio$tempMinus <- 100-ohio$percentNotAfford11
        tempMinusText = "Percent of People <br> Who Could <br> Afford Doctor's Visit"
      } else if(input$choices11=="coverage"){
        ohio$tempMinus <- 100-ohio$percentCovered11
        tempMinusText = "Percent of People <br> Who Do Not <br> Have Access to Healthcare"
      } else if(input$choices11=="education"){
        ohio$tempMinus <- 100-ohio$percentMoreThanHighSchool11
        tempMinusText = "Percent of People <br> With No Higher <br> Education"
      } else if(input$choices11=="exercise"){
        ohio$tempMinus <- 100-ohio$percentExercise11
        tempMinusText = "Percent of People <br> Who Do Not <br> Exercise"
      } else if(input$choices11=="health"){
        ohio$tempMinus <- 100-ohio$percentUnhealthy11
        tempMinusText = "Percent of People <br> Who Consider <br> Themselves Healthy"
      } else if(input$choices11=="income"){
        ohio$tempMinus <- 100-ohio$`percent<15K11`
        tempMinusText = "Percent of People <br> Who Make More <br> Than $15K a Year"
      } else if(input$choices11=="race"){
        ohio$tempMinus <- 100-ohio$percentMinority11
        tempMinusText = "Percent of Whites"
      } else{
        ohio$tempMinus <- 100-ohio$percentFemale11
        tempMinusText = "Percent of People <br> Who Are Male"
      }
      
      countyPal <- colorBin("Blues", domain = ohio$tempMinus, bins = 5)
      
      
      popupInfo <- paste0("<p><b>County: ", ohio$County, "</b>",
                          "<br><em>Percent: </em>",
                          round(ohio$tempMinus, digits = 0),"%</P>")
      
      leaflet(ohio) %>%
        addTiles() %>%
        addPolygons(stroke=F,
                    fillOpacity = .8,
                    smoothFactor=1,
                    color=~countyPal(tempMinus),
                    popup=popupInfo) %>%
        addLegend("bottomright", pal=countyPal, values=~tempMinus, title=tempMinusText, opacity=1, labFormat = labelFormat(suffix="%"))
    }
    
  }) # end of minus111
  
  
  output$main12 <- renderLeaflet({
    
    if(input$choices12=="diabetes"){
      ohio$temp <- ohio$percentDiabetic12
      tempText = "Percent of People <br> Who Are Diabetic"
    }else if(input$choices12 == "afford"){
      ohio$temp <- ohio$percentNotAfford12
      tempText = "Percent of People <br> Who Couldn't <br> Afford Doctor's Visit"
    } else if(input$choices12=="coverage"){
      ohio$temp <- ohio$percentCovered12
      tempText = "Percent of People <br> Who have Access <br> to Healthcare"
    } else if(input$choices12=="education"){
      ohio$temp <- ohio$percentMoreThanHighSchool12
      tempText = "Percent of People <br> With at Least <br> Some College"
    } else if(input$choices12=="exercise"){
      ohio$temp <- ohio$percentExercise12
      tempText = "Percent of People <br> Who Exercise"
    } else if(input$choices12=="health"){
      ohio$temp <- ohio$percentUnhealthy12
      tempText = "Percent of People <br> Who Consider <br> Themselves Unhealthy"
    } else if(input$choices12=="income"){
      ohio$temp <- ohio$`percent<15K12`
      tempText = "Percent of People <br> Who Make Less <br> Than $15K a Year"
    } else if(input$choices12=="race"){
      ohio$temp <- ohio$percentMinority12
      tempText = "Percent of Racial <br> Minorities"
    } else{
      ohio$temp <- ohio$percentFemale12
      tempText = "Percent of People <br> Are Female"
    }
    
    countyPal <- colorBin("Blues", domain = ohio$temp, bins = 5)
    
    
    popupInfo <- paste0("<p><b>County: ", ohio$County, "</b>",
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
  
  
  output$minus112 <- renderLeaflet({
    if(input$choices12!="diabetes"){
      if(input$choices12 == "afford"){
        ohio$tempMinus <- 100-ohio$percentNotAfford12
        tempMinusText = "Percent of People <br> Who Could <br> Afford Doctor's Visit"
      } else if(input$choices12=="coverage"){
        ohio$tempMinus <- 100-ohio$percentCovered12
        tempMinusText = "Percent of People <br> Who Have <br> Access to Healthcare"
      } else if(input$choices12=="education"){
        ohio$tempMinus <- 100-ohio$percentMoreThanHighSchool12
        tempMinusText = "Percent of People <br> With No Higher <br> Education"
      } else if(input$choices12=="exercise"){
        ohio$tempMinus <- 100-ohio$percentExercise12
        tempMinusText = "Percent of People <br> Who Do Not <br> Exercise"
      } else if(input$choices12=="health"){
        ohio$tempMinus <- 100-ohio$percentUnhealthy12
        tempMinusText = "Percent of People <br> Who Consider <br> Themselves Healthy"
      } else if(input$choices12=="income"){
        ohio$tempMinus <- 100-ohio$`percent<15K12`
        tempMinusText = "Percent of People <br> Who Make More <br> Than $15K a Year"
      } else if(input$choices12=="race"){
        ohio$tempMinus <- 100-ohio$percentMinority12
        tempMinusText = "Percent of Whites"
      } else{
        ohio$tempMinus <- 100-ohio$percentFemale12
        tempMinusText = "Percent of People <br> Who Are Male"
      }
      
      countyPal <- colorBin("Blues", domain = ohio$tempMinus, bins = 5)
      
      
      popupInfo <- paste0("<p><b>County: ", ohio$County, "</b>",
                          "<br><em>Percent: </em>",
                          round(ohio$tempMinus, digits = 0),"%</P>")
      
      leaflet(ohio) %>%
        addTiles() %>%
        addPolygons(stroke=F,
                    fillOpacity = .8,
                    smoothFactor=1,
                    color=~countyPal(tempMinus),
                    popup=popupInfo) %>%
        addLegend("bottomright", pal=countyPal, values=~tempMinus, title=tempMinusText, opacity=1, labFormat = labelFormat(suffix="%"))
    }
    
  }) # end of minus112
  
  
  output$plot11 <- renderPlot({
    if(input$choices11=="diabetes"){
      return(Diabetic11graph)
    }else if(input$choices11 == "afford"){
      return(affordVisit11graph)
    } else if(input$choices11=="coverage"){
      return(insured11graph)
    } else if(input$choices11=="education"){
      return(education11graph)
    } else if(input$choices11=="exercise"){
      return(exercise11graph)
    } else if(input$choices11=="health"){
      return(health11graph)
    } else if(input$choices11=="income"){
      return(income11graph)
    } else if(input$choices11=="race"){
      return(race11graph)
    } else{
      return(sex11graph)
    }
  })
  
  
  output$plot12 <- renderPlot({
    if(input$choices12=="diabetes"){
      return(Diabetic12graph)
    }else if(input$choices12 == "afford"){
      return(affordVisit12graph)
    } else if(input$choices12=="coverage"){
      return(insured12graph)
    } else if(input$choices12=="education"){
      return(education12graph)
    } else if(input$choices12=="exercise"){
      return(exercise12graph)
    } else if(input$choices12=="health"){
      return(health12graph)
    } else if(input$choices12=="income"){
      return(income12graph)
    } else if(input$choices12=="race"){
      return(race12graph)
    } else{
      return(sex12graph)
    }
  })
  
  
  output$ofhs_left <- renderLeaflet({
    if(input$choicesOFHS!="income"){
      if(input$choicesOFHS=="coverage"){
        ggcounty$temp <- ggcounty$PercentInsured
        tempText = "Percent of People <br> Who Do Not <br> Have Access to Healthcare"
      } else if(input$choicesOFHS=="diabetes"){
        ggcounty$temp <- ggcounty$percentDiabetes
        tempText = "Percent of People <br> Who Are Diabetic"
      }  else if(input$choicesOFHS=="education"){
        ggcounty$temp <- ggcounty$PercentAtLeastSomeCollege
        tempText = "Percent of People <br> With at Least <br> Some College"
      } else if(input$choicesOFHS=="race"){
        ggcounty$temp <- ggcounty$PercentMinority
        tempText = "Percent of Racial <br> Minorities"
      } else if(input$choicesOFHS=="sex"){
        ggcounty$temp <- ggcounty$PercentFemale
        tempText = "Percent of People <br> Who Are Female"
      } else if(input$choicesOFHS=="age"){
        ggcounty$temp <- ggcounty$Percent18to35
        tempText = "Percent of People Ages <br> 18 to 34"
      } else {
        ggcounty$temp <- ggcounty$PercentUnderNormal
        tempText = "Percent of People with <br> an Underweight or Normal BMI"
      }
      
      countyPal <- colorBin("Blues", domain = ggcounty$temp, bins = 5)
      
      
      popupInfo <- paste0("<p><b>County: ", ggcounty$County, "</b>",
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
    } else{
      
      countyPal <- colorBin("Blues", domain = ggcounty$WeightedIncome, bins = 5)
      
      
      popupInfo <- paste0("<p><b>County: ", ggcounty$County, "</b>",
                          "<br><em>Average Household Income: </em>",
                          round(ggcounty$WeightedIncome, digits = 0),"%</P>")
      
      leaflet(ggcounty) %>%
        addTiles() %>%
        addPolygons(stroke=F,
                    fillOpacity = .8,
                    smoothFactor=1,
                    color=~countyPal(WeightedIncome),
                    popup=popupInfo) %>%
        addLegend("bottomright", pal=countyPal, values=~WeightedIncome, title="Average Income", opacity=1, labFormat = labelFormat(prefix="$"))
    }
    
  })
  
  
  output$ofhs_right <- renderLeaflet({
    if(input$choicesOFHS!="diabetes" & input$choicesOFHS!="income"){
      if(input$choicesOFHS=="coverage"){
        ggcounty$tempMinus <- 100-ggcounty$PercentInsured
        tempMinusText = "Percent of People <br> Who Do Not <br> Have Access to Healthcare"
      } else if(input$choicesOFHS=="education"){
        ggcounty$tempMinus <- 100-ggcounty$PercentAtLeastSomeCollege
        tempMinusText = "Percent of People <br> With No Higher <br> Education"
      } else if(input$choicesOFHS=="race"){
        ggcounty$tempMinus <- 100-ggcounty$PercentMinority
        tempMinusText = "Percent of Whites"
      } else if(input$choicesOFHS=="sex"){
        ggcounty$tempMinus <- 100-ggcounty$PercentFemale
        tempMinusText = "Percent of People <br> Who Are Male"
      }else if(input$choicesOFHS=="exercise"){
        ggcounty$tempMinus <- 100-ggcounty$percentExercise12
        tempMinusText = "Percent of People <br> Who Do Not <br> Exercise"
      } else if(input$choicesOFHS=="age"){
        ggcounty$tempMinus <- 100-ggcounty$Percent18to35
        tempMinusText = "Percent of People Ages <br> above 34"
      } else {
        ggcounty$tempMinus <- 100-ggcounty$PercentUnderNormal
        tempMinusText = "Percent of People with <br> an Overwieght BMI"
      }
      
      countyPal <- colorBin("Blues", domain = ggcounty$tempMinus, bins = 5)
      
      
      popupInfo <- paste0("<p><b>County: ", ggcounty$County, "</b>",
                          "<br><em>Percent: </em>",
                          round(ggcounty$tempMinus, digits = 0),"%</P>")
      
      leaflet(ggcounty) %>%
        addTiles() %>%
        addPolygons(stroke=F,
                    fillOpacity = .8,
                    smoothFactor=1,
                    color=~countyPal(tempMinus),
                    popup=popupInfo) %>%
        addLegend("bottomright", pal=countyPal, values=~tempMinus, title=tempMinusText, opacity=1, labFormat = labelFormat(suffix="%"))
    }
    
  })
  
  
  
  output$plotOFHS <- renderPlot({
    
    if(input$choicesOFHS=="income"){
      return(ggplot(DIncome, aes(x= IncomeLevel, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip())
    } else if(input$choicesOFHS=="coverage"){
      return(ggplot(DCoverage, aes(x= InsuranceType, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip())
    } else if(input$choicesOFHS=="diabetes"){
      return(
        ggplot(DDiabetic, aes(x=Dummy, y=percent, group=diabetic, fill=diabetic)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          xlab(""))
    }  else if(input$choicesOFHS=="education"){
      return(ggplot(DEducation, aes(x= LevelOfEducation, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip())
    } else if(input$choicesOFHS=="race"){
      return(ggplot(DRace, aes(x= RaceEthnicity, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip())
    } else if(input$choicesOFHS=="sex"){
      return(ggplot(DSex, aes(x= Gender, y = percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip())
    } else if(input$choicesOFHS=="age"){
      return(ggplot(DAge, aes(x=ageGroup, y=percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip())
    } else{
      return(ggplot(DBMI, aes(x=BMICategory, y=percent, group = diabetic, fill = diabetic)) +
               geom_bar(stat = "identity") +
               coord_flip())
    }
  })
  
  
  output$testgif = renderImage({
    list(src="TieFighters.gif", width=650, height=700)
  }, deleteFile=F)
  
})