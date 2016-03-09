library(shiny)
library(leaflet)
library(ggplot2)
library(magrittr)
library(rgdal)
library(RColorBrewer)
load("diabetes_map.rdata")
load("brfss1112.RData")
load("pop_dem_graphs.rdata")

shinyUI(bootstrapPage(
  tabsetPanel("tabs",
              tabPanel("Home",
                       sidebarPanel(
                         h3("Diabeties Prevalence in Ohio's 88 Counties"),
                         p("This app was created to explore some of the demographic characteristics that may correlate with diabetes presence in the state of Ohio. As one can see from the accompanying animation, diabetes prevalence in all of Ohio's counties has been on the rise in the past nine years. Between 2004 and 2012, diabetes prevalence increased by an average of 2.4 percent throughout Ohio's 88 counties."),
                         p("Abdalah El-Barrad and Danielle Keeton-Olsen, two undergraduate student researchers for the Voinovich School of Leadership and Public Affairs under the direction of Dr. Anirudh Ruhil, have taken public data from various sources to create the visualizations in this app. The app uses data from the Ohio Family Health Survey in 2008, and the Behavioral Risk Factor Surveillance System in 2011 and 2012."),
                         p("Note: All of this data is supplied from survey responses, so some of the more specific details, such as the county-level racial and ethnicity maps, can get lost in the survey population sizes."),
                         p("Because diabetes - in particular, Type II Adult-Onset Diabetes - is often correlated with lifestyle traits, we hope this app may elucidate some of the factors that may contribute to a person's likelihood in having the illness, as well as explore county-level differences in Ohio's diabetic populations.")
                       ),
                       imageOutput("testgif")
                       ),
              tabPanel("Ohio Family Health Survey",
                       titlePanel("Percent of Respondents with Diabetes, OFHS 2008"),
                       sidebarPanel(
                         selectInput("choicesOFHS", "Demographic:", choices = c("See More Details"="diabetes", "Access to Healthcare" = "coverage", "Higher Education" = "education", "Minority"="race", "Sex"="sex", "Income Level"="income", "Age" = "age", "BMI Category" = "bmi")),#"Diabetic"="diabetes" 
                         p("Of the nearly 49,000 people who responded to the Ohio Family Health Survey in 2008 (and included their county of residence in their response), 7,260 have some form of diabetes. This map shows the distribution of respondents who said 'yes' when asked if they have diabetes in the 2008 OFHS survey. Based on this distribution, it appears that diabetes cases are most prevalent in southern Ohio and Appalachian counties. Click on a county to view the number of respondents, the percentage of people with diabetes, as well as the distribution of Type I and Type II diabetes among the diabetic population."),
                         p("The charts below each map show the distribution of demographic traits among diabetic and nondiabetic populations at the state level.")
                       ),
                       mainPanel(
                         fluidRow(
                           column(6, 
                                  leafletOutput("ofhs_left")),
                           column(6, 
                                  leafletOutput("ofhs_right"))
                         ),
                         fluidRow(
                           plotOutput("plotOFHS")
                         )
                       )
                       
                       
                       
              ), #end OFHS panel
              tabPanel("BRFSS 2011",
                       sidebarPanel(
                         selectInput("choices11", "Demographic:", choices = c("See More Details"="diabetes", "Access to Healthcare" = "coverage", "Higher Education" = "education", "Minority"="race", "Sex"="sex", "Income Level"="income", "Afford Doctor's Visit"="afford", "Exercise"="exercise", "General Health"="health")),
                         p("The map(s) displayed show the demographics for the diabetics only. Nondiabetics are not used to calculate this percent. The graph below shows the proportion of diabetics and nondiabetics for the entire state. Click on a county to view the percent of the population that falls into the designated category."), 
                         p("The charts below each map show the distribution of demographic traits among diabetic and nondiabetic populations at the state level.")
                       ),
                       titlePanel("Diabetic Demographics for 2011"),
                       mainPanel(
                         fluidRow(
                           column(6,
                                  leafletOutput("main11")
                           ),
                           column(6,
                                  
                                  leafletOutput("minus111")
                           )
                         ),
                         fluidRow(
                           plotOutput("plot11")
                         )
                       )
                       
              ),
              tabPanel("BRFSS 2012",
                       sidebarPanel(
                         selectInput("choices12", "Demographic:", choices = c("See More Details"="diabetes", "Access to Healthcare" = "coverage", "Higher Education" = "education", "Minority"="race", "Sex"="sex", "Income Level"="income", "Afford Doctor's Visit"="afford", "Exercise"="exercise", "General Health"="health")),
                         p("The map(s) displayed show the demographics for the diabetics only. Nondiabetics are not used to calculate this percent. The graph below shows the proportion of diabetics and nondiabetics for the entire state."), 
                         p("The charts below each map show the distribution of demographic traits among diabetic and nondiabetic populations at the state level.")
                       ),
                       titlePanel("Diabetic Demographics for 2012"),
                       mainPanel(
                         fluidRow(
                           column(6,
                                  leafletOutput("main12")
                           ),
                           column(6,
                                  
                                  leafletOutput("minus112")
                           )
                         ),
                         fluidRow(
                           plotOutput("plot12")
                         )
                       )
              )
  )
  
))