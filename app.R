library(leaflet)
library(leaflet.extras)
library(readxl)
library(dplyr)
library(shiny)
library(shinydashboard)


data2018 <- read_excel("egrid2018_data_v2.xlsx", na = "0")
data2010 <- read_excel("eGRID2010_Data.xls", na = "0")
data2000 <- read_excel("eGRID2000_plant.xls", na = "0")
#d[is.na(d)] <- 0

data2018[is.na(data2018)] <- 0
data2010[is.na(data2010)] <- 0
data2000[is.na(data2000)] <- 0

data2018$TNPR <- data2018$TNPR * 100
data2018$TRPR <- data2018$TRPR * 100

data2018$TYPE <- as.factor(data2018$TYPE)

data2010$YEAR <- as.numeric(data2010$YEAR)


ILdata <- subset(data2018,STATE == "IL")
ILdata$TYPE <- as.factor(ILdata$TYPE)



pal =  colorFactor(palette= c("saddlebrown","black","red4","darkorange","deepskyblue4","purple1","midnightblue",
                              "peru","gold3","seagreen"),
                   domain = data2018$TYPE)



################# START UI #################
ui <- fluidPage(
  
  navbarPage("Project 2: Raw Power",
             
             
################# ILLINOIS PANEL #################
             
             tabPanel("Illinois Map", 
                      fluidRow(
                        column(2,
                               checkboxInput("checkAll", "All",TRUE),
                               checkboxInput("checkCoal", "Coal",FALSE),
                               checkboxInput("checkGeo", "Geothermal",FALSE),
                               checkboxInput("checkHydro", "Hydro",FALSE),
                               checkboxInput("checkGas", "Natural Gas",FALSE),
                               checkboxInput("checkNC", "Nuclear",FALSE),
                               checkboxInput("checkOil", "Oil",FALSE),
                               checkboxInput("checkSol", "Solar",FALSE),
                               checkboxInput("checkWind", "Wind",FALSE),
                               checkboxInput("checkBio",  "Biomass",FALSE),      
                               checkboxInput("checkOth",  "Other",FALSE),
                               checkboxInput("checkRN", "Renewable",FALSE),
                               checkboxInput("checkNRN", "Non Renewable", FALSE),
                               actionButton("reset", "Reset Checkboxes")
                               
                        ),
                        
                        column(10,
                               box(title = "IL Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                                   leafletOutput("leaf", height = 800,width = 800)
                               )
                        )
                        
                      )
             ),#end of first panel
             
#################  END ILLINOIS PANEL #################
             
          
#################  START COMPARE 2 MAPS PANEL  #################

             
             tabPanel("Compare 2 Maps", 
                      fluidRow(
                        column(6,
                               fluidRow(
                                 column(width = 2,
                                        checkboxInput("checkAll2", "All",TRUE),
                                        checkboxInput("checkCoal2", "Coal",FALSE),
                                        checkboxInput("checkGeo2", "Geothermal",FALSE),
                                        checkboxInput("checkHydro2", "Hydro",FALSE),
                                        checkboxInput("checkGas2", "Natural Gas",FALSE),
                                        checkboxInput("checkNC2", "Nuclear",FALSE),
                                        checkboxInput("checkOil2", "Oil",FALSE),
                                        checkboxInput("checkSol2", "Solar",FALSE),
                                        checkboxInput("checkWind2", "Wind",FALSE),
                                        checkboxInput("checkBio2",  "Biomass",FALSE),      
                                        checkboxInput("checkOth2",  "Other",FALSE),
                                        checkboxInput("checkRN2", "Renewable",FALSE),
                                        checkboxInput("checkNRN2", "Non Renewable", FALSE),
                                        checkboxInput("checkLink", "Link Checkboxes", FALSE),
                                        selectInput("selectState1", "First Map state:",
                                                    c("US(All States)" = "All States","Alabama" = "AL","Alaska" = "AK","Arizona" = "AZ","Arkansas" = "AR","California" = "CA",
                                                      "Colorado" = "CO","Connecticut" = "CT","Delaware" = "DE","Florida" = "FL","Georgia" = "GA",
                                                      "Hawaii" = "HI","Idaho" = "ID","Illinois" = "IL","Indiana" = "IN","Iowa" = "IA",
                                                      "Kansas" = "KS","Kentucky" = "KY","Louisiana" = "LA", "Maine" = "ME","Maryland" = "MD",
                                                      "Massachusetts" = "MA","Michigan" = "MI","Minnesota" = "MN","Mississippi" = "MS","Missouri" = "MO",
                                                      "Montana" = "MT","Nebraska" = "NE","Nevada" = "NV","New Hampshire" = "NH","New Jersey" = "NJ",
                                                      "New Mexico" = "NM","New York" = "NY","North Carolina" = "NC","North Dakota" = "ND","Ohio" = "OH",
                                                      "Oklahoma" = "OK","Oregon" = "OR","Pennsylvania" = "PA","Rhode Island" = "RI","South Carolina" = "SC",
                                                      "South Dakota" = "SD","Tennessee" = "TN","Texas" = "TX","Utah" = "UT","Vermont" = "VT",
                                                      "Virginia" = "VA","Washington" = "WA","West Virginia" = "WV","Wisconsin" = "WI","Wyoming" = "WY",
                                                      "Washington DC" = "DC"),
                                                    selected="IL"
                                        ),
                                        selectInput("selectYear1", "First Map year:",
                                                    c("2000" = 2000,
                                                      "2010" = 2010,
                                                      "2018" = 2018),
                                                    selected=2000),
                                        selectInput("selectStyle", "Select Map type:",
                                                    c("Default" = "default",
                                                      "Orange Highways" = "orange_high",
                                                      "Blue Highways" ="blue_high"),
                                                    selected="default"),
                                        actionButton("reset1", "Reset Checkboxes")
                                 ),
                                 column(width = 4,
                                        box(title = textOutput("title1"), solidHeader = TRUE, status = "primary", width = 12,
                                            leafletOutput("leaf1", height = 700,width = 700)
                                        )
                                        
                                 )
                               )),
                        column(6,
                               fluidRow(
                                 column(width = 2,
                                        checkboxInput("checkAll3", "All",TRUE),
                                        checkboxInput("checkCoal3", "Coal",FALSE),
                                        checkboxInput("checkGeo3", "Geothermal",FALSE),
                                        checkboxInput("checkHydro3", "Hydro",FALSE),
                                        checkboxInput("checkGas3", "Natural Gas",FALSE),
                                        checkboxInput("checkNC3", "Nuclear",FALSE),
                                        checkboxInput("checkOil3", "Oil",FALSE),
                                        checkboxInput("checkSol3", "Solar",FALSE),
                                        checkboxInput("checkWind3", "Wind",FALSE),
                                        checkboxInput("checkBio3",  "Biomass",FALSE),      
                                        checkboxInput("checkOth3",  "Other",FALSE),
                                        checkboxInput("checkRN3", "Renewable",FALSE),
                                        checkboxInput("checkNRN3", "Non Renewable", FALSE),
                                        checkboxInput("checkLink2", "Link Checkboxes", FALSE),
                                        selectInput("stateSelect2", "Second Map state:",
                                                    c("US(All States)" = "All States","Alabama" = "AL","Alaska" = "AK","Arizona" = "AZ","Arkansas" = "AR","California" = "CA",
                                                      "Colorado" = "CO","Connecticut" = "CT","Delaware" = "DE","Florida" = "FL","Georgia" = "GA",
                                                      "Hawaii" = "HI","Idaho" = "ID","Illinois" = "IL","Indiana" = "IN","Iowa" = "IA",
                                                      "Kansas" = "KS","Kentucky" = "KY","Louisiana" = "LA", "Maine" = "ME","Maryland" = "MD",
                                                      "Massachusetts" = "MA","Michigan" = "MI","Minnesota" = "MN","Mississippi" = "MS","Missouri" = "MO",
                                                      "Montana" = "MT","Nebraska" = "NE","Nevada" = "NV","New Hampshire" = "NH","New Jersey" = "NJ",
                                                      "New Mexico" = "NM","New York" = "NY","North Carolina" = "NC","North Dakota" = "ND","Ohio" = "OH",
                                                      "Oklahoma" = "OK","Oregon" = "OR","Pennsylvania" = "PA","Rhode Island" = "RI","South Carolina" = "SC",
                                                      "South Dakota" = "SD","Tennessee" = "TN","Texas" = "TX","Utah" = "UT","Vermont" = "VT",
                                                      "Virginia" = "VA","Washington" = "WA","West Virginia" = "WV","Wisconsin" = "WI","Wyoming" = "WY",
                                                      "Washington DC" = "DC"),
                                                    selected="IL"
                                        ),
                                        selectInput("yearSelect2", "Second Map year:",
                                                    c("2000" = 2000,
                                                      "2010" = 2010,
                                                      "2018" = 2018),
                                                    selected=2018)
                                 ),
                                 column(width = 4,
                                        box(title = textOutput("title2"), solidHeader = TRUE, status = "primary", width = 12,
                                            leafletOutput("leaf2", height = 700,width = 700)
                                        )
                                        
                                 )
                               ))
                      )),#end compare panel
             
#################  END COMPARE 2 MAPS PANEL  #################
             
             
             
#################  START US MAP PANEL  #################
             
             tabPanel("US Map",
                      fluidRow(
                        column(12,
                               fluidRow(
                                 column(width = 2,
                                        checkboxInput("checkAll4", "All",TRUE),
                                        checkboxInput("checkCoal4", "Coal",FALSE),
                                        checkboxInput("checkGeo4", "Geothermal",FALSE),
                                        checkboxInput("checkHydro4", "Hydro",FALSE),
                                        checkboxInput("checkGas4", "Natural Gas",FALSE),
                                        checkboxInput("checkNC4", "Nuclear",FALSE),
                                        checkboxInput("checkOil4", "Oil",FALSE),
                                        checkboxInput("checkSol4", "Solar",FALSE),
                                        checkboxInput("checkWind4", "Wind",FALSE),
                                        checkboxInput("checkBio4",  "Biomass",FALSE),      
                                        checkboxInput("checkOth4",  "Other",FALSE),
                                        checkboxInput("checkRN4", "Renewable",FALSE),
                                        checkboxInput("checkNRN4", "Non Renewable", FALSE),
                                        selectInput("selectState3", "Select state:",
                                                    c("US(All States)" = "All States","Alabama" = "AL","Alaska" = "AK","Arizona" = "AZ","Arkansas" = "AR","California" = "CA",
                                                      "Colorado" = "CO","Connecticut" = "CT","Delaware" = "DE","Florida" = "FL","Georgia" = "GA",
                                                      "Hawaii" = "HI","Idaho" = "ID","Illinois" = "IL","Indiana" = "IN","Iowa" = "IA",
                                                      "Kansas" = "KS","Kentucky" = "KY","Louisiana" = "LA", "Maine" = "ME","Maryland" = "MD",
                                                      "Massachusetts" = "MA","Michigan" = "MI","Minnesota" = "MN","Mississippi" = "MS","Missouri" = "MO",
                                                      "Montana" = "MT","Nebraska" = "NE","Nevada" = "NV","New Hampshire" = "NH","New Jersey" = "NJ",
                                                      "New Mexico" = "NM","New York" = "NY","North Carolina" = "NC","North Dakota" = "ND","Ohio" = "OH",
                                                      "Oklahoma" = "OK","Oregon" = "OR","Pennsylvania" = "PA","Rhode Island" = "RI","South Carolina" = "SC",
                                                      "South Dakota" = "SD","Tennessee" = "TN","Texas" = "TX","Utah" = "UT","Vermont" = "VT",
                                                      "Virginia" = "VA","Washington" = "WA","West Virginia" = "WV","Wisconsin" = "WI","Wyoming" = "WY",
                                                      "Washington DC" = "DC"),
                                                    selected="All States"
                                        ),
                                        selectInput("selectYear3", "Select year:",
                                                    c("2000" = 2000,
                                                      "2010" = 2010,
                                                      "2018" = 2018),
                                                    selected=2000),
                                        selectInput("selectStyle2", "Select Map type:",
                                                    c("Default" = "default",
                                                      "Orange Highways" = "orange_high",
                                                      "Blue Highways" ="blue_high"),
                                                    selected="default"),
                                        actionButton("reset2", "Reset Checkboxes")
                                        
                                 ),#end of first column
                                 column(width = 10,

                                        box(title = textOutput("title3"), solidHeader = TRUE, status = "primary", width = 12,
                                            leafletOutput("USmap", height = 600,width = 1200)
                                        ),
                                        box(
                                          uiOutput("slider")
                                        )
                                 )#end of second column(map)
                                 
                               )
                        ) 
                      )),
             
#################  END US MAP PANEL  #################
             
             
             
             #### START ABOUT PANEL ####
             tabPanel("About",
                      
                      p("This web app was created by Jesus Perez-Serna on 3/13/2021."),
                      p("The data used in this web app is from https://www.epa.gov/egrid/download-data"),
                      p("Specifically these files eGRID2018v2 Data File (XLSX), and eGRID historical files (1996-2016) (ZIP) "),
                      p("This web app uses leaflet maps to plot different power plants across the US. You can also filter data based on type of energy source,etc.")

                      
                      )
  )
  
  
)
################# END UI #################


################# START SERVER #################
server <- function(input, output, session) {
  
  output$title1 <- renderText(paste(input$selectState1," Power Plants Map"))
  output$title2 <- renderText(paste(input$selectState2," Power Plants Map"))
  output$title3 <- renderText(paste(input$selectState3," Power Plants Map"))
  
  #reset map to orginal by unchecking boxes
  observeEvent(input$reset,{
    updateCheckboxInput(session,"checkAll",value = TRUE)
    updateCheckboxInput(session,"checkCoal",value = FALSE)
    updateCheckboxInput(session,"checkGeo",value = FALSE)  
    updateCheckboxInput(session,"checkHydro",value = FALSE)  
    updateCheckboxInput(session,"checkGas",value = FALSE)  
    updateCheckboxInput(session,"checkNC",value = FALSE)  
    updateCheckboxInput(session,"checkOil",value = FALSE)  
    updateCheckboxInput(session,"checkSol",value = FALSE)  
    updateCheckboxInput(session,"checkWind",value = FALSE)  
    updateCheckboxInput(session,"checkBio",value = FALSE)  
    updateCheckboxInput(session,"checkOth",value = FALSE)  
    updateCheckboxInput(session,"checkRN",value = FALSE)  
    updateCheckboxInput(session,"checkNRN",value = FALSE)  
    
  })
  
  #OneMap reactive for first tabpanel in navpage
  oneMap <- reactive({
    dataInfo <- ILdata
    returnData <- NULL
    
    
    
    if (input$checkCoal) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "COAL"))
    }
    if (input$checkGeo) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "GEOTHERMAL"))
    }
    if (input$checkHydro) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "HYDRO"))
    }
    if (input$checkGas) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "GAS"))
    }
    if (input$checkNC) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "NUCLEAR"))
    }
    if (input$checkOil) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "OIL"))
    }
    if (input$checkSol) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "SOLAR"))
    }
    if (input$checkWind) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "WIND"))
    }
    if (input$checkBio) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "BIOMASS"))
    }
    if (input$checkOth) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "OTHER"))
    }
    if (input$checkRN) {
      renew = c("HYDRO","SOLAR","WIND","GEOTHERMAL","BIOMASS")
      returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% renew,])
    }
    if (input$checkNRN) {
      nonrenew = c("COAL","GAS","NUCLEAR","OIL","OTHER")
      returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% nonrenew,])
    }
    # All energy sources
    if (input$checkAll) {
      returnData <- dataInfo
    }
    
    
    
    returnData
  })### END OF ONEMAP REACTIVE
  
  
  
  #reset map to orginal by unchecking boxes
  observeEvent(input$reset1,{
    updateCheckboxInput(session,"checkAll2",value = TRUE)
    updateCheckboxInput(session,"checkCoal2",value = FALSE)
    updateCheckboxInput(session,"checkGeo2",value = FALSE)  
    updateCheckboxInput(session,"checkHydro2",value = FALSE)  
    updateCheckboxInput(session,"checkGas2",value = FALSE)  
    updateCheckboxInput(session,"checkNC2",value = FALSE)  
    updateCheckboxInput(session,"checkOil2",value = FALSE)  
    updateCheckboxInput(session,"checkSol2",value = FALSE)  
    updateCheckboxInput(session,"checkWind2",value = FALSE)  
    updateCheckboxInput(session,"checkBio2",value = FALSE)  
    updateCheckboxInput(session,"checkOth2",value = FALSE)  
    updateCheckboxInput(session,"checkRN2",value = FALSE)  
    updateCheckboxInput(session,"checkNRN2",value = FALSE)  
    updateCheckboxInput(session,"checkAll3",value = TRUE)
    updateCheckboxInput(session,"checkCoal3",value = FALSE)
    updateCheckboxInput(session,"checkGeo3",value = FALSE)  
    updateCheckboxInput(session,"checkHydro3",value = FALSE)  
    updateCheckboxInput(session,"checkGas3",value = FALSE)  
    updateCheckboxInput(session,"checkNC3",value = FALSE)  
    updateCheckboxInput(session,"checkOil3",value = FALSE)  
    updateCheckboxInput(session,"checkSol3",value = FALSE)  
    updateCheckboxInput(session,"checkWind3",value = FALSE)  
    updateCheckboxInput(session,"checkBio3",value = FALSE)  
    updateCheckboxInput(session,"checkOth3",value = FALSE)  
    updateCheckboxInput(session,"checkRN3",value = FALSE)  
    updateCheckboxInput(session,"checkNRN3",value = FALSE)
    updateCheckboxInput(session,"checkLink",value = FALSE)
    updateCheckboxInput(session,"checkLink2",value = FALSE)
    
  })
  
  
  #TwoMap reactive for first tabpanel in navpage
  TwoMap <- reactive({
    dataInfo <- NULL
    returnData <- NULL
    
    # CHECK YEAR FOR FIRST MAP
    if(input$selectYear1 == 2000){
      dataInfo <- data2000
      
    }
    else if(input$selectYear1 == 2010){
      dataInfo <- data2010
    }
    else if(input$selectYear1 == 2018){
      dataInfo <- data2018
    }
    
    
    #CHECK STATE SELECTED 
    if (input$selectState1 == "All States") {
      dataInfo <- dataInfo
    }
    else {
      dataInfo <- subset(dataInfo, dataInfo$STATE == input$selectState1)
    }
    
    
    
    if (input$checkCoal2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "COAL"))
    }
    if (input$checkGeo2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "GEOTHERMAL"))
    }
    if (input$checkHydro2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "HYDRO"))
    }
    if (input$checkGas2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "GAS"))
    }
    if (input$checkNC2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "NUCLEAR"))
    }
    if (input$checkOil2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "OIL"))
    }
    if (input$checkSol2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "SOLAR"))
    }
    if (input$checkWind2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "WIND"))
    }
    if (input$checkBio2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "BIOMASS"))
    }
    if (input$checkOth2) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "OTHER"))
    }
    if (input$checkRN2) {
      renew = c("HYDRO","SOLAR","WIND","GEOTHERMAL","BIOMASS")
      returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% renew,])
    }
    if (input$checkNRN2) {
      nonrenew = c("COAL","GAS","NUCLEAR","OIL","OTHER")
      returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% nonrenew,])
    }
    # All energy sources
    if (input$checkAll2) {
      returnData <- dataInfo
    }
    
    
    if(input$checkLink == FALSE){
      updateCheckboxInput(session,"checkLink2",value = FALSE)
    }
    
    
    
    if(input$checkLink){
      
      updateCheckboxInput(session,"checkLink2",value = TRUE)
      
      if (input$checkCoal2) {
        updateCheckboxInput(session,"checkCoal3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "COAL"))
      }
      if (input$checkGeo2) {
        updateCheckboxInput(session,"checkGeo3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "GEOTHERMAL"))
      }
      if (input$checkHydro2) {
        updateCheckboxInput(session,"checkHydro3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "HYDRO"))
      }
      if (input$checkGas2) {
        updateCheckboxInput(session,"checkGas3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "GAS"))
      }
      if (input$checkNC2) {
        updateCheckboxInput(session,"checkNC3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "NUCLEAR"))
      }
      if (input$checkOil2) {
        updateCheckboxInput(session,"checkOil3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "OIL"))
      }
      if (input$checkSol2) {
        updateCheckboxInput(session,"checkSol3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "SOLAR"))
      }
      if (input$checkWind2) {
        updateCheckboxInput(session,"checkWind3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "WIND"))
      }
      if (input$checkBio2) {
        updateCheckboxInput(session,"checkBio3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "BIOMASS"))
      }
      if (input$checkOth2) {
        updateCheckboxInput(session,"checkOth3",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "OTHER"))
      }
      if (input$checkRN2) {
        updateCheckboxInput(session,"checkRN3",value = TRUE)
        renew = c("HYDRO","SOLAR","WIND","GEOTHERMAL","BIOMASS")
        returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% renew,])
      }
      if (input$checkNRN2) {
        updateCheckboxInput(session,"checkNRN3",value = TRUE)
        nonrenew = c("COAL","GAS","NUCLEAR","OIL","OTHER")
        returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% nonrenew,])
      }
      if (input$checkAll2) {
        updateCheckboxInput(session,"checkAll3",value = TRUE)
        returnData <- dataInfo
      }
      if (input$checkCoal2 == FALSE) {
        updateCheckboxInput(session,"checkCoal3",value = FALSE)
        
      }
      if (input$checkGeo2 == FALSE) {
        updateCheckboxInput(session,"checkGeo3",value = FALSE)
        
      }
      if (input$checkHydro2 == FALSE) {
        updateCheckboxInput(session,"checkHydro3",value = FALSE)
        
      }
      if (input$checkGas2 == FALSE) {
        updateCheckboxInput(session,"checkGas3",value = FALSE)
        
      }
      if (input$checkNC2 == FALSE) {
        updateCheckboxInput(session,"checkNC3",value = FALSE)
        
      }
      if (input$checkOil2 == FALSE) {
        updateCheckboxInput(session,"checkOil3",value = FALSE)
        
      }
      if (input$checkSol2 == FALSE) {
        updateCheckboxInput(session,"checkSol3",value = FALSE)
        
      }
      if (input$checkWind2 == FALSE) {
        updateCheckboxInput(session,"checkWind3",value = FALSE)
        
      }
      if (input$checkBio2 == FALSE) {
        updateCheckboxInput(session,"checkBio3",value = FALSE)
        
      }
      if (input$checkOth2 == FALSE) {
        updateCheckboxInput(session,"checkOth3",value = FALSE)
        
      }
      if (input$checkRN2 == FALSE) {
        updateCheckboxInput(session,"checkRN3",value = FALSE)
        
      }
      if (input$checkNRN2 == FALSE) {
        updateCheckboxInput(session,"checkNRN3",value = FALSE)
        
      }
      if (input$checkAll2 == FALSE) {
        updateCheckboxInput(session,"checkAll3",value = FALSE)
        
      }
      
      
      
    }
    
    
    
    
    returnData
  })### END OF ONEMAP REACTIVE
  
  
  
  #OneMap reactive for first tabpanel in navpage
  TwoMap2 <- reactive({
    dataInfo <- NULL
    returnData <- NULL
    
    # CHECK YEAR FOR FIRST MAP
    if(input$yearSelect2 == 2000){
      dataInfo <- data2000
      
    }
    else if(input$yearSelect2 == 2010){
      dataInfo <- data2010
    }
    else if(input$yearSelect2 == 2018){
      dataInfo <- data2018
    }
    
    
    #CHECK STATE SELECTED 
    if (input$stateSelect2 == "All States") {
      dataInfo <- dataInfo
    }
    else {
      dataInfo <- subset(dataInfo, dataInfo$STATE == input$stateSelect2)
    }
    
    
    
    
    
    if (input$checkCoal3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "COAL"))
    }
    if (input$checkGeo3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "GEOTHERMAL"))
    }
    if (input$checkHydro3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "HYDRO"))
    }
    if (input$checkGas3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "GAS"))
    }
    if (input$checkNC3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "NUCLEAR"))
    }
    if (input$checkOil3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "OIL"))
    }
    if (input$checkSol3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "SOLAR"))
    }
    if (input$checkWind3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "WIND"))
    }
    if (input$checkBio3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "BIOMASS"))
    }
    if (input$checkOth3) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "OTHER"))
    }
    if (input$checkRN3) {
      renew = c("HYDRO","SOLAR","WIND","GEOTHERMAL","BIOMASS")
      returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% renew,])
    }
    if (input$checkNRN3) {
      nonrenew = c("COAL","GAS","NUCLEAR","OIL","OTHER")
      returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% nonrenew,])
    }
    # All energy sources
    if (input$checkAll3) {
      returnData <- dataInfo
    }
    
    
    if(input$checkLink2 == FALSE){
      updateCheckboxInput(session,"checkLink",value = FALSE)
      
    }
    
    if(input$checkLink2){
      
      updateCheckboxInput(session,"checkLink",value = TRUE)
      
      if (input$checkCoal3) {
        updateCheckboxInput(session,"checkCoal2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "COAL"))
      }
      if (input$checkGeo3) {
        updateCheckboxInput(session,"checkGeo2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "GEOTHERMAL"))
      }
      if (input$checkHydro3) {
        updateCheckboxInput(session,"checkHydro2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "HYDRO"))
      }
      if (input$checkGas3) {
        updateCheckboxInput(session,"checkGas2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "GAS"))
      }
      if (input$checkNC3) {
        updateCheckboxInput(session,"checkNC2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "NUCLEAR"))
      }
      if (input$checkOil3) {
        updateCheckboxInput(session,"checkOil2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "OIL"))
      }
      if (input$checkSol3) {
        updateCheckboxInput(session,"checkSol2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "SOLAR"))
      }
      if (input$checkWind3) {
        updateCheckboxInput(session,"checkWind2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "WIND"))
      }
      if (input$checkBio3) {
        updateCheckboxInput(session,"checkBio2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "BIOMASS"))
      }
      if (input$checkOth3) {
        updateCheckboxInput(session,"checkOth2",value = TRUE)
        returnData <- rbind(returnData, subset(dataInfo, TYPE == "OTHER"))
      }
      if (input$checkRN3) {
        updateCheckboxInput(session,"checkRN2",value = TRUE)
        renew = c("HYDRO","SOLAR","WIND","GEOTHERMAL","BIOMASS")
        returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% renew,])
      }
      if (input$checkNRN3) {
        updateCheckboxInput(session,"checkNRN2",value = TRUE)
        nonrenew = c("COAL","GAS","NUCLEAR","OIL","OTHER")
        returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% nonrenew,])
      }
      if (input$checkAll3) {
        updateCheckboxInput(session,"checkAll2",value = TRUE)
        returnData <- dataInfo
      }
      if (input$checkCoal3 == FALSE) {
        updateCheckboxInput(session,"checkCoal2",value = FALSE)
        
      }
      if (input$checkGeo3 == FALSE) {
        updateCheckboxInput(session,"checkGeo2",value = FALSE)
        
      }
      if (input$checkHydro3 == FALSE) {
        updateCheckboxInput(session,"checkHydro2",value = FALSE)
        
      }
      if (input$checkGas3 == FALSE) {
        updateCheckboxInput(session,"checkGas2",value = FALSE)
        
      }
      if (input$checkNC3 == FALSE) {
        updateCheckboxInput(session,"checkNC2",value = FALSE)
        
      }
      if (input$checkOil3 == FALSE) {
        updateCheckboxInput(session,"checkOil2",value = FALSE)
        
      }
      if (input$checkSol3 == FALSE) {
        updateCheckboxInput(session,"checkSol2",value = FALSE)
        
      }
      if (input$checkWind3 == FALSE) {
        updateCheckboxInput(session,"checkWind2",value = FALSE)
        
      }
      if (input$checkBio3 == FALSE) {
        updateCheckboxInput(session,"checkBio2",value = FALSE)
        
      }
      if (input$checkOth3 == FALSE) {
        updateCheckboxInput(session,"checkOth2",value = FALSE)
        
      }
      if (input$checkRN3 == FALSE) {
        updateCheckboxInput(session,"checkRN2",value = FALSE)
        
      }
      if (input$checkNRN3 == FALSE) {
        updateCheckboxInput(session,"checkNRN2",value = FALSE)
        
      }
      if (input$checkAll3 == FALSE) {
        updateCheckboxInput(session,"checkAll2",value = FALSE)
        
      }
    }
    
    
    returnData
  })### END OF ONEMAP REACTIVE
  
  
  
  
  #reset map to orginal by unchecking boxes
  observeEvent(input$reset2,{
    updateCheckboxInput(session,"checkAll4",value = TRUE)
    updateCheckboxInput(session,"checkCoal4",value = FALSE)
    updateCheckboxInput(session,"checkGeo4",value = FALSE)  
    updateCheckboxInput(session,"checkHydro4",value = FALSE)  
    updateCheckboxInput(session,"checkGas4",value = FALSE)  
    updateCheckboxInput(session,"checkNC4",value = FALSE)  
    updateCheckboxInput(session,"checkOil4",value = FALSE)  
    updateCheckboxInput(session,"checkSol4",value = FALSE)  
    updateCheckboxInput(session,"checkWind4",value = FALSE)  
    updateCheckboxInput(session,"checkBio4",value = FALSE)  
    updateCheckboxInput(session,"checkOth4",value = FALSE)  
    updateCheckboxInput(session,"checkRN4",value = FALSE)  
    updateCheckboxInput(session,"checkNRN4",value = FALSE)  
    
  })
  
  #OneMap reactive for first tabpanel in navpage
  USMap <- reactive({
    dataInfo <- NULL
    returnData <- NULL

    
    # CHECK YEAR FOR FIRST MAP
    if(input$selectYear3 == 2000){
      dataInfo <- data2000
      
      
    }
    else if(input$selectYear3 == 2010){
      dataInfo <- data2010
      

    }
    else if(input$selectYear3 == 2018){
      dataInfo <- data2018
      
      
    }

    
    
    #CHECK STATE SELECTED 
    if (input$selectState3 == "All States") {
      dataInfo <- dataInfo
    }
    else {
      dataInfo <- subset(dataInfo, dataInfo$STATE == input$selectState3)
    }
    
    
    
    if (input$checkCoal4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "COAL" & GENACOAL>min(input$slider) & GENACOAL<max(input$slider)))
    }
    if (input$checkGeo4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "GEOTHERMAL" & GENAGT>min(input$slider) & GENAGT<max(input$slider)))
    }
    if (input$checkHydro4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "HYDRO" & GENAHY>min(input$slider) & GENAHY<max(input$slider)))
    }
    if (input$checkGas4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "GAS" & GENAGAS>min(input$slider) & GENAGAS<max(input$slider)))
    }
    if (input$checkNC4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "NUCLEAR" & GENANC>min(input$slider) & GENANC<max(input$slider)))
    }
    if (input$checkOil4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "OIL" & GENAOIL>min(input$slider) & GENAOIL<max(input$slider)))
    }
    if (input$checkSol4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "SOLAR" & GENASO>min(input$slider) & GENASO<max(input$slider)))
    }
    if (input$checkWind4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "WIND" & GENAWI>min(input$slider) & GENAWI<max(input$slider)))
    }
    if (input$checkBio4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "BIOMASS" & GENABM>min(input$slider) & GENABM<max(input$slider)))
    }
    if (input$checkOth4) {
      returnData <- rbind(returnData, subset(dataInfo, TYPE == "OTHER" & GENAO>min(input$slider) & GENAO<max(input$slider)))
    }
    if (input$checkRN4) {
      renew = c("HYDRO","SOLAR","WIND","GEOTHERMAL","BIOMASS")
      returnData <- rbind(returnData,dataInfo[dataInfo$TYPE  %in% renew,] )
    }
    if (input$checkNRN4) {
      nonrenew = c("COAL","GAS","NUCLEAR","OIL","OTHER")
      returnData <- rbind(returnData,dataInfo[dataInfo$TYPE %in% nonrenew,] )
    }
    # All energy sources
    if (input$checkAll4) {
      returnData <- dataInfo
    }
    
    
    
    returnData
  })### END OF ONEMAP REACTIVE
  
  
  
  output$slider <- renderUI({
    
    maxGena <- 0
    
    if(input$selectYear3 == 2000){
      if(input$selectState3 == "All States"){
        
        
        if(input$checkAll4){
          maxGena <-max(data2000$TOTAL)
        }
        if(input$checkCoal4){
          maxGena <- max(data2000$GENACOAL)
        }
        if (input$checkGeo4) {
          maxGena <- max(data2000$GENAGT)
          
        }
        if (input$checkHydro4) {
          maxGena <- max(data2000$GENAHY)
          
        }
        if (input$checkGas4) {
          maxGena <- max(data2000$GENAGAS)
          
        }
        if (input$checkOil4) {
          maxGena <- max(data2000$GENAOIL)
          
        }
        if (input$checkSol4) {
          maxGena <- max(data2000$GENASO)
          
        }
        if (input$checkWind4) {
          maxGena <- max(data2000$GENAWI)
          
        }
        if (input$checkBio4) {
          maxGena <- max(data2000$GENABM)
          
        }
        if (input$checkOth4) {
          maxGena <- max(data2000$GENAO)
          
        }
        if (input$checkNC4) {
          maxGena <- max(data2000$GENANC)
          
        }
        if (input$checkRN4) {
          maxGena <- max(data2000$GENATR)
          
        }
        if (input$checkNRN4) {
          maxGena <- max(data2000$GENATN)
          
        }
        else{
          
          inputState <- subset(data2000, data2000$STATE == input$selectState3)
          if(input$checkAll4){
            maxGena <-max(data2000$TOTAL)
          }
          if(input$checkCoal4){
            maxGena <- max(data2000$GENACOAL)
          }
          if (input$checkGeo4) {
            maxGena <- max(data2000$GENAGT)
            
          }
          if (input$checkHydro4) {
            maxGena <- max(data2000$GENAHY)
            
          }
          if (input$checkGas4) {
            maxGena <- max(data2000$GENAGAS)
            
          }
          if (input$checkOil4) {
            maxGena <- max(data2000$GENAOIL)
            
          }
          if (input$checkSol4) {
            maxGena <- max(data2000$GENASO)
            
          }
          if (input$checkWind4) {
            maxGena <- max(data2000$GENAWI)
            
          }
          if (input$checkBio4) {
            maxGena <- max(data2000$GENABM)
            
          }
          if (input$checkOth4) {
            maxGena <- max(data2000$GENAO)
            
          }
          if (input$checkNC4) {
            maxGena <- max(data2000$GENANC)
            
          }
          if (input$checkRN4) {
            maxGena <- max(data2000$GENATR)
            
          }
          if (input$checkNRN4) {
            maxGena <- max(data2000$GENATN)
            
          }
          
        }
      }
    }
    
    
    
    if(input$selectYear3 == 2010){
      if(input$selectState3 == "All States"){
        
        
        if(input$checkAll4){
          maxGena <-max(data2010$TOTAL)
        }
        if(input$checkCoal4){
          maxGena <- max(data2010$GENACOAL)
        }
        if (input$checkGeo4) {
          maxGena <- max(data2010$GENAGT)
          
        }
        if (input$checkHydro4) {
          maxGena <- max(data2010$GENAHY)
          
        }
        if (input$checkGas4) {
          maxGena <- max(data2010$GENAGAS)
          
        }
        if (input$checkOil4) {
          maxGena <- max(data2010$GENAOIL)
          
        }
        if (input$checkSol4) {
          maxGena <- max(data2010$GENASO)
          
        }
        if (input$checkWind4) {
          maxGena <- max(data2010$GENAWI)
          
        }
        if (input$checkBio4) {
          maxGena <- max(data2010$GENABM)
          
        }
        if (input$checkOth4) {
          maxGena <- max(data2010$GENAO)
          
        }
        if (input$checkNC4) {
          maxGena <- max(data2010$GENANC)
          
        }
        if (input$checkRN4) {
          maxGena <- max(data2010$GENATR)
          
        }
        if (input$checkNRN4) {
          maxGena <- max(data2010$GENATN)
          
        }
        else{
          
          inputState <- subset(data2010, data2010$STATE == input$selectState3)
          if(input$checkAll4){
            maxGena <-max(data2010$TOTAL)
          }
          if(input$checkCoal4){
            maxGena <- max(data2010$GENACOAL)
          }
          if (input$checkGeo4) {
            maxGena <- max(data2010$GENAGT)
            
          }
          if (input$checkHydro4) {
            maxGena <- max(data2010$GENAHY)
            
          }
          if (input$checkGas4) {
            maxGena <- max(data2010$GENAGAS)
            
          }
          if (input$checkOil4) {
            maxGena <- max(data2010$GENAOIL)
            
          }
          if (input$checkSol4) {
            maxGena <- max(data2010$GENASO)
            
          }
          if (input$checkWind4) {
            maxGena <- max(data2010$GENAWI)
            
          }
          if (input$checkBio4) {
            maxGena <- max(data2010$GENABM)
            
          }
          if (input$checkOth4) {
            maxGena <- max(data2010$GENAO)
            
          }
          if (input$checkNC4) {
            maxGena <- max(data2010$GENANC)
            
          }
          if (input$checkRN4) {
            maxGena <- max(data2010$GENATR)
            
          }
          if (input$checkNRN4) {
            maxGena <- max(data2010$GENATN)
            
          }
          
        }
      }
    }
    
    

    if(input$selectYear3 == 2018){
      if(input$selectState3 == "All States"){
        
        
        if(input$checkAll4){
          maxGena <-max(data2018$TOTAL)
        }
        if(input$checkCoal4){
          maxGena <- max(data2018$GENACOAL)
        }
        if (input$checkGeo4) {
          maxGena <- max(data2018$GENAGT)
          
        }
        if (input$checkHydro4) {
          maxGena <- max(data2018$GENAHY)
          
        }
        if (input$checkGas4) {
          maxGena <- max(data2018$GENAGAS)
          
        }
        if (input$checkOil4) {
          maxGena <- max(data2018$GENAOIL)
          
        }
        if (input$checkSol4) {
          maxGena <- max(data2018$GENASO)
          
        }
        if (input$checkWind4) {
          maxGena <- max(data2018$GENAWI)
          
        }
        if (input$checkBio4) {
          maxGena <- max(data2018$GENABM)
          
        }
        if (input$checkOth4) {
          maxGena <- max(data2018$GENAO)
          
        }
        if (input$checkNC4) {
          maxGena <- max(data2018$GENANC)
          
        }
        if (input$checkRN4) {
          maxGena <- max(data2018$GENATR)
          
        }
        if (input$checkNRN4) {
          maxGena <- max(data2018$GENATN)
          
        }
        else{
          
          inputState <- subset(data2018, data2018$STATE == input$selectState3)
          if(input$checkAll4){
            maxGena <-max(data2018$TOTAL)
          }
          if(input$checkCoal4){
            maxGena <- max(data2018$GENACOAL)
          }
          if (input$checkGeo4) {
            maxGena <- max(data2018$GENAGT)
            
          }
          if (input$checkHydro4) {
            maxGena <- max(data2018$GENAHY)
            
          }
          if (input$checkGas4) {
            maxGena <- max(data2018$GENAGAS)
            
          }
          if (input$checkOil4) {
            maxGena <- max(data2018$GENAOIL)
            
          }
          if (input$checkSol4) {
            maxGena <- max(data2018$GENASO)
            
          }
          if (input$checkWind4) {
            maxGena <- max(data2018$GENAWI)
            
          }
          if (input$checkBio4) {
            maxGena <- max(data2018$GENABM)
            
          }
          if (input$checkOth4) {
            maxGena <- max(data2018$GENAO)
            
          }
          if (input$checkNC4) {
            maxGena <- max(data2018$GENANC)
            
          }
          if (input$checkRN4) {
            maxGena <- max(data2018$GENATR)
            
          }
          if (input$checkNRN4) {
            maxGena <- max(data2018$GENATN)
            
          }
          
        }
      }
    }
    
    

    
    
  sliderInput("slider","Generation Range(kMWh):", min   = 0,
              max   = maxGena,
              value = c(0,maxGena))
    })
  
  
  
  
  
  
  
  ############## RENDER FOR IL MAP
  
  output$leaf <- renderLeaflet({
    m <- leaflet(data = oneMap()) %>%
      addProviderTiles("OpenStreetMap.HOT") %>%  
      addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                       popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                       "Percent Renewable:", TNPR ,"%", "<br>",
                                       "Percent Non-Renewable:", TRPR,"%")
                       ,color = ~pal(TYPE)
                       ,radius = ~RADIUS)%>% 
      addLegend("bottomright", 
                pal = pal,
                values = ~ TYPE,
                title= "Type of Energy Source",
                opacity = .80)%>%
      addResetMapButton()
    
    m  # Print the map
  })
  ############# END OF RENDER FOR IL MAP
  
  output$leaf1 <- renderLeaflet({
    if(input$selectStyle == "default"){
      m <- leaflet(data = TwoMap()) %>%
        addProviderTiles("OpenStreetMap.HOT") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>%
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    
    else if(input$selectStyle == "orange_high"){
      m <- leaflet(data = TwoMap()) %>%
        addProviderTiles("OpenStreetMap.DE") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    else if(input$selectStyle == "blue_high"){
      m <- leaflet(data = TwoMap()) %>%
        addProviderTiles("Esri.DeLorme") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    
  })
  
  output$leaf2 <- renderLeaflet({
    if(input$selectStyle == "default"){
      m <- leaflet(data = TwoMap2()) %>%
        addProviderTiles("OpenStreetMap.HOT") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    else if(input$selectStyle == "orange_high"){
      m <- leaflet(data = TwoMap2()) %>%
        addProviderTiles("OpenStreetMap.DE") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    else if(input$selectStyle == "blue_high"){
      m <- leaflet(data = TwoMap2()) %>%
        addProviderTiles("Esri.DeLorme") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    
  })
  
  
  ################ 
  output$USmap <- renderLeaflet({
    if(input$selectStyle == "default"){
      m <- leaflet(data = TwoMap2()) %>%
        addProviderTiles("OpenStreetMap.HOT") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    else if(input$selectStyle == "orange_high"){
      m <- leaflet(data = TwoMap2()) %>%
        addProviderTiles("OpenStreetMap.DE") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    else if(input$selectStyle == "blue_high"){
      m <- leaflet(data = TwoMap2()) %>%
        addProviderTiles("Esri.DeLorme") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      m  # Print the map
    }
    
  })
  
  
  
  
  
  
  
  
  
  
  output$USmap <- renderLeaflet({

    
    if(input$selectStyle2 == "default"){
      m <- leaflet(data = USMap()) %>%
        addProviderTiles("OpenStreetMap.HOT") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      
      m  # Print the map

    }
    else if(input$selectStyle2 == "orange_high"){
      
      m <- leaflet(data = USMap()) %>%
        addProviderTiles("OpenStreetMap.DE") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      
      m  # Print the map
    
    }
    else if(input$selectStyle2 == "blue_high"){
      
      m <- leaflet(data = USMap()) %>%
        addProviderTiles("Esri.DeLorme") %>%  
        addCircleMarkers(lat = ~ LAT, lng = ~ LON ,
                         popup = ~ paste("Plant Name:",  PNAME, "<br>",
                                         "Percent Renewable:", TNPR ,"%", "<br>",
                                         "Percent Non-Renewable:", TRPR,"%")
                         ,color = ~pal(TYPE)
                         ,radius = ~RADIUS)%>% 
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ TYPE,
                  title= "Type of Energy Source",
                  opacity = .80)%>%
        addResetMapButton()
      
      m  # Print the map

    }
    
  })
  
  
  
  
  
  
  
  
  
}
################# ENDSERVER #################

shinyApp(ui, server)