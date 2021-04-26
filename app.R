# install packages
install.packages("tigris")
install.packages("mapview")
install.packages("leaflet")
# libraries to include
library(purrr)
library(dplyr)
library(ggplot2)
library(shiny)
library(stringr)
library(shinydashboard)
library(shinythemes)
library(reshape2)
library(leaflet)
library(DT)
library(sp)
library(tigris)
library(mapview)

options(scipen = 999)


setwd("/Users/hkhandwala06/Desktop/CS424/424P3")
file10 = "Energy_Usage_2010.csv"

chi_data <- read.csv(file = file10, sep = ",")
colnames(chi_data)[colnames(chi_data) == "CENSUS.BLOCK"] <- "GEOID10"
chi_data$BUILDING.TYPE <- as.factor(chi_data$BUILDING.TYPE)
chi_data$COMMUNITY.AREA.NAME <- as.factor(chi_data$COMMUNITY.AREA.NAME)

cook_blocks <- blocks(state='IL', county='Cook', year="2010")
cook_blocks$GEOID10 <- as.numeric(cook_blocks$GEOID10)
chi_blocks <- subset(cook_blocks,  GEOID10 %in% chi_data$GEOID10)

cook_tract <- tracts("IL", county = "Cook", year = 2010)
colnames(cook_tract)[colnames(cook_tract) == "CENSUS.BLOCK"] <- "GEOID10"

communities <- levels(chi_data$COMMUNITY.AREA.NAME)
communities <- c(communities, "All")

filters <- c("Electricity Usage", "Gas Usage", "Building Age", "Building Height", "Total Population")
months <- c("Total", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
building_type <- levels(chi_data$BUILDING.TYPE)
building_type[1] <- "All"


ui <- navbarPage("CS 424 - Project 3", id="nav", theme=shinytheme("cosmo"),
                 tabPanel("About",
                          h1("About this Project"),
                          hr(),
                          p("This web app was created by Herrit Khandwala on 4/24/2021. The data used in this web app is from https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp
                To download the data, hit the export button and download the file as a CSV or CSV for Excel.")
                          
                          
                 ),
                 tabPanel("Part 1",
                          bootstrapPage(
                            tags$style(type = "text/css", "html, body {width:100%;height:100%;"),
                            h1("Near West Side Heatmap"),
                            fluidRow(class="f-row",
                                     column(3,
                                            selectInput("part1_filters", label = "Filters",
                                                        choices = filters,
                                                        selected = "Electricity Usage")),
                                     column(3, 
                                            selectInput("part1_Months", label = "Months",
                                                        choices = months,
                                                        selected = "Total")),
                                     column(3,
                                            selectInput("part1_BuildingTypes", label = "Building Types",
                                                        choices = building_type,
                                                        selected = "All")),
                                     column(3, top=15, left="0%",
                                            actionButton("part1_RESET", "RESET", class="bg-danger"))
                            ),
                            mapviewOutput("part1_map"),
                            fluidRow(
                              column(6,
                                     h1("Graph"),
                                     plotOutput("part1_plot")),
                              column(6, 
                                     h1("Table"),
                                     DT::dataTableOutput("part1_table"))
                            )
                            
                          )
                 ),
                 
                 tabPanel("Part 2",
                          bootstrapPage(
                            fluidRow(
                              column(6,
                                     fluidRow(class="f-row",
                                              column(3,
                                                     selectInput("part2_communities1", label = "Communities",
                                                                 choices = communities[-c(1)],
                                                                 selected = "Near West Side")),
                                              column(3, offset=5,
                                                     actionButton("part2_RESET1", "RESET", class="bg-danger"))
                                     ),
                                     fluidRow(class="f-row",
                                              column(4,
                                                     selectInput("part2_filters1", label = "Filters",
                                                                 choices = filters,
                                                                 selected = "Electricity Usage")),
                                              column(4, 
                                                     selectInput("part2_Months1", label = "Months",
                                                                 choices = months,
                                                                 selected = "Total")),
                                              column(4,
                                                     selectInput("part2_BuildingTypes1", label = "Building Types",
                                                                 choices = building_type,
                                                                 selected = "All")),
                                     ),
                                     mapviewOutput("part2_map1"),
                              ),
                              column(6,
                                     fluidRow(class="f-row",
                                              column(3,
                                                     selectInput("part2_communities2", label = "Communities",
                                                                 choices = communities[-c(1)],
                                                                 selected = "Loop")),
                                              column(3, offset=5,
                                                     actionButton("part2_RESET2", "RESET", class="bg-danger"))),
                                     fluidRow(class="f-row",
                                              column(4,
                                                     selectInput("part2_filters2", label = "Filters",
                                                                 choices = filters,
                                                                 selected = "Electricity Usage")),
                                              column(4, 
                                                     selectInput("part2_Months2", label = "Months",
                                                                 choices = months,
                                                                 selected = "Total")),
                                              column(4,
                                                     selectInput("part2_BuildingTypes2", label = "Building Types",
                                                                 choices = building_type,
                                                                 selected = "All")),
                                     ),
                                     mapviewOutput("part2_map2"),
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Part 3",
                          bootstrapPage(
                            fluidRow(class="f-row",
                                     column(3,
                                            selectInput("part3_communities", label = "Communities",
                                                        choices = communities,
                                                        selected = "All")),
                                     column(3, offset=5,
                                            actionButton("part3_RESET", "RESET", class="bg-danger"))
                            ),
                            fluidRow(class="f-row",
                                     column(4,
                                            selectInput("part3_Filters", label = "Filters",
                                                        choices = filters,
                                                        selected = "Electricity Usage")),
                                     column(4, 
                                            selectInput("part3_Months", label = "Months",
                                                        choices = months,
                                                        selected = "Total")),
                                     column(4,
                                            selectInput("part3_BuildingTypes", label = "Building Types",
                                                        choices = building_type,
                                                        selected = "All")),
                            ),
                            mapviewOutput("part3_map")
                          )
                 )
)

getMap <- function(community, filter, month, building_type) {
  data <- 0
  
  if (community != "All") {
    data <- subset(chi_data, chi_data$COMMUNITY.AREA.NAME %in% community)
  } else {
    data <- chi_data
  }
  
  blocks <- subset(chi_blocks, GEOID10 %in% data$GEOID10)
  filts <- c()
  
  switch (filter,
          "Electricity Usage" = filts <- colnames(chi_data)[grepl("KWH", colnames(chi_data))],
          "Gas Usage" = filts <- colnames(chi_data)[grepl("THERM", colnames(chi_data))],
          "Building Age" = filts <- colnames(chi_data)[grepl("BUILDING.AGE", colnames(chi_data))],
          "Building Height" = filts <- colnames(chi_data)[grepl("STORIES", colnames(chi_data))],
          "Total Population" = filts <- colnames(chi_data)[grepl("POPULATION", colnames(chi_data))]
  )
  
  if (filter == "Electricity Usage" || filter == "Gas Usage") {
    filts <- filts[grepl(toupper(month), filts)]
  }
  
  data <- merge(blocks, data[c(filts, "GEOID10", "BUILDING.TYPE")], by = "GEOID10")
  
  if (building_type != "All") {
    filtered <- subset(data, BUILDING.TYPE %in% building_type)
    return( mapView(filtered, z=filts))
  } else {
    return(mapView(data, z=filts))
  }
}


server = function(input, output) {
  
  ## PART 1
  output$part1_map <- renderLeaflet({ 
    m <- getMap("Near West Side", input$part1_filters, input$part1_Months, input$part1_BuildingTypes)
    m@map
  })
  
  output$plot2 <- renderPlot({
    
    data <- querryData("Near West Side", input)
    
    ggplot(data=chi_data)
  })
  
  output$mytable <- DT::renderDataTable({
    new_data <- 0
    
    
    DT::datatable(chi_data, rownames = FALSE)
    
  })
  
  ## PART 2
  
  output$part2_map1 <- renderLeaflet({ 
    m <- getMap(input$part2_communities1, input$part2_filters1, input$part2_Months1, input$part2_BuildingTypes1)
    m@map
  })
  
  output$part2_map2 <- renderLeaflet({ 
    m <- getMap(input$part2_communities2, input$part2_filters2, input$part2_Months2, input$part2_BuildingTypes2)
    m@map
  })
  
  ## PART 3
  
  output$part3_map <- renderLeaflet({ 
    m <- mapView(cook_tract)
    m@map
  })
}


# Actually run the Shiny App
shinyApp(ui, server)


