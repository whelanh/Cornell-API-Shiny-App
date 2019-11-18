library(DT)
library(shiny)
library(data.table)
library(stringr)
library(rebird)
library(tableHTML)
library(geosphere)
library(htmlTable)


locs <- as.data.table(read.csv("data//birdingLocations.csv"))

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Bird Target List"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Text for providing a caption ----
            # Note: Changes made to the caption in the textInput control
            # are updated in the output area immediately as you type

            # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                        label = "Choose a life list:",
                        choices = c("ABA Life", "FL Life", "ABA Year", "ABA World")),
            
            # Input: Numeric entry for number of obs to view ----
            selectInput(inputId = "location",
                         label = "Choose a location:",
                         choices = locs$City)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Formatted text for caption ----
            h3(textOutput("caption", container = shiny::span)),
            
            # Output: HTML table with requested number of observations ----
            DT::dataTableOutput("view")
            
        )
    )
)
multitab <- function() {
  tab1 <- data.table(ebirdregion(loc='US-CT',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx'))
  tab2 <- data.table(ebirdregion(loc='US-MA',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx'))
  tab3 <- data.table(ebirdregion(loc='US-NY',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx'))
  rbind(tab1,tab2,tab3)
}

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    # Return the requested dataset ----
    # By declaring datasetInput as a reactive expression we ensure
    # that:
    #
    # 1. It is only called when the inputs it depends on changes
    # 2. The computation and result are shared by all the callers,
    #    i.e. it only executes a single time
    datasetInput <- reactive({
        switch(input$location,
               "Ft. Myers" = data.table(ebirdregion(loc='US-FL',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "W. Hartford" =  multitab(),
               "Columbus-OH" = data.table(ebirdregion(loc='US-OH',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "W. Pittston-PA" = data.table(ebirdregion(loc='US-PA',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Quintana Roo-MX" = data.table(ebirdregion(loc='MX-ROO',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Orleans-MA" = data.table(ebirdregion(loc='US-MA',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "St. Paul-MN" = data.table(ebirdregion(loc='US-MN',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Madison-WI" = data.table(ebirdregion(loc='US-WI',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Duluth-MN" = data.table(ebirdregion(loc='US-MN',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Grantsburg-WI" = data.table(ebirdregion(loc='US-WI',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Eagle River-WI" = data.table(ebirdregion(loc='US-WI',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "McAllen-TX" = data.table(ebirdregion(loc='US-TX',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Glen Spey-NY" = data.table(ebirdregion(loc='US-NY',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Zapata-TX" = data.table(ebirdregion(loc='US-TX',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "La Joya-TX" = data.table(ebirdregion(loc='US-TX',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')),
               "Seward-AK" = data.table(ebirdregion(loc='US-AK',provisional=TRUE,back=4,simple=FALSE,key='xxxxxxxxxxxx')))
    })

datasetInput2 <- reactive({
    switch(input$dataset,
           "ABA Life" = as.data.table(read.csv("data//ebird_aba_life_list.csv")),
           "FL Life" = as.data.table(read.csv("data//ebird_US-FL_life_list.csv")),
           "ABA Year" = as.data.table(read.csv("data//ebird_aba_year_list.csv")),
           "ABA World" = as.data.table(read.csv("data//ebird_world_life_list.csv")))
})



# Create caption ----
# The output$caption is computed based on a reactive expression
# that returns input$caption. When the user changes the
# "caption" field:
#
# 1. This function is automatically called to recompute the output
# 2. New caption is pushed back to the browser for re-display
#
# Note that because the data-oriented reactive expressions
# below don't depend on input$caption, those expressions are
# NOT called when input$caption changes
output$caption <- renderText({
    input$location
})

# Generate a summary of the dataset ----
# The output$summary depends on the datasetInput reactive
# expression, so will be re-executed whenever datasetInput is
# invalidated, i.e. whenever the input$dataset changes
output$summary <- renderPrint({
    dataset <- datasetInput()
    dataset2 <- datasetInput2()
    
})

# Show the first "n" observations ----
# The output$view depends on both the databaseInput reactive
# expression and input$obs, so it will be re-executed whenever
# input$dataset or input$obs is changed
output$view <- DT::renderDataTable({
    dataset <- datasetInput()
    dataset <- dataset[-grep("sp\\.",dataset$comName),]
    dataset <- dataset[-grep("/",dataset$comName),]
    dataset2 <- datasetInput2()
    hUSunique <- as.data.table(unique(dataset2[,Species]))
    colnames(hUSunique) <- c("comName")
    data1 <- dataset[!hUSunique, on="comName"]
    data1[,N:=uniqueN(sciName),by=locId]
    # Count total number of birds you need observed at each location
    ans<-data1[,.(locName,comName,obsDt,lat,lng,locId,N)]
    colnames(ans) <- c("place","bird","date","lat","lng","locId","N")
    # Calculate distance in miles and approximate time (using 30 mph) to each location
    dist <- unique(ans[,.(trimws(place),lat,lng)])
    
    dis=data.table(miles=numeric(), minutes=numeric())
    currLoc <- c(locs[City==input$location,Lon],locs[City==input$location,Lat])
    
    
    for (i in 1:nrow(dist)) {
        dest<-c(dist[i,lng],dist[i,lat])
        m<-distHaversine(currLoc,dest)/1609.34
        dp <- data.table(miles=m,
                         minutes=round(m/30*60,0))
        dis <- rbind(dis,dp)
    }
    
    dist <- cbind(dist[,.(V1,lat,lng)],dis)
    colnames(dist) <- c("place","lat", "lon","miles","minutes")
    comb <- merge(ans,dist[,.(place,miles,minutes,lat,lon)])
    comb[,map:=paste0('<a href="https://www.google.com/maps/place/',lat,",",lng,"/",
                      "@",lat,",",lng,",13z",'" target="_blank">','map' ,"</a>")]
    comb[,miles:=round(miles,0)]
    comb[,chklst:=paste0('<a href="https://ebird.org/hotspot/',comb[,locId],'" target="_blank">','locId' ,"</a>")]
    comb[,bird:=gsub(" ","_",comb[,bird])]
    comb[,bird:=paste0('<a href="https://www.allaboutbirds.org/guide/',comb[,bird],'" target="_blank">',comb[,bird] ,"</a>")]
    comb <- comb[,.(place,bird,date,map,chklst,N,miles)]
    comb <- comb[order(comb[,miles]),]
    colnames(comb) <- c("place","bird","date","map","locId","#@hotspot","miles from location")
    DT::datatable(comb, escape = F, options = list(pageLength=40))
})

}

# Create Shiny app ----
shinyApp(ui, server)
