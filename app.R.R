library(shiny)
library(ggvis)
library(cluster)
DataAll <- read.csv("DataAll.csv")
colnames(DataAll)<-c("Country","Latitude","Longitude","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009")


ui <- fluidPage(
  headerPanel("Exploring Infant Mortality Rate for 177 Countries Using the k-Means Clustering Technique"),
  sidebarLayout(position = "right", sidebarPanel(selectInput("year", "Choose a year", list("2019", "2018", "2017","2016","2015","2014","2013","2012","2011","2010","2009")),
  numericInput("clusters", "Number of clusters", "4", min=2, max=8)),
  mainPanel("Infant Mortality Rate represents the number of deaths of children, who are less than 1 year of age, per 1000 births. Use the dropdown menu to the right to review clustering results for a specific year (2009 through 2019). You can also adjust the number of clusters by typing in a value between 2 and 8 (inclusive).")),
  plotOutput("plot1")
) 
  
server <- function(input, output) {
  selectedData <- reactive({
    DataAll[, c("Latitude", "Longitude", input$year)]
  })
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
})
  
  output$plot1 <- renderPlot({
    clusplot(selectedData(), clusters()$cluster, color=T, shade=F,labels=0,lines=0, main=input$year)
  })
  
}
shinyApp(ui = ui, server = server)