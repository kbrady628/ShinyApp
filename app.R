library(shiny)
library(ggvis)
library(cluster)
library(DT)
DataAll <- read.csv("DataAll.csv")
colnames(DataAll)<-c("Country","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010")


ui <- fluidPage(
  titlePanel(h1("Analysis of Infant Mortality Rate for 177 Countries")),
  
  sidebarLayout(position = "right", sidebarPanel(numericInput("clusters", "Specify Number of clusters (4 through 8)", "4", min=4, max=8)),
  
  
  mainPanel(
    h3("Infant Mortality rate is the number of deaths per 1000 births for children less than 1 year of age."),
    tabsetPanel(type="tabs",
      tabPanel("Overview",
               p("This application uses the k-means clustering technique to cluster data based on infant mortality rate for 177 countries for 10 years (2010-2019)."),
               p("Change the number of clusters by typing in a value or using the arrows in the box (4 through 8)."),
               p("Click the k-Means Cluster tab to view the clustering results, and the All Data tab to view all of the data and the assigned cluster for the selected number of clusters."),
               p("Clustering is a technique used to group similar data of a dataset. There are multiple types of clustering methods, including k-means clustering which is an unsupervised learning algorithm used for knowledge discovery. K-means clustering provides intuition about data structure, and the process consists of identification of subgroups in the data so data points in the same cluster are alike, but different than data points in other clusters.")),
      tabPanel("k-Means Cluster",plotOutput("plot1")),
      tabPanel("All Data",
               p("The data table below shows the Infant Mortality Rate for all 177 countries for each year. The last column shows the assigned cluster. The number of clusters can be changed using the box in the top right."),
               DT::dataTableOutput("table")))
      )

  )
)

server <- function(input, output) {
  selectedData<-DataAll[, c("2019", "2018", "2017","2016","2015","2014","2013","2012","2011","2010")]
  clusters <- reactive({
    kmeans(selectedData, input$clusters)
})
 
  
  
    output$plot1 <- renderPlot({
    clusplot(selectedData, clusters()$cluster, color=T, shade=F,labels=0,lines=0, main='k-Means Clustering Analysis')
  })
  
  #Generate HTML table view of the data.
    output$table <-DT::renderDataTable({
    DataAll$Cluster<-clusters()$cluster
    DataAll[, c("Country", "2019", "2018", "2017","2016","2015","2014","2013","2012","2011","2010", "Cluster")]
  })
  
}
shinyApp(ui = ui, server = server)