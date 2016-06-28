library(shinythemes)
shinyUI(fluidPage(theme = shinytheme("spacelab"),
                  tags$head(includeScript("googleanalytics.js")),
  navbarPage("2D Outlier Analysis",tabPanel("App",
  sidebarPanel(
    selectInput('Model',label="Model",choices=list("Hierarchical Clustering"=12,
                                  "Mahalanobis Distance"=2,
                                  "EM - can be slow to converge"=17,
                                  "Kmeans Euclidean Distance" =1, 
                                  "Kmeans Means Distance"=3,
                                  #"Kmeans Minkowski"=4, 
                                  "Fuzzy kmeans"=9,"Fuzzy kmeans - Gustafson and Kessel"=10,"Fuzzy k-medoids"=13,"Fuzzy k-means with polynomial fuzzifier"=11, 
                                  "Local Outlier Factor"=5,
                                 # "SVM"=16,
                                  "RandomForest"=6,"Isolation Forest"=7,
                                  "FBOD"=14,"SOD"=15,
                                  "Autoencoder"=8
                                  ),selected = 1),
    selectInput('sample',label="Sample",choices=list("Random" =1, 
                                  "Corners"=2,"Doughnut"=3,"Smiley"=4,"Spiral"=5,"Eyes"=6,"Butterfly"=7,"Axis-Parallel Subspace"=8,"StarsCYG"=9),selected = 1),
    numericInput('clusters', 'Cluster count/Neighbors', 3,
                 min = 1, max = 9),
    sliderInput("outlierper", "Outlier %:", 
                min = 85, max = 99, value = 90, step= 1), 
    actionButton("button", "Update Sample Data"),
    p(),
    actionButton("scalebutton", "Scale Data")
  ),
  mainPanel(
    plotOutput('plot1',click = "plot_click",
               brush = brushOpts(id = "plot1_brush")),
    DT::dataTableOutput("mytable1") 
  )),
tabPanel("About" ,
         fluidRow(
           column(10,includeMarkdown("docs/introduction.md"))
         )),
tabPanel("Tutorial" ,
         fluidRow(
           column(10,includeMarkdown("docs/tutorial.md"))
         ))

) ))