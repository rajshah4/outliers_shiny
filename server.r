
#setwd("~/Documents/Documents/Projects/Anomaly")
library(DT) 
library(proxy)
library(dbscan)
library(mlbench)
library(randomForest)
library(IsolationForest)
library(autoencoder)
library(fclust)
library(DMwR)
library(HighDimOut)
library(robustbase)
library(SeleMix)
#library(e1071)
#library(reshape)


shinyServer(function(input, output, session) {
  
  selectedData <- reactiveValues()
  
    #####EVENTS
  counter <- 1
   observeEvent(input$button, {
     n <- 50
     if (input$sample==1) {y <- data.frame(cbind(x=(1:10 + rnorm(30,sd=15))),y=(1:10 + rnorm(30,sd=2)))}
     if (input$sample==2) {y<- as.data.frame((matrix(c(sample(1:10,8),sample(30:40,9), sample(80:90,9)), ncol=2, byrow = F)))}
     if (input$sample==3) {theta = runif(50, 0,1)*2*pi
                           x1=c(2*sin(theta[1:30])+rnorm(30,0,0.05),sin(theta[31:50])+rnorm(20,0, 0.01))
                           x2=c(2*cos(theta[1:30])+rnorm(30, 0, 0.05),cos(theta[31:50])+rnorm(20, 0, 0.01))
                           x1 = (x1-mean(x1))/sd(x1)
                           x2 = (x2-mean(x2))/sd(x2)
                           y <- as.data.frame(cbind(x1,x2))}
     if (input$sample==4) {shapes <- mlbench.smiley(n=60, sd1=0.1,sd2=.05)
                          y <- as.data.frame(cbind(shapes$x[,1],shapes$x[,2]))}
     if (input$sample==5) {shapes <- mlbench.spirals(n=60, cycles = 1,sd=.02)
                          y <- as.data.frame(cbind(shapes$x[,1],shapes$x[,2]))}
     if (input$sample==6) {x1 <- cbind(rnorm(6, -0.8, .06), rnorm(6, 1, .06))
                           x2 <- cbind(rnorm(12, 0.8, .16), rnorm(12, 1, .16))
                           y <- rbind(x1,x2)
                           y <- as.data.frame(y)}
     if (input$sample==7) {data("butterfly")
                            y <- as.data.frame(butterfly)}
     if (input$sample==8) { y <- subset(TestData, select = c(x,y))}
     if (input$sample==9) {data("starsCYG")
       y <- as.data.frame(starsCYG)}
      ## Add synt.data from isolation forest 
      colnames(y)<-c("x","y")
      selectedData$df<-y})  
   
  observeEvent(input$scalebutton, {
    selectedData$df <- as.data.frame(scale(selectedData$df,center = TRUE, scale = TRUE))
        })
   
  observeEvent(input$plot_click, {
    res <- nearPoints(selectedData$df, input$plot_click, xvar="x", yvar="y", maxpoints = 1, allRows = TRUE)
    res2 <- nearPoints(selectedData$df, input$plot_click, xvar="x", yvar="y", maxpoints = 1)
    if (length(res2$x)>0) {selectedData$df <- subset(res, selected_==FALSE)
    res2<- 0
   # print ("erase")
    selectedData$df<-selectedData$df[-c(3)]
    row.names(selectedData$df) <- NULL
    } else { 
      #print ("add")
      selectedData$df[nrow(selectedData$df) + 1, ] <- c(input$plot_click$x,input$plot_click$y)}
    })     
    
  observeEvent(input$plot1_brush, {
    res <- brushedPoints(selectedData$df, input$plot1_brush, xvar="x", yvar="y", allRows = TRUE)
    res2 <- brushedPoints(selectedData$df, input$plot1_brush, xvar="x", yvar="y")
    if (length(res2$x)>0) {
      selectedData$df <- subset(res, selected_==FALSE)
      res2<- 0
      selectedData$df<-selectedData$df[-c(3)]
      row.names(selectedData$df) <- NULL}
    })

  ##Distances
  dfdist <-  reactive({
    ##Initial data load
    if (counter==1){selectedData$df<- data.frame(cbind(x=(1:10 + rnorm(30,sd=15))),y=(1:10 + rnorm(30,sd=2)))
    print ("change counter")
    counter <<- 2 }
    #Outlier count
    outliercount <- nrow(selectedData$df) - nrow(selectedData$df)*input$outlierper/100
    if (input$Model==1) {results <- kmeans(selectedData$df, input$clusters)
        centers <<- results$centers[results$cluster, ]
        center2 <- as.data.frame(centers)
        d <- sqrt(rowSums((selectedData$df[,1:2] - centers)^2))
        distance <- as.vector(d)
        temp <- cbind(selectedData$df,distance)
        temp$cluster <- results$cluster
        outlier <- order(temp$distance, decreasing=T)[1:outliercount]
        temp$outlier <- FALSE
        temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==2) {
        working <- selectedData$df
        d <- mahalanobis(working,colMeans(working),cov(working))
                distance <- as.vector(d)
                temp <- cbind(selectedData$df,distance)
                temp$cluster <- 1
                outlier <- order(temp$distance, decreasing=T)[1:outliercount]
                temp$outlier <- FALSE
                temp$outlier[outlier[1:outliercount]] <- TRUE }

    if (input$Model==3) {
      results <- kmeans(selectedData$df, input$clusters)
      centers <<- results$centers[results$cluster, ]
      #center2 <- as.data.frame(centers)
      d <- sqrt(rowSums((selectedData$df[,1:2] - centers)^2))
      m <- tapply(d, results$cluster,mean)
      # divide each distance by the mean for its cluster:
      distance <- d/(m[results$cluster])
      d <- as.numeric(distance)
      distance <- as.vector(d)
      temp <- cbind(selectedData$df,distance)
      temp$cluster <- results$cluster
      outlier <- order(temp$distance, decreasing=T)[1:outliercount]
      temp$outlier <- FALSE
      temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==4) {
        results <- kmeans(selectedData$df, input$clusters)
        centers <<- results$centers[results$cluster, ]
        center2 <- as.data.frame(centers)
        d <- dist(x=selectedData$df[,1:2],y=center2,method = "Minkowski",p=1.5)
        distance <- d[,1]
        temp <- cbind(selectedData$df,distance)
        temp$cluster <- results$cluster
        outlier <- order(temp$distance, decreasing=T)[1:outliercount]
        temp$outlier <- FALSE
        temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==5) {
          d <- dbscan::lof(selectedData$df[,1:2], k=input$clusters)
          d <- as.data.frame(d)
          distance <- d[,1]
          temp <- cbind(selectedData$df,distance)
          temp$cluster <- 1
          outlier <- order(temp$distance, decreasing=T)[1:outliercount]
          temp$outlier <- FALSE
          temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==6) {
          prox <- randomForest(selectedData$df[,1:2], proximity=TRUE)
          d <- outlier(prox)
          d <- as.data.frame(d)
          distance <- d[,1]
          temp <- cbind(selectedData$df,distance)
          temp$cluster <- 1
          outlier <- order(temp$distance, decreasing=T)[1:outliercount]
          temp$outlier <- FALSE
          temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==7) {
          tr<-IsolationTrees(selectedData$df[,1:2], rFactor=0)
          as<-AnomalyScore(selectedData$df[,1:2],tr)
          d <- as.data.frame(as$outF)
          distance <- d[,1]
          temp <- cbind(selectedData$df,distance)
          temp$cluster <- 1
          outlier <- order(temp$distance, decreasing=T)[1:outliercount]
          temp$outlier <- FALSE
          temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==8) {
          nl=3
          unit.type = "tanh"
          Nx.patch=10
          Ny.patch=10
          N.input = Nx.patch*Ny.patch 
          N.hidden = 5*5
          lambda = 0.0002
          beta=6
          rho = 0.01
          epsilon <- 0.001
          max.iterations = 2000
          traind = as.matrix(selectedData$df[,1:2])
          autoencoder.object <- autoencode(X.train=traind,nl=nl,N.hidden=N.hidden,
                                           unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
                                           optim.method="BFGS",max.iterations=max.iterations,
                                           rescale.flag=TRUE,rescaling.offset=0.001)
          scores2 <- predict(autoencoder.object,X.input = traind)
          rajmse<-function(x_hat,x) rowMeans((x_hat-x)^2)
          score3 <- rajmse(selectedData$df[,1:2], scores2$X.output)
          d <- as.data.frame(score3)
          distance <- d[,1]
          temp <- cbind(selectedData$df,distance)
          temp$cluster <- 1
          outlier <- order(temp$distance, decreasing=T)[1:outliercount]
          temp$outlier <- FALSE
          temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==9) {
                results <- FKM.ent.noise(selectedData$df,RS=5,delta=3,k=input$clusters)
                d <- as.data.frame(results$clus)
                distance <- d[,2]
                temp <- cbind(selectedData$df,distance)
                temp$cluster <- d$Cluster
                outlier <- order(temp$distance, decreasing=T)[1:outliercount]
                temp$outlier <- FALSE
                temp$outlier[outlier[1:outliercount]] <- TRUE 
    }
    if (input$Model==10) {
      results <- FKM.gk.noise(selectedData$df,RS=5,delta=3,k=input$clusters)
      d <- as.data.frame(results$clus)
      distance <- d[,2]
      temp <- cbind(selectedData$df,distance)
      temp$cluster <- d$Cluster
      outlier <- order(temp$distance, decreasing=T)[1:outliercount]
      temp$outlier <- FALSE
      temp$outlier[outlier[1:outliercount]] <- TRUE 
    }
    if (input$Model==11) {
      results <- FKM.pf.noise(selectedData$df,stand=1,k=input$clusters)
      d <- as.data.frame(results$clus)
      distance <- d[,2]
      temp <- cbind(selectedData$df,distance)
      temp$cluster <- d$Cluster
      outlier <- order(temp$distance, decreasing=T)[1:outliercount]
      temp$outlier <- FALSE
      temp$outlier[outlier[1:outliercount]] <- TRUE 
    }
    if (input$Model==12) {
      out <- outliers.ranking(selectedData$df, test.data = NULL, method = "sizeDiff",
                              method.pars = NULL,
                              clus = list(dist = "euclidean",alg = "hclust",
                                          meth = "ward.D"),
                              power = 1, verb = F)
      d<- as.data.frame (out$prob.outliers)
      distance <- d[,1]
      temp <- cbind(selectedData$df,distance)
      temp$cluster <- 1
      outlier <- order(temp$distance, decreasing=T)[1:outliercount]
      temp$outlier <- FALSE
      temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==13) {
      results <- FKM.med.noise(selectedData$df,stand=1,k=input$clusters)
      d <- as.data.frame(results$clus)
      distance <- d[,2]
      temp <- cbind(selectedData$df,distance)
      temp$cluster <- d$Cluster
      outlier <- order(temp$distance, decreasing=T)[1:outliercount]
      temp$outlier <- FALSE
      temp$outlier[outlier[1:outliercount]] <- TRUE 
    }
    if (input$Model==14) {
      results <- Func.FBOD(selectedData$df, iter=5, k.nn=input$clusters)
      d<- as.data.frame (results)
      distance <- d[,1]
      temp <- cbind(selectedData$df,distance)
      temp$cluster <- 1
      outlier <- order(temp$distance, decreasing=T)[1:outliercount]
      temp$outlier <- FALSE
      temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==15) {
      results <- Func.SOD(data = selectedData$df, k.nn = input$clusters, k.sel = 5, alpha = 0.8)
      d<- as.data.frame (results)
      distance <- d[,1]
      temp <- cbind(selectedData$df,distance)
      temp$cluster <- 1
      outlier <- order(temp$distance, decreasing=T)[1:outliercount]
      temp$outlier <- FALSE
      temp$outlier[outlier[1:outliercount]] <- TRUE }
    if (input$Model==16) {
      svm.model<-svm(selectedData$df,y=NULL,type='one-classification',
                     nu=0.10,scale=TRUE,kernel="radial")
      outliers.index<-svm.model$index[svm.model$coefs==1.0] 
      temp <- as.data.frame(selectedData$df)
      temp$distance <- 1
      temp$cluster <- 1
      temp$outlier <- FALSE
      print(str(temp))
      print(str(outliers.index))
      if (length(outliers.index) >= 1){
        temp[outliers.index,]$outlier <- TRUE
      }}
    if (input$Model==17) {
      maxs <- apply(selectedData$df, 2, max)
      mins <- apply(selectedData$df, 2, min)
      df <- scale(selectedData$df, center = mins, scale = maxs - mins)
      results <- ml.est(df, max.iter=50)
      distance <- results$tau
      temp <- cbind(selectedData$df,distance)
      temp$cluster <- 1
      outlier <- order(temp$distance, decreasing=T)[1:outliercount]
      temp$outlier <- FALSE
      temp$outlier[outlier[1:outliercount]] <- TRUE }
    temp[,1:4] <-round(temp[,1:4],4)
    temp                 
  })
  
  ##################
  #OUTPUTS
  ##################
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    par(mar = c(5.1, 4.1, 0, 1))
    temp <- dfdist()
    temp2 <- subset(temp,outlier==TRUE)
    plot(temp$x,temp$y,
         col = temp$cluster,
         pch = 20, cex = 3,asp = 1,xlab="",ylab="y"
           )
    if (input$Model==1) {points(centers, pch = 4, cex = 3, lwd = 2, col = temp$cluster)}
    mtext("Outliers are marked with a star, cluster centers with an X, 
          Add or remove points with mouse clicks", 1, line=4, cex=1.3)
    points(temp2$x,temp2$y, pch = 8, cex = 3, lwd = 3, col = temp2$cluster)
    text(temp$x,temp$y, row.names(selectedData$df), cex=0.7, pos=4, col="black")
    
    })
  

   output$info <- renderText({
    #paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  # paste0(clusters()$centers)
   })
   
  output$mytable1 <- DT::renderDataTable({
   dfdist()
  }, options = list(lengthMenu = c(50, 30, 50), pageLength = 50,searching=FALSE))
})