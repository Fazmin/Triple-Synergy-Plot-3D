library("shiny")
library("plot3D")
library(reshape2)
library(lattice)
# library("plot3Drgl")
# library("threejs")
# library(scatterplot3d)
# library("plotly")
library(mgcv)
library(rgl)
library(ggplot2)
library(gridExtra)
library(grid)
# data(mtcars)
source("Make3DPlotFunctions.R")

if (!exists("example_data")) example_data <- matrix(runif(50 * 3), ncol = 3)
shinyServer(function(session,input, output)
{
    values <- reactiveValues(shouldShow=FALSE)

    # observe({
    #     if (input$submitclick>0) {
    #         pasted_data<- read.table(header=TRUE, sep="\t", text=input$datainput, fileEncoding="latin1") 
    #         # writeto.table(pasted_data)
    #     } else {NULL}
    # })

    writeto.table <- function(data){
        output$submitdata_table <- renderTable(data)
    }

    observe({
      if(is.null(input$file2)) return()
      values$shouldShow =FALSE
    })

    observe({
      if (input$clearText_button1>0) 
        # return()
        updateTextInput(session, "datainput", value = "")
    })


    #CheckerBOARD
    observe({
      if (input$clearText_button>0) 
        # return()

        updateTextInput(session, "myData", value = "")
    })


    #LoadSample Data 3D
    observe({
      if (input$load_sample_data_scatter>0) {
        # return()
        samp_data<-"no,x,y,z
1,109,80,71
2,60,79,66
3,3,16,3
4,51,8,117
5,63,112,67
6,14,92,35
7,18,21,82
8,47,50,14
9,116,59,71
10,36,45,39
11,100,80,9
12,75,32,37
13,106,13,94
14,7,89,96
15,76,77,109
16,76,88,65
17,64,5,28
18,17,97,111
19,74,100,59
20,55,83,9
21,70,82,29
22,88,54,10
23,61,56,86
24,109,36,90
25,114,114,38
26,87,31,109
27,82,55,120
28,93,28,102
29,116,44,85
30,74,119,63
31,50,89,36
32,115,74,14
33,96,59,97
34,94,109,26
35,26,70,61
36,81,83,30
37,70,38,83
38,63,6,59
39,100,74,6
40,98,104,2
41,64,51,83
42,107,98,92
43,17,43,73
44,11,45,40
45,5,52,25
46,11,6,81
47,75,113,70
48,7,116,44
49,80,120,113
50,106,85,67"
        updateTextInput(session, "datainput", value = samp_data)
        }
    })


    #LoadSample Data Checker
    observe({
      if (input$load_sample_data_checker>0) {
        # return()
        samp_data<-"0.405,0.513,0.129,0.106,0.107,0.116,0.108,0.118
0.134,0.166,0.125,0.125,0.114,0.122,0.11,0.125
0.135,0.218,0.371,0.425,0.377,0.38,0.339,0.35
0.426,0.566,0.57,0.63,0.483,0.561,0.427,0.498
0.879,0.85,0.767,0.751,0.666,0.635,0.652,0.86
0.945,0.875,0.757,0.889,0.699,0.843,0.723,0.992
0.965,0.92,0.936,0.891,0.882,0.949,0.866,1.013
0.877,0.948,0.894,0.96,0.935,0.948,0.913,1.031"
        updateTextInput(session, "myData", value = samp_data)
        }
    })
    #*******

    output$fileUploaded <- reactive({
      return(is.null(input$file2))
    })
    
    outputOptions(output, 'fileUploaded',suspendWhenHidden=FALSE)

    inFile <- reactive({
        if(input$datainput==""){
          validate(
            need(!is.null(input$file2), '')
            #<-- Please upload your drug-drug combination data or load example data on the left panel
          )
          if(values$shouldShow) return(dfpath)
          if(!is.null(input$file2)) {
            rawIn2 <- input$file2
            if(input$fty=="csv"){
              fileIn <- read.csv(rawIn2[[1,"datapath"]], header=TRUE, sep=",",fileEncoding="latin1")
            } else {
              fileIn <- read.table(rawIn2[[1,"datapath"]], header=TRUE, sep="\t",fileEncoding="latin1") 
            }
            return(fileIn)
            } else {
            return(NULL)
            }
        }   else {
            if(input$fty=="csv"){
              pasted_data<- read.table(header=TRUE, sep=",", text=input$datainput, fileEncoding="latin1")
            } else {
              pasted_data<- read.table(header=TRUE, sep="\t", text=input$datainput, fileEncoding="latin1") 
            }
            return(pasted_data)
        }
    })

    consoleOut<- function(text){
      output$dconsole <- renderPrint({
        print(text)
      })
    } 

    consoleOutraw<- function(text){
      output$dconsole2 <- renderPrint({
        print(text)
      })
    } 

    output$scatterplot1 <- renderPlot({
        data<-as.data.frame(inFile())
        consoleOut(data)


    colna<-colnames(data)
    # consoleOut(data[,colna[4]])
    if(input$xlab==""){ xlab<-colna[2] } else { xlab<-input$xlab }
    if(input$ylab==""){ ylab<-colna[3] } else { ylab<-input$ylab }
    if(input$zlab==""){ zlab<-colna[4] } else { zlab<-input$zlab }

    x<-as.numeric(data[,colna[2]])
    y<-as.numeric(data[,colna[3]])
    z<-as.numeric(data[,colna[4]])
    colvar<-as.integer(data[,colna[1]])
    fulltitle<-paste(paste(xlab,"vs",ylab)," | ",paste(ylab,"vs",zlab)," | ",paste(zlab,"vs",xlab))

    dat1 <- data.frame(plate = colvar, xvar = x, yvar = y, tit=paste(xlab,"vs",ylab))
    plot_right1 <- ggplot(dat1, aes(xvar, yvar, fill=plate), top=tit) + theme(legend.position = "none")  + geom_point() + geom_rug(col="darkred",alpha=0.4) + stat_smooth(colour="#FF337A") + labs(x = ylab,y = xlab)
    # plot_right_d1 <- ggplot(dat1, aes(plate, xvar, fill=plate, color="#800000"), top=tit) + theme(legend.position = "none")  + geom_point() + geom_rug(col="darkred",alpha=0.4) + stat_smooth() + labs(x = "Data row",y = xlab) + geom_bar(colour="red", stat="identity") + guides(fill=FALSE)
    plot_right_d1 <- ggplot(dat1, aes(plate, xvar, fill=plate), top=tit) + theme(legend.position = "none") + labs(x = "Data row",y = xlab) + geom_bar(colour="#FF337A", stat="identity", fill="#FF337A") + guides(fill=FALSE)

    dat2 <- data.frame(plate = colvar, xvar = y, yvar = z,tit=paste(ylab,"vs",zlab))
    plot_right2 <- ggplot(dat2, aes(xvar, yvar, fill=plate), top=tit) + theme(legend.position = "none") + geom_point() + geom_rug(col="darkred",alpha=0.4) + stat_smooth(colour="#2F5EED") + labs(x = zlab,y = ylab)
    # plot_right_d2 <- ggplot(dat2, aes(plate, yvar, fill=plate, color="#800000"), top=tit) + theme(legend.position = "none") + geom_point() + geom_rug(col="darkred",alpha=0.4) + stat_smooth() + labs(x = "Data row",y = ylab)  + geom_bar(colour="blue", stat="identity") + guides(fill=FALSE)
    plot_right_d2 <- ggplot(dat2, aes(plate, yvar, fill=plate), top=tit) + theme(legend.position = "none") + labs(x = "Data row",y = ylab)  + geom_bar(colour="#2F5EED", stat="identity", fill="#2F5EED") + guides(fill=FALSE)

    dat3 <- data.frame(plate = colvar, xvar = z, yvar = x, tit=paste(zlab,"vs",xlab))
    plot_right3 <- ggplot(dat3, aes(xvar, yvar, fill=plate), top=tit) + theme(legend.position = "none") + geom_point() + geom_rug(col="darkred",alpha=0.4) + stat_smooth(colour="#33CC99") + labs(x = xlab,y = zlab)
    # plot_right_d3 <- ggplot(dat3, aes(plate, xvar, fill=plate, color="#800000"), top=tit) + theme(legend.position = "none") + geom_point() + geom_rug(col="darkred",alpha=0.4) + stat_smooth() + labs(x = "Data row",y = zlab) + geom_bar(colour="green", stat="identity") + guides(fill=FALSE)
    plot_right_d3 <- ggplot(dat3, aes(plate, xvar, fill=plate), top=tit) + theme(legend.position = "none") + labs(x = "Data row",y = zlab) + geom_bar(colour="#33CC99", stat="identity", fill="#33CC99") + guides(fill=FALSE)
    if(input$showotherplots==1){
        grid.arrange(plot_right1, plot_right2, plot_right3, plot_right_d1, plot_right_d2, plot_right_d3, ncol=3, nrow=2, top=textGrob(fulltitle, gp=gpar(fontsize=14,font=8)))
    }

    })
    
    output$scatterplot2 <- renderPlot({

        data<-as.data.frame(inFile())
        consoleOut(data)
        output$data_table <- renderTable(data)


    colna<-colnames(data)
    # consoleOut(data[,colna[4]])
    if(input$xlab==""){ xlab<-colna[2] } else { xlab<-input$xlab }
    if(input$ylab==""){ ylab<-colna[3] } else { ylab<-input$ylab }
    if(input$zlab==""){ zlab<-colna[4] } else { zlab<-input$zlab }

    x<-as.numeric(data[,colna[2]])
    # y<-rev(range(as.numeric(data[,colna[3]])))
    y<-as.numeric(data[,colna[3]])
    z<-as.numeric(data[,colna[4]])
    colvar<-as.integer(data[,colna[1]])
    xticks<-seq(0,max(x),by=0.5)
    yticks<-seq(0,max(y),by=0.5)
    zticks<-seq(0,max(z),by=0.5)

    panelfirst <- function(pmat) {
        #LINE 1 **************
        XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
        scatter2D(XY$x, XY$y, colvar = colvar, pch = 20, 
              cex = input$aixdotsize, add = TRUE, colkey = FALSE, col="#FF337A")
        if(input$showlines==1){
        lo <- smooth.spline(XY$x, XY$y, spar=input$linesmooth)
        lines(lo, colvar = colvar, lwd = 3, add = TRUE, colkey = FALSE, col="#FF337A")
        
        XYx <- trans3D(x = c(min(x), min(x)), y = c(max(y), min(y)), z = c(min(z), max(z)), pmat = pmat) 
        lines(XYx, lwd = 1, col = "black",lty = 3)
        }

        
        #LINE 2 **************
        XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
        scatter2D(XY$x, XY$y, colvar = colvar, pch = 20, 
              cex = input$aixdotsize, add = TRUE, colkey = FALSE, col="#2F5EED") 
        if(input$showlines==1){
        lo <- smooth.spline(XY$x, XY$y, spar=input$linesmooth)
        lines(lo, colvar = colvar, lwd = 3, add = TRUE, colkey = FALSE, col="#2F5EED")
        XYx <- trans3D(x = c(max(x), min(x)), y = c(min(y), min(y)), z = c(min(z), max(z)), pmat = pmat) 
        lines(XYx, lwd = 1, col = "black",lty = 3)
        }


        #LINE 3 **************
        XY <- trans3D(x, y = rep(min(y), length(y)), z, pmat = pmat)
        scatter2D(XY$x, XY$y, colvar = colvar, pch = 20, 
              cex = input$aixdotsize, add = TRUE, colkey = FALSE, col="#33CC99") 
        if(input$showlines==1){
        lo <- smooth.spline(XY$x, XY$y, spar=input$linesmooth)
        lines(lo, colvar = colvar, lwd = 3, add = TRUE, colkey = FALSE, col="#33CC99")
        XYx <- trans3D(x = c(min(x),max(x)), y = c(max(y), min(y)), z = c(min(z), min(z)), pmat = pmat) 
        lines(XYx, lwd = 1, col = "black",lty = 3)
        }
        }

        # consoleOut(yticks)

# mar: A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.

# WORKING Vertion using PLOT3D
    # plot_Main<-scatter3D(x = as.numeric(data[,colna[2]]), y = as.numeric(data[,colna[3]]), z = as.numeric(data[,colna[4]]), pch = 19, bty="b2", theta = input$theta, phi = input$phi, colvar = as.integer(data[,colna[1]]), ticktype = "detailed", cex=input$dotsize,xlab = xlab, ylab = ylab, zlab = zlab, clab = "", main = input$ptitle, expand  =input$yexpand , axes=FALSE, col.grid = "gray90",colkey = list(side = 1, length = 0.5, addlines = TRUE), panel.first=panelfirst) + text3D(x, y, z, labels = paste(x,"|",y,"|",z), add = TRUE, colkey = FALSE, cex = 1.1, mar=c(1, 3, 1, 3)) + axes3d(c('x--','z')) + axis3d(edge= 'x--', at=yticks, labels = yticks )

    if(input$axisticks==0){
        aticks<-"simple"
    } else {
        aticks<-"detailed"
    }
    if(input$showovlines==0){
        type<-NULL
    } else {
        type<-"h"
    }
    consoleOut(summary(data))
    if(input$showcolkey==1){
        plot_Main<-scatter3D(x = as.numeric(data[,colna[2]]), y = as.numeric(data[,colna[3]]), z = as.numeric(data[,colna[4]]), pch = 19, cex=input$dotsize, bty=input$btytype, theta = input$theta, phi = input$phi, ticktype = aticks, nticks=12, xlab = xlab, ylab = ylab, zlab = zlab, clab = "", main = input$ptitle, cex.main=16, expand  =input$yexpand , axes=TRUE, col.grid = "gray90", colkey = list(side = 1, length = 0.5, addlines = TRUE, cex.clab = 0.75), panel.first=panelfirst, mar=c(0,0,0,0),type =type)
        if(input$axisticks==3){
            text3D(x = xticks, y = rep(0,length(xticks)), z = rep(0,length(xticks)), labels = xticks, add = TRUE, adj = 0)
            text3D(x = rep(0,length(yticks)), y = yticks, z = rep(0,length(yticks)), labels = yticks, add = TRUE, adj = 0)
            text3D(x = rep(0,length(zticks)), y = rep(0,length(zticks)), z = zticks, labels = zticks, add = TRUE, adj = 0)
        }
    } else {
        plot_Main<-scatter3D(x = as.numeric(data[,colna[2]]), y = as.numeric(data[,colna[3]]), z = as.numeric(data[,colna[4]]), pch = 19, cex=input$dotsize, bty=input$btytype, theta = input$theta, phi = input$phi, ticktype = aticks, nticks=12, xlab = xlab, ylab = ylab, zlab = zlab, clab = "", main = input$ptitle, cex.main=2,expand =input$yexpand , axes=TRUE, col.grid = "gray90", colkey = FALSE, panel.first=panelfirst, mar=c(0,0,0,0),type =type) 
        if(input$axisticks==3){
            text3D(x = xticks, y = rep(0,length(xticks)), z = rep(0,length(xticks)), labels = xticks, add = TRUE, adj = 0)
            text3D(x = rep(0,length(yticks)), y = yticks, z = rep(0,length(yticks)), labels = yticks, add = TRUE, adj = 0)
            text3D(x = rep(0,length(zticks)), y = rep(0,length(zticks)), z = zticks, labels = zticks, add = TRUE, adj = 0)
        }
    } 

    plotdownload3d <- function(){
    if(input$axisticks==0){
        aticks<-"simple"
    } else {
        aticks<-"detailed"
    }
    # consoleOut(summary(data))
    if(input$showcolkey==1){
        plot_Main<-scatter3D(x = as.numeric(data[,colna[2]]), y = as.numeric(data[,colna[3]]), z = as.numeric(data[,colna[4]]), pch = 19, cex=input$dotsize, bty=input$btytype, theta = input$theta, phi = input$phi, ticktype = aticks, nticks=12, xlab = xlab, ylab = ylab, zlab = zlab, clab = "", main = input$ptitle, cex.main=16, expand  =input$yexpand , axes=TRUE, col.grid = "gray90", colkey = list(side = 1, length = 0.5, addlines = TRUE, cex.clab = 0.75), panel.first=panelfirst, mar=c(0,0,0,0),type ="h")
        if(input$axisticks==3){
            text3D(x = xticks, y = rep(0,length(xticks)), z = rep(0,length(xticks)), labels = xticks, add = TRUE, adj = 0)
            text3D(x = rep(0,length(yticks)), y = yticks, z = rep(0,length(yticks)), labels = yticks, add = TRUE, adj = 0)
            text3D(x = rep(0,length(zticks)), y = rep(0,length(zticks)), z = zticks, labels = zticks, add = TRUE, adj = 0)
        }
    } else {
        plot_Main<-scatter3D(x = as.numeric(data[,colna[2]]), y = as.numeric(data[,colna[3]]), z = as.numeric(data[,colna[4]]), pch = 19, cex=input$dotsize, bty=input$btytype, theta = input$theta, phi = input$phi, ticktype = aticks, nticks=12, xlab = xlab, ylab = ylab, zlab = zlab, clab = "", main = input$ptitle, cex.main=2,expand  =input$yexpand , axes=TRUE, col.grid = "gray90", colkey = FALSE, panel.first=panelfirst, mar=c(0,0,0,0),type ="h") 
        if(input$axisticks==3){
            text3D(x = xticks, y = rep(0,length(xticks)), z = rep(0,length(xticks)), labels = xticks, add = TRUE, adj = 0)
            text3D(x = rep(0,length(yticks)), y = yticks, z = rep(0,length(yticks)), labels = yticks, add = TRUE, adj = 0)
            text3D(x = rep(0,length(zticks)), y = rep(0,length(zticks)), z = zticks, labels = zticks, add = TRUE, adj = 0)
        }
    } 
    }

    # Add legend
    # colkey = list(side = 1, length = 0.5, addlines = TRUE, cex.clab = 0.75)

    # ad 3d line 
    # type ="h"

    # Use text3D to label x axis
    # text3D(x = xticks, y = xticks, z = zticks, labels = xticks, add = TRUE, adj = 0)
    # Use text3D to label y axis
    # text3D(x = xticks,   y = xticks, z = zticks, labels  = xticks, add = TRUE, adj = 1)

    # color veriable if needed -  colvar = as.integer(data[,colna[1]])

    if(input$pointtext==1){
        text3D(x, y, z, labels = paste(x,"|",y,"|",z), add = TRUE, colkey = FALSE, cex = 1.1, mar=c(1, 3, 1, 3))
    }
    plot_Main
    })

    # CHECKERBOARD *** Read in data matrix ***
    dataM <- reactive({
      if (input$dataInput == 1) {
        if (input$sampleData == 1) {
          data <- read.table("testData3.tab", sep = "\t", header = FALSE)
        } else {
          data <- read.table("testData.tab", sep = "\t", header = TRUE, row.names = 1)
        }
      } else if (input$dataInput == 2) {
        inFile <- input$upload
        # Avoid error message while file is not uploaded yet
        if (is.null(input$upload)) {
          return(NULL)
        }
        # Get the separator
        mySep <- switch(input$fileSepDF, `1` = ",", `2` = "\t", `3` = ";", `4` = "")  #list('Comma'=1,'Tab'=2,'Semicolon'=3)
        if (file.info(inFile$datapath)$size <= 5245800) {
          if (input$fileHeader) {
            data <- read.table(inFile$datapath, sep = mySep, header = TRUE, row.names = 1, 
              fill = TRUE)
          } else {
            data <- read.table(inFile$datapath, sep = mySep, header = FALSE, 
              fill = TRUE)
          }
        } else print("5MB upload limit.")
      } else {
        # To be looked into again - for special case when last column has empty entries
        # in some rows
        if (is.null(input$myData)) {
          return(NULL)
        }
        # tmp<-matrix(strsplit(input$myData, '\n')[[1]])
        mySep <- switch(input$fileSepP, `1` = ",", `2` = "\t", `3` = ";")
        # myColnames<-strsplit(tmp[1], mySep)[[1]] data<-matrix(0, length(tmp)-1,
        # length(myColnames)) colnames(data)<-myColnames
        # rownames(data)<-myColnames[1:nrow(data)] for(i in 2:length(tmp)){
        # myRow<-as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=''), mySep)[[1]])
        # data[i-1,]<-myRow[-length(myRow)] }
        data <- read.table(header = TRUE, sep = mySep, text = input$myData, fileEncoding = "latin1")
        data <- data.frame(data)

        # data <- data.frame(data)
      }
      # consoleOutraw(data)
      return(data)
    })
    # ************
    # *** The plot dimensions ***
    heightSize <- reactive({
      input$myHeight
    })
    widthSize <- reactive({
      input$myWidth
    })
    ## *** Data in table ***
    output$filetable <- renderTable({
    if(input$xyzcolmdata){
        # data<-acast(data, data[,1]~data[,2], value.var="z")
        xx<-dataM()
        oo<-xtabs(xx[,3]~xx[,1]+xx[,2], data=xx)
        colnum<-ncol(oo)
        # consoleOutraw(xx)
        if(input$smoothmatrix){
            if(input$graphori==1){
                xx<-gam(xx[,3] ~ te(xx[,1], xx[,2]), data = xx)
            } else if (input$graphori==2){
                xx<-gam(xx[,2] ~ te(xx[,3], xx[,1]), data = xx)
            } else if (input$graphori==3){
                xx<-gam(xx[,1] ~ te(xx[,2], xx[,3]), data = xx)
            } else {
                xx<-gam(xx[,3] ~ te(xx[,2], xx[,1]), data = xx)
            }
            xx<-matrix(fitted(xx), ncol = colnum)
        } else {
            if(input$graphori==1){
                xx<-xtabs(xx[,3]~xx[,1]+xx[,2], data=xx)
            } else if (input$graphori==2){
                xx<-xtabs(xx[,2]~xx[,3]+xx[,1], data=xx)
            } else if (input$graphori==3){
                xx<-xtabs(xx[,1]~xx[,2]+xx[,3], data=xx)
            } else {
                xx<-xtabs(xx[,3]~xx[,2]+xx[,1], data=xx)
            }
        }
        consoleOutraw(xx)
        return(xx)
        # data<-data.frame(datax)
    } else {
      print(nrow(dataM()))
      if (nrow(dataM()) < 500) {
        return(dataM())
      } else {
        return(dataM()[1:100, ])
      }
    }
    })



    # *** Generate the box plot ***
    generateCheckerboardPlot <- function(xx, ...) {
      
      showplot <- TRUE
      if (length(list(...))) {
        Lst <- list(...)
        if (!is.null(Lst$plot)) {
          showplot <- Lst$plot
        }
      }
      
      if (input$inverseData == TRUE) {
        xx <- create_inverse(xx)
      }
      
      if (input$flipDataX == TRUE) {
        xx <- create_x_flip(xx)
      }
      if (input$flipDataY == TRUE) {
        xx <- create_y_flip(xx)
      }
      colna<-colnames(xx)
        if(input$labelsTitle==TRUE){ 
            xlab<-input$myXlab
            ylab<-input$myYlab
            zlab<-input$myZlab

        } else { 
            xlab<-colna[1]
            ylab<-colna[2]
            zlab<-colna[3]
        }
        if(input$xyzcolmdata){
            # data<-acast(data, data[,1]~data[,2], value.var="z")
           oo<-xtabs(xx[,3]~xx[,1]+xx[,2], data=xx)
            colnum<-ncol(oo)
            # consoleOutraw(xx)
            if(input$smoothmatrix){
                if(input$graphori==1){
                    xlab<-colna[1]
                    ylab<-colna[2]
                    zlab<-colna[3]
                    xx<-gam(xx[,3] ~ te(xx[,1], xx[,2], fx = TRUE), data = xx)
                } else if (input$graphori==2){
                    xlab<-colna[1]
                    ylab<-colna[2]
                    zlab<-colna[3]
                    xx<-gam(xx[,2] ~ te(xx[,3], xx[,1], fx = TRUE), data = xx)
                } else {
                    xlab<-colna[1]
                    ylab<-colna[2]
                    zlab<-colna[3]
                    xx<-gam(xx[,1] ~ te(xx[,2], xx[,3], fx = TRUE), data = xx)
                }
                xx<-matrix(fitted(xx), ncol = colnum)
            } else {
                if(input$graphori==1){
                    xlab<-colna[1]
                    ylab<-colna[2]
                    zlab<-colna[3]
                    xx<-xtabs(xx[,3]~xx[,1]+xx[,2], data=xx)
                } else if (input$graphori==2){
                    zlab<-colna[1]
                    xlab<-colna[2]
                    ylab<-colna[3]
                    xx<-xtabs(xx[,2]~xx[,3]+xx[,1], data=xx)
                } else {
                    ylab<-colna[1]
                    zlab<-colna[2]
                    xlab<-colna[3]
                    xx<-xtabs(xx[,1]~xx[,2]+xx[,3], data=xx)
                }
            }
            consoleOutraw(xx)
            # return(xx)
            # consoleOutraw(xx)
            # data<-data.frame(datax)
        }
      # matrixdata<-acast(xx, xx[1,]~xx[2,], value.var=colna[3])
        # consoleOutraw(matrixdata)
      
      par(mar = c(2.1, 2.1, 4.1, 2.1))
      # *** Generate xyz raw data plot ***
        if (input$plotType == "0") {
            if (showplot == TRUE) {
              raw_plot(xx, xlab, ylab, zlab, input$myTitle, 
                input$obs, input$obsv, input$cexTitle, input$cexAxislabel, input$cexAxis, input$btytype2, input$colkey2, input$singlec,input$inity,input$plottrans )
            } else {
              my.stat <- data.frame(stats = "NA")
            }
        } else if(input$plotType == "2"){
            if (showplot == TRUE) {
              raw_plot_hist(xx, xlab, ylab, zlab, input$myTitle, 
                input$obs, input$obsv, input$cexTitle, input$cexAxislabel, input$cexAxis, input$btytype2, input$colkey2, input$singlec,input$inity,input$plottrans )
            } else {
              my.stat <- data.frame(stats = "NA")
            }
        } else if(input$plotType == "3"){
            if (showplot == TRUE) {
              raw_plot_ribbon(xx, xlab, ylab, zlab, input$myTitle, 
                input$obs, input$obsv, input$cexTitle, input$cexAxislabel, input$cexAxis, input$btytype2, input$colkey2, input$singlec,input$inity,input$plottrans )
            } else {
              my.stat <- data.frame(stats = "NA")
            }
        } else if(input$plotType == "4"){
            if (showplot == TRUE) {
              raw_plot_mesh(xx, xlab, ylab, zlab, input$myTitle, 
                input$obs, input$obsv, input$cexTitle, input$cexAxislabel, input$cexAxis, input$btytype2, input$colkey2, input$singlec,input$inity,input$plottrans,input$meshspace )
            } else {
              my.stat <- data.frame(stats = "NA")
            }
        } else if(input$plotType == "5"){
            if (showplot == TRUE) {
              raw_plot_2dc(xx, xlab, ylab, zlab, input$myTitle, input$obs, input$obsv, input$cexTitle, input$cexAxislabel, input$cexAxis, input$btytype2, input$colkey2, input$singlec,input$inity,input$plottrans,input$meshspace )
            } else {
              my.stat <- data.frame(stats = "NA")
            }
        } else {
        # *** Generate synergy stats ***
        max_tmp <- (1 - xx/max(xx, na.rm = TRUE))
        max_tmp_x <- which.max(max_tmp[1, ])
        max_tmp_y <- which.max(max_tmp[, 1])
        max_real <- max(max_tmp)
        max_real_exp <- max_tmp[max_tmp_y, max_tmp_x]

        if (input$otherPlotType == 0) {
          # Bliss
          bliss <- bliss_calculus(xx)
          reverse <- 1
          if (max(bliss) < abs(min(bliss))) {
            reverse <- -1
            max_bliss <- find_xy(abs(bliss))
          } else {
            max_bliss <- find_xy(bliss)
          }
          
          if (showplot == TRUE) {
            if (input$myOrientation == 1) {
              # address Antagonism
              bliss.show <- bliss
              bliss.show[bliss.show > 0] <- 0
              myImagePlotReverse(abs(bliss.show), reverse = -1, xLab = input$myXlab, 
              yLab = input$myYlab, title = input$myTitle, cTitle = "Bliss Antagonism", 
              cex.T = input$cexTitle, cex.L = input$cexAxislabel, cex.A = input$cexAxis)  # =-1 
            } else {
              bliss.show <- bliss
              bliss.show[bliss.show < 0] <- 0
              myImagePlotReverse(bliss.show, reverse, xLab = input$myXlab, yLab = input$myYlab, 
              title = input$myTitle, cTitle = "Bliss Synergy", cex.T = input$cexTitle, 
              cex.L = input$cexAxislabel, cex.A = input$cexAxis)
            }
          }
          my.bliss <- max_tmp[max_bliss$y, max_bliss$x] - (max_tmp[1, max_bliss$x] + 
            max_tmp[max_bliss$y, 1] - (max_tmp[1, max_bliss$x] * max_tmp[max_bliss$y, 
            1]))
          my.stat <- data.frame(BLISS = my.bliss, Idx_A = max_bliss$x, Idx_B = max_bliss$y, 
            value_A = max_tmp[1, max_bliss$x], value_B = max_tmp[max_bliss$y, 
              1], value_AB = max_tmp[max_bliss$y, max_bliss$x])
        }
        if (input$otherPlotType == 1) {
          # HSA
          reverse <- 1
          hsa <- hsa_calculus(xx)
          if (max(hsa) < abs(min(hsa))) {
            reverse <- -1
            max_hsa <- find_xy(abs(hsa))
          } else {
            max_hsa <- find_xy(hsa)
          }
          # myImagePlot(hsa)
          if (showplot == TRUE) {
            
            if (input$myOrientation == 1) {
              # address Antagonism
              hsa.show <- hsa
              hsa.show[hsa.show > 0] <- 0
              myImagePlotReverse(abs(hsa.show), reverse = -1, xLab = input$myXlab, 
              yLab = input$myYlab, title = input$myTitle, cTitle = "HSA Antagonism", 
              cex.T = input$cexTitle, cex.L = input$cexAxislabel, cex.A = input$cexAxis)
            } else {
              # synergism
              hsa.show <- hsa
              hsa.show[hsa.show < 0] <- 0
              myImagePlotReverse(hsa.show, reverse, xLab = input$myXlab, yLab = input$myYlab, 
              title = input$myTitle, cTitle = "HSA Synergy", cex.T = input$cexTitle, 
              cex.L = input$cexAxislabel, cex.A = input$cexAxis)
            }
          }
          my.hsa <- max_tmp[max_hsa$y, max_hsa$x] - (max_tmp[1, max_hsa$x] + max_tmp[max_hsa$y, 
            1] - (max_tmp[1, max_hsa$x] * max_tmp[max_hsa$y, 1]))
          my.stat <- data.frame(HSA = my.hsa, Idx_A = max_hsa$x, Idx_B = max_hsa$y, 
            value_A = max_tmp[1, max_hsa$x], value_B = max_tmp[max_hsa$y, 1], 
            value_AB = max_tmp[max_hsa$y, max_hsa$x])
        }
        if (input$otherPlotType == 2) {
          # raw
          max_inh <- find_xy(max_tmp)
          my.stat <- data.frame(Max_Inhibition_AB = max_real, Idx_A = max_inh$x, 
            Idx_B = max_inh$y, Max_Inhibition_A = max_tmp[1, max_tmp_x], Max_Inhibition_B = max_tmp[max_tmp_y, 
              1], Max_Inhibition_Expected_AB = max_real_exp, Idx_Exp_A = max_tmp_x, 
            Idx_Exp_B = max_tmp_y)
          if (showplot == TRUE) {
            if (input$myOrientation == 1) {
              myImagePlotReverse(xx, reverse = -1, xLab = input$myXlab, yLab = input$myYlab, 
              title = input$myTitle, cTitle = input$myZlab, cex.T = input$cexTitle, 
              cex.L = input$cexAxislabel, cex.A = input$cexAxis)
            } else {
              myImagePlotReverse(xx, reverse = 1, xLab = input$myXlab, yLab = input$myYlab, 
              title = input$myTitle, cTitle = input$myZlab, cex.T = input$cexTitle, 
              cex.L = input$cexAxislabel, cex.A = input$cexAxis)
            }
          }
        }

        }
        if (showplot == FALSE) {
        df.basics <- data.frame(ConditionA = input$myXlab, ConditionB = input$myYlab, 
          Observation = input$myZlab)
        df.settings <- data.frame(InverseDataMatrix = input$inverseData, FlipDataMatrixOnX = input$flipDataX, 
          FlipDataMatrixOnY = input$flipDataY)  # input$myOrientation,input$otherPlotType
        tmp <- cbind(df.basics, df.settings, my.stat)
        rownames(tmp) <- c("Values")
        return(t(tmp))
        }
      
    }

    output$rawPlot <- renderPlot({
      
      # generate and plot checkerboard data

      generateCheckerboardPlot(dataM())
      
    }, height = heightSize, width = widthSize)

    # *** Output checkerboard data in table below plot ***
    # output$checkerboardStatsTable <- renderTable({
    #   M <- generateCheckerboardPlot(dataM(), plot = FALSE)
    #   M
    # })

    #3D Checkerboard Download
    ## *** Download EPS file ***
    output$downloadPlotEPS1 <- downloadHandler(filename <- function() {
      paste("3DCheckerboardPlot.eps")
    }, content <- function(file) {
      postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special", width = input$myWidth/72, 
        height = input$myHeight/72)
      ## ---------------
      plotdownload3d()
      ## ---------------
      dev.off()
    }, contentType = "application/postscript")
    ## *** Download PDF file ***
    output$downloadPlotPDF1 <- downloadHandler(filename <- function() {
      paste("3DCheckerboardPlot.pdf")
    }, content <- function(file) {
      pdf(file, width = 10, height = 6)
      ## ---------------
      plotdownload3d()
      ## ---------------
      dev.off()
    }, contentType = "application/pdf"  # MIME type of the image
    )
    ## *** Download SVG file ***
    output$downloadPlotSVG1 <- downloadHandler(filename <- function() {
      paste("3DCheckerboardPlot.svg")
    }, content <- function(file) {
      svg(file, width = input$myWidth/72, height = input$myHeight/72)
      ## ---------------
      plotdownload3d()
      ## ---------------
      dev.off()
    }, contentType = "image/svg")


    ## *** Download EPS file ***
    output$downloadPlotEPS <- downloadHandler(filename <- function() {
      paste("Checkerboard.eps")
    }, content <- function(file) {
      postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special", width = input$myWidth/72, 
        height = input$myHeight/72)
      ## ---------------
      generateCheckerboardPlot(dataM())
      ## ---------------
      dev.off()
    }, contentType = "application/postscript")
    ## *** Download PDF file ***
    output$downloadPlotPDF <- downloadHandler(filename <- function() {
      paste("Checkerboard.pdf")
    }, content <- function(file) {
      pdf(file, width = input$myWidth/72, height = input$myHeight/72)
      ## ---------------
      generateCheckerboardPlot(dataM())
      ## ---------------
      dev.off()
    }, contentType = "application/pdf"  # MIME type of the image
    )
    ## *** Download SVG file ***
    output$downloadPlotSVG <- downloadHandler(filename <- function() {
      paste("Checkerboard.svg")
    }, content <- function(file) {
      svg(file, width = input$myWidth/72, height = input$myHeight/72)
      ## ---------------
      generateCheckerboardPlot(dataM())
      ## ---------------
      dev.off()
    }, contentType = "image/svg")


})
