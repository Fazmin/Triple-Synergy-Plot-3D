#
# R functions for Checkerboard shiny
#
# author: jw
# created: 14th March 2014
# TODO(jw): add loewe additivity, allow input color range

myImagePlotReverse <- function(x, ...) {
  # Produce raw, synergy antagonism heatmap from plate data
  #
  # Args:
  #   x: data matrix from input 
  #   reverse: sets synergy 1 (red range) or antagonism -1 (blue range)
  #   xLab: x axis label
  #   yLab: y axis label
  #   title: main title 
  #   cTitle: color bar title
  #   cex.T: size main title
  #   cex.L: size labels
  #   cex.A: size tick labels
  # Returns:
  # 
  
  # max preset for color bar number range is -1 to 1
  # TODO(jw): make color bar range input shiny variable
  cm_max <- 0.7
  cm_min <- 0.1
  reverse <- 1
  xLab <- ""
  yLab <- ""
  
  min <- min(x,na.rm=TRUE)
  max <- max(x,na.rm=TRUE)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$cm_max) ){
      cm_max <- c(Lst$cm_max)
    }
    if( !is.null(Lst$ycm_min) ){
      cm_min <- c(Lst$ycm_min)
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$yLab) ){
      yLab <- c(Lst$yLab)
    }
    if( !is.null(Lst$xLab) ){
      xLab <- c(Lst$xLab)
    }
    if( !is.null(Lst$cTitle) ){
      cTitle <- c(Lst$cTitle)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
    if( !is.null(Lst$reverse) ){
      reverse <- Lst$reverse
    }
    if( !is.null(Lst$cex.T) ){
      cex.T <- Lst$cex.T*.1
    }
    if( !is.null(Lst$cex.L) ){
      cex.L <- Lst$cex.L*.1
    }
    if( !is.null(Lst$cex.A) ){
      cex.A <- Lst$cex.A*.1
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  # adjust for no synergy
  # keep real max for future reference 
  max_impact <- max
  print(max)
  if( is.null(max) | is.na(max) ) { max <- cm_max }
  if( is.null(min) | is.na(max) ) { min <- cm_min }
  
  if(max < cm_max) { max <- cm_max }
  if(min < cm_min) { 
    x[x < cm_min] <- cm_min
    min <- cm_min
  }
  
  layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(1,1), heights=c(4,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  ColorRamp <- rgb( seq(1,0,length=256),  # Red
                    seq(1,0,length=256),  # Green
                    seq(0,1,length=256))  # Blue
  
  # fancy new colors
  library(RColorBrewer)
  ColorRamp <- colorRampPalette(brewer.pal(9, "YlOrRd"))(128)
  if (reverse == -1) {
    ColorRamp <- colorRampPalette(brewer.pal(9, "YlGnBu"))(128)
  }
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(4,6,3.5,4),cex.lab=cex.L,cex.main=cex.T)
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab=xLab,
        ylab=yLab, axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=cex.A)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,cex.axis=cex.A)
  
  # Color Scale
  par(mar = c(4,6,2.5,4))
  image( ColorLevels,1,
         matrix(data=ColorLevels, nrow=length(ColorLevels),ncol=1),
         col=ColorRamp,
         xlab=cTitle,ylab="",
         yaxt="n",cex.axis=cex.A)
  
  layout(1)
  return(max_impact)
}


raw_plot <- function(xx,yl,xl,zl,title,theta,ltheta,cex.T,cex.L,cex.A,bty,colkey,scol,inity,trans) {
  # Produce 3D Checkerboard plot for raw data
  #
  # Args:
  #   xx: data matrix from input
  #   yl: y axis label
  #   xl: x axis label
  #   zl: z axis labe
  #   title: main title
  #   theta: plot angle theta
  #   ltheta: plot angel ltheta
  #   cex.T: size main title
  #   cex.L: size labels
  #   cex.A: size axis ... not relevant here :)
  # Returns:
  #   
  
  collut <- c()
  z <- as.matrix(xx)
#  z <- z-min(z)+1 # added for mammalian cell lines where smallest number is not nessesarily ~ 0
  z <- z/max(z)
  z_tmp <- array(z)
  x<-seq(1:nrow(z))
  y<-seq(1:ncol(z))
#  x<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))
#  y<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))

#  collut <- colors()[542:557]
#  collut[1:10] <- colors()[646:655]
#  collut[11:20] <-colors()[589:598]
#  collut[21:100] <-colors()[262:341]
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
#  ColorLevels <- seq(min(z_tmp), max(z_tmp), length=length(ColorRamp))
  ex <- length(ColorRamp)/max(z_tmp)
  temp <- round(ex * z)
  #temp <- 1 + 100*(z-min(z)) / length(ColorRamp)
  #temp <- 1 + 100*(z_tmp-min(z_tmp)) / diff(range(z_tmp))
  if(scol==TRUE){
    icol<-"#0072B2"
  } else {
    icol<-NULL
  }
  mapcol <- ColorRamp[temp]
  mapcol2 <- matrix(mapcol,nrow=nrow(z), byrow=FALSE)
  par(cex.axis=cex.A*.1,cex.lab=cex.L*.1,cex.main=cex.T*.1)

        if(colkey==0){
           persp3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,bty=bty,colkey = FALSE, col = icol, inttype = inity,alpha = trans)
        } else {
          persp3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,colkey = list(side = 1, length = 0.5, addlines = TRUE, cex.clab = 0.75), bty=bty, col = icol, inttype = inity,alpha = trans)

        }

  # persp(x,y,z, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, col=mapcol2[2:nrow(z),2:ncol(z)], ltheta=-60,border=0.1,nticks=8)
 
}

raw_plot_hist <- function(xx,yl,xl,zl,title,theta,ltheta,cex.T,cex.L,cex.A,bty,colkey,scol,inity,trans) {
  # Produce 3D Checkerboard plot for raw data
  #
  # Args:
  #   xx: data matrix from input
  #   yl: y axis label
  #   xl: x axis label
  #   zl: z axis labe
  #   title: main title
  #   theta: plot angle theta
  #   ltheta: plot angel ltheta
  #   cex.T: size main title
  #   cex.L: size labels
  #   cex.A: size axis ... not relevant here :)
  # Returns:
  #   
  
  collut <- c()
  z <- as.matrix(xx)
#  z <- z-min(z)+1 # added for mammalian cell lines where smallest number is not nessesarily ~ 0
  z <- z/max(z)
  z_tmp <- array(z)
  x<-seq(1:nrow(z))
  y<-seq(1:ncol(z))
#  x<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))
#  y<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))

#  collut <- colors()[542:557]
#  collut[1:10] <- colors()[646:655]
#  collut[11:20] <-colors()[589:598]
#  collut[21:100] <-colors()[262:341]
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
#  ColorLevels <- seq(min(z_tmp), max(z_tmp), length=length(ColorRamp))
  ex <- length(ColorRamp)/max(z_tmp)
  temp <- round(ex * z)
  #temp <- 1 + 100*(z-min(z)) / length(ColorRamp)
  #temp <- 1 + 100*(z_tmp-min(z_tmp)) / diff(range(z_tmp))
  if(scol==TRUE){
    icol<-"#0072B2"
  } else {
    icol<-NULL
  }
  mapcol <- ColorRamp[temp]
  mapcol2 <- matrix(mapcol,nrow=nrow(z), byrow=FALSE)
  par(cex.axis=cex.A*.1,cex.lab=cex.L*.1,cex.main=cex.T*.1)

        if(colkey==0){
           hist3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,bty=bty,colkey = FALSE, col = icol, inttype = inity,alpha = trans)
        } else {
          hist3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,colkey = list(side = 1, length = 0.5, addlines = TRUE, cex.clab = 0.75), bty=bty, col = icol, inttype = inity,alpha = trans)
        }

  # persp(x,y,z, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, col=mapcol2[2:nrow(z),2:ncol(z)], ltheta=-60,border=0.1,nticks=8)
 
}

raw_plot_ribbon <- function(xx,yl,xl,zl,title,theta,ltheta,cex.T,cex.L,cex.A,bty,colkey,scol,inity,trans) {
  # Produce 3D Checkerboard plot for raw data
  #
  # Args:
  #   xx: data matrix from input
  #   yl: y axis label
  #   xl: x axis label
  #   zl: z axis labe
  #   title: main title
  #   theta: plot angle theta
  #   ltheta: plot angel ltheta
  #   cex.T: size main title
  #   cex.L: size labels
  #   cex.A: size axis ... not relevant here :)
  # Returns:
  #   
  
  collut <- c()
  z <- as.matrix(xx)
#  z <- z-min(z)+1 # added for mammalian cell lines where smallest number is not nessesarily ~ 0
  z <- z/max(z)
  z_tmp <- array(z)
  x<-seq(1:nrow(z))
  y<-seq(1:ncol(z))
#  x<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))
#  y<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))

#  collut <- colors()[542:557]
#  collut[1:10] <- colors()[646:655]
#  collut[11:20] <-colors()[589:598]
#  collut[21:100] <-colors()[262:341]
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
#  ColorLevels <- seq(min(z_tmp), max(z_tmp), length=length(ColorRamp))
  ex <- length(ColorRamp)/max(z_tmp)
  temp <- round(ex * z)
  if(scol==TRUE){
    icol<-"#0072B2"
  } else {
    icol<-NULL
  }
  #temp <- 1 + 100*(z-min(z)) / length(ColorRamp)
  #temp <- 1 + 100*(z_tmp-min(z_tmp)) / diff(range(z_tmp))
  mapcol <- ColorRamp[temp]
  mapcol2 <- matrix(mapcol,nrow=nrow(z), byrow=FALSE)
  par(cex.axis=cex.A*.1,cex.lab=cex.L*.1,cex.main=cex.T*.1)

        if(colkey==0){
           ribbon3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,bty=bty,colkey = FALSE, col = icol, inttype = inity,alpha = trans)
        } else {
          ribbon3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,colkey = list(side = 1, length = 0.5, addlines = TRUE, cex.clab = 0.75), bty=bty, col = icol, inttype = inity,alpha = trans)
        }

  # persp(x,y,z, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, col=mapcol2[2:nrow(z),2:ncol(z)], ltheta=-60,border=0.1,nticks=8)
 
}


raw_plot_mesh <- function(xx,yl,xl,zl,title,theta,ltheta,cex.T,cex.L,cex.A,bty,colkey,scol,inity,trans,meshspace) {
  # Produce 3D Checkerboard plot for raw data
  #
  # Args:
  #   xx: data matrix from input
  #   yl: y axis label
  #   xl: x axis label
  #   zl: z axis labe
  #   title: main title
  #   theta: plot angle theta
  #   ltheta: plot angel ltheta
  #   cex.T: size main title
  #   cex.L: size labels
  #   cex.A: size axis ... not relevant here :)
  # Returns:
  #   
  
  collut <- c()
  z <- as.matrix(xx)
#  z <- z-min(z)+1 # added for mammalian cell lines where smallest number is not nessesarily ~ 0
  z <- z/max(z)
  z_tmp <- array(z)
  x<-seq(1:nrow(z))
  y<-seq(1:ncol(z))
#  x<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))
#  y<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))

#  collut <- colors()[542:557]
#  collut[1:10] <- colors()[646:655]
#  collut[11:20] <-colors()[589:598]
#  collut[21:100] <-colors()[262:341]
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
#  ColorLevels <- seq(min(z_tmp), max(z_tmp), length=length(ColorRamp))
  ex <- length(ColorRamp)/max(z_tmp)
  temp <- round(ex * z)
  #temp <- 1 + 100*(z-min(z)) / length(ColorRamp)
  #temp <- 1 + 100*(z_tmp-min(z_tmp)) / diff(range(z_tmp))
  if(scol==TRUE){
    icol<-"#0072B2"
  } else {
    icol<-NULL
  }
  mapcol <- ColorRamp[temp]
  mapcol2 <- matrix(mapcol,nrow=nrow(z), byrow=FALSE)
  par(cex.axis=cex.A*.1,cex.lab=cex.L*.1,cex.main=cex.T*.1)

        if(colkey==0){
           ribbon3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,bty=bty,colkey = FALSE, col = icol, inttype = inity,alpha = trans, along = "xy", space = meshspace)
        } else {
          ribbon3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,colkey = list(side = 1, length = 0.5, addlines = TRUE, cex.clab = 0.75), bty=bty, col = icol, inttype = inity,alpha = trans,along = "xy", space = meshspace)
        }

  # persp(x,y,z, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, col=mapcol2[2:nrow(z),2:ncol(z)], ltheta=-60,border=0.1,nticks=8)
 
}

raw_plot_2dc <- function(xx,yl,xl,zl,title,theta,ltheta,cex.T,cex.L,cex.A,bty,colkey,scol,inity,trans,meshspace) {
  # Produce 3D Checkerboard plot for raw data
  #
  # Args:
  #   xx: data matrix from input
  #   yl: y axis label
  #   xl: x axis label
  #   zl: z axis labe
  #   title: main title
  #   theta: plot angle theta
  #   ltheta: plot angel ltheta
  #   cex.T: size main title
  #   cex.L: size labels
  #   cex.A: size axis ... not relevant here :)
  # Returns:
  #   
  
  collut <- c()
  z <- as.matrix(xx)
#  z <- z-min(z)+1 # added for mammalian cell lines where smallest number is not nessesarily ~ 0
  z <- z/max(z)
  z_tmp <- array(z)
  x<-seq(1:nrow(z))
  y<-seq(1:ncol(z))
#  x<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))
#  y<-sort(c(50,1.58,0.5,0.158,0.05,0.00158,0.0005,0.0000158))

#  collut <- colors()[542:557]
#  collut[1:10] <- colors()[646:655]
#  collut[11:20] <-colors()[589:598]
#  collut[21:100] <-colors()[262:341]
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
#  ColorLevels <- seq(min(z_tmp), max(z_tmp), length=length(ColorRamp))
  ex <- length(ColorRamp)/max(z_tmp)
  temp <- round(ex * z)
  #temp <- 1 + 100*(z-min(z)) / length(ColorRamp)
  #temp <- 1 + 100*(z_tmp-min(z_tmp)) / diff(range(z_tmp))
  if(scol==TRUE){
    icol<-"#0072B2"
  } else {
    icol<-NULL
  }
  mapcol <- ColorRamp[temp]
  mapcol2 <- matrix(mapcol,nrow=nrow(z), byrow=FALSE)
  par(cex.axis=cex.A*.1,cex.lab=cex.L*.1,cex.main=cex.T*.1)

  io<-data.frame(xx)
  persp(x, y, z, phi = 45, theta = 45,
  xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
  main = "Surface elevation data"
  )
        # if(colkey==0){
        #    ribbon3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,bty=bty,colkey = FALSE, col = icol, inttype = inity,alpha = trans, along = "xy", space = meshspace)
        # } else {
        #   ribbon3D(z = z, x = x, y = y, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, ltheta=-60,border=0.1,nticks=8,contour = FALSE ,colkey = list(side = 1, length = 0.5, addlines = TRUE, cex.clab = 0.75), bty=bty, col = icol, inttype = inity,alpha = trans,along = "xy", space = meshspace)
        # }

  # persp(x,y,z, xlab=xl,ylab=yl,zlab=zl,main=title,zlim=c(0,1),ticktype="simple", theta=theta, phi=ltheta, expand=0.5, shade=0.2, col=mapcol2[2:nrow(z),2:ncol(z)], ltheta=-60,border=0.1,nticks=8)
 
}


bliss_calculus <- function(xx) {
  # Bliss calculus according to 'Multicomponent therapeutics for networked 
  # systems' Keith C. Borisy A.
  #
  # Args:
  #   xx: data matrix
  # Returns:
  #   z-bm: normalised bliss values between -1 and 1 
  
   z <- as.matrix(xx)
   z <- z/(max(z)) # was max(z) before before now z[1,1]
   z <- 1 - z
   bm <- as.matrix(xx)

   a <- z[1,]
   b <- z[,1]

   for (i in 1:length(a)) {
      for (j in 1:length(b)) {
         bm[j,i] <-  a[i] + b[j] - ( a[i]*b[j] )
      }
   }

   return(z-bm)
}

hsa_calculus <- function(xx) {
  # HSA calculus according to 'Multicomponent therapeutics for networked 
  # systems' Keith C. Borisy A.
  #
  # Args:
  #   xx: data matrix
  # Returns:
  #   z-bm: normalised HSA values between -1 and 1  
  
   z <- as.matrix(xx)
   z <- z/max(z)
   z <- 1 - z
   bm <- as.matrix(xx)

   a <- z[1,]
   b <- z[,1]

   for (i in 1:length(a)) {
      for (j in 1:length(b)) {
         bm[j,i] <-  max(a[i],b[j])
      }
   }

   return(z-bm)
}

find_xy <- function(m) {
  # find coordinate to maximum synergy/antagonism value
  #
  # Args:
  #   m: data matrix
  # Returns:
  #   track[topnr, ]: normalised HSA values between -1 and 1  
  
    max <- 0
    x <- 0
    y <- 0
    track <- data.frame(x,y,max)
    m[is.na(m)] <- 0
    for (i in 1:ncol(m)) {
        #if(sign(round(as.numeric(max(m[,i],rm.na=TRUE)),5) - round(as.numeric(p),5))) {
            y <- which.max(m[ ,i])
            x <- i
            p <- round(as.numeric(max(m[ ,i],na.rm=TRUE)),5)
        track[i, ] <- c(x,y,p)    
        #}
        cat(paste("x:",x,"y:",y,"val:",p,"\n"))
    }
    topnr <- which.max(track$max)
    cat(paste("Return - x:",track$x[topnr],"y:",track$y[topnr],"val:",track$max[topnr],"\n"))
    return(track[topnr, ])
}


lowess_plot <- function(xx) {
  # Visual exam if I can improve bumpy surfaces
  #
  # Args:
  #   xx: data matrix
  # Returns:
  #   
  
  z <- as.matrix(xx)
  z <- z/max(z)
  
  inc <- 2
  
  nz <- matrix(data=0, nrow=nrow(z)*inc, ncol=ncol(z)*inc)
  
  for (i in 1:8) {
    for(j in 1:8) {
      nz[(inc*(i-1))+1:inc,(inc*(j-1))+1:inc] <- rep(z[i,j],inc*inc)
    }
  }
   
  myspline <- matrix(data=0, nrow=nrow(z)*inc, ncol=ncol(z)*inc)  
  for (i in 1:(nrow(z)*inc)) {
    tmp <- lowess(nz[i,],f=1/2)
    myspline[i,] <- tmp$y
    
  }
    
  myspline2 <- matrix(data=0, nrow=nrow(z)*inc, ncol=ncol(z)*inc)  
  for (i in 1:(ncol(z)*inc)) {
    tmp <- lowess(nz[,i],f=1/2)
    myspline2[,i] <- tmp$y
  }
  
  x<-seq(1, (nrow(z)*inc))
  y<-seq(1, (ncol(z)*inc))
  
  mysplavg <- myspline + myspline2 %/% 2
  #mysplavg <-mysplavg-min(mysplavg)
  #rownames(mysplavg) <-c('A','B','C','D','E','F','G','H')
  persp3D(z = z, x = x, y = mysplavg)
  # persp(x*inc,y*inc,mysplavg,main="Response Curve",xlab="DrugA",ylab="DrugB", zlab="OD",ticktype="detailed", theta=140, phi=40, expand=0.5, shade=0.5, col="cyan", ltheta=-60)
  
}


create_inverse <-function(xx) {
  # Inverse 'dose response direction' for correct visualisation and calculation
  #
  # Args:
  #   xx: data matrix
  # Returns:
  #   yy: inversed matrix
  
  r <- nrow(xx)
  c <- ncol(xx)
  yy <- xx
  
  for (i in 1: r) {
    for (j in 1: c) {
      yy[(r-i+1),(c-j+1)] <- xx[i,j] 
    }
  }
  return(yy)
}

create_flip <-function(xx) {
  # Flip 'dose response direction' for correct visualisation and calculation
  #
  # Args:
  #   xx: data matrix
  # Returns:
  #   yy: flipped matrix 
  
  r <- nrow(xx)
  c <- ncol(xx)
  yy <- xx
  
  for (i in 1: r) {
    yy[(r-i+1),] <- xx[i,] 
  }
  return(yy)
}

create_x_flip <-function(xx) {
  # Flip X  'dose response direction' for correct visualisation and calculation
  #
  # Args:
  #   xx: data matrix
  # Returns:
  #   yy: x-flipped matrix 
  
  r <- nrow(xx)
  c <- ncol(xx)
  yy <- xx
  
  for (i in 1: r) {
    yy[(r-i+1),] <- xx[i,] 
  }
  return(yy)
}

create_y_flip <-function(xx) {
  # Flip Y 'dose response direction' for correct visualisation and calculation
  #
  # Args:
  #   xx: data matrix
  # Returns:
  #   yy: y-flipped matrix 
  
  r <- nrow(xx)
  c <- ncol(xx)
  yy <- xx
  
  for (i in 1: c) {
    yy[,(c-i+1)] <- xx[,i] 
  }
  return(yy)
}

create_transpose <-function(xx) {
  # Transpose 'dose response direction' for correct visualisation and calculation
  #
  # Args:
  #   xx: data matrix
  # Returns:
  #   yy: transposed matrix 
  
  r <- nrow(t(xx))
  c <- ncol(t(xx))
  yy <- t(xx)
  
  return(yy)
}

# last line
